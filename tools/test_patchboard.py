#!/usr/bin/env python3
"""Tests for the patchboard's HTTP boundary — runnable without surgepy.

    python3 -m unittest tools/test_patchboard.py

License: GPL-2.0-or-later.
"""

import http.client
import json
import os
import queue
import sys
import tempfile
import threading
import time
import types
import unittest
from unittest import mock

try:
    import numpy
except ImportError:  # the HTTP boundary tests still run without it
    numpy = None

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import patchboard  # noqa: E402


class FakeEngine:
    def __init__(self):
        self.sr = 48000
        self.parts = [{"channels": [0, 1]}, {"channels": [2]}]
        self.playlist = [("a", {}), ("b", {})]
        self.ch_mute = {0: False, 1: False, 2: False}
        self.ch_gain = {0: 1.0, 1: 1.0, 2: 1.0}
        self.playing = False
        self.jump = None
        self.loaded = []
        self.subscribers = set()

    def request_patch(self, pi, path):
        self.loaded.append((pi, path))

    def state(self):
        return {"playing": self.playing}

    def subscribe(self, prefill=False, token=None):
        listener = queue.Queue()
        self.subscribers.add(listener)
        return listener

    def unsubscribe(self, listener):
        self.subscribers.discard(listener)


class StreamingEngine(FakeEngine):
    def __init__(self):
        super().__init__()
        self.lock = threading.Lock()
        self.running = True
        self.silence = b"\0" * (patchboard.CHUNK_FRAMES * 2 * 4)
        self.producer = threading.Thread(target=self.produce)
        self.producer.start()

    def subscribe(self, prefill=False, token=None):
        listener = queue.Queue(maxsize=256)
        with self.lock:
            self.subscribers.add(listener)
        if prefill:
            for _ in range(60):
                listener.put_nowait(self.silence)
        return listener

    def unsubscribe(self, listener):
        with self.lock:
            self.subscribers.discard(listener)

    def produce(self):
        while self.running:
            with self.lock:
                listeners = list(self.subscribers)
            for listener in listeners:
                try:
                    listener.put_nowait(self.silence)
                except queue.Full:
                    pass
            time.sleep(patchboard.CHUNK_FRAMES / self.sr)

    def close(self):
        self.running = False
        self.producer.join()


CATS = {"Leads": [("Good", "/lib/Leads/Good.fxp")]}


class Boundary(unittest.TestCase):
    # a fresh engine and server per test: no assertion may depend on
    # what an alphabetically earlier test happened to POST
    def setUp(self):
        self.engine = FakeEngine()
        self.srv = patchboard.serve(
            self.engine, CATS, patchboard.DEFAULT_HOST, 0)
        self.port = self.srv.server_address[1]

    def tearDown(self):
        self.srv.shutdown()
        self.srv.server_close()

    def post(self, path, body, headers=None, raw=None):
        c = http.client.HTTPConnection("127.0.0.1", self.port, timeout=5)
        data = raw if raw is not None else json.dumps(body).encode()
        c.request("POST", path, body=data, headers=headers or {})
        r = c.getresponse()
        out = (r.status, json.loads(r.read() or b"{}"))
        c.close()
        return out

    def get(self, path, headers=None):
        c = http.client.HTTPConnection("127.0.0.1", self.port, timeout=5)
        c.request("GET", path, headers=headers or {})
        r = c.getresponse()
        out = (r.status, json.loads(r.read() or b"{}"))
        c.close()
        return out

    def test_binds_loopback_by_default(self):
        # the module constant is what argparse hands --host as its
        # default; binding it must land on loopback
        self.assertEqual(patchboard.DEFAULT_HOST, "127.0.0.1")
        self.assertEqual(self.srv.server_address[0], patchboard.DEFAULT_HOST)

    def test_patch_must_be_in_library(self):
        st, _ = self.post("/patch", {"part": 0, "path": "/etc/passwd"})
        self.assertEqual(st, 400)
        st, _ = self.post("/patch", {"part": 0, "path": "/lib/Leads/Good.fxp"})
        self.assertEqual(st, 200)
        st, _ = self.post("/patch", {"part": 1, "path": "(init)"})
        self.assertEqual(st, 200)
        self.assertEqual(self.engine.loaded,
                         [(0, "/lib/Leads/Good.fxp"), (1, "(init)")])

    def test_indices_validated(self):
        self.assertEqual(self.post("/patch", {"part": 7, "path": "(init)"})[0], 400)
        self.assertEqual(self.post("/mute", {"part": -1})[0], 400)
        self.assertEqual(self.post("/mute", {"part": "0"})[0], 400)
        self.assertEqual(self.post("/piece", {"index": 2})[0], 400)
        self.assertEqual(self.post("/piece", {"index": 1})[0], 200)
        self.assertEqual(self.engine.jump, 1)

    def test_gain_bounded(self):
        self.assertEqual(self.post("/gain", {"part": 0, "gain": 99})[0], 400)
        self.assertEqual(self.post("/gain", {"part": 0, "gain": "1"})[0], 400)
        self.assertEqual(self.post("/gain", {"part": 0, "gain": 0.5})[0], 200)
        self.assertEqual(self.engine.ch_gain[0], 0.5)
        self.assertEqual(self.engine.ch_gain[1], 0.5)

    def test_body_limits(self):
        big = b"{" + b" " * (patchboard.MAX_BODY + 1) + b"}"
        self.assertEqual(self.post("/toggle", None, raw=big)[0], 400)
        self.assertEqual(self.post("/toggle", None, raw=b"[1,2]")[0], 400)
        self.assertEqual(self.post("/toggle", None, raw=b"not json")[0], 400)

    def test_cross_origin_refused(self):
        h = {"Origin": "http://evil.example"}
        st, _ = self.post("/toggle", {}, headers=h)
        self.assertEqual(st, 403)
        h = {"Origin": f"http://127.0.0.1:{self.port}"}
        st, _ = self.post("/toggle", {}, headers=h)
        self.assertEqual(st, 200)

    def test_cross_origin_stream_refused(self):
        for path in ("/opus", "/pcm"):
            self.assertEqual(
                self.get(path, {"Origin": "http://evil.example"})[0], 403)
            self.assertEqual(
                self.get(path, {"Sec-Fetch-Site": "cross-site"})[0], 403)

    def test_pcm_listener_limit_is_a_clean_503(self):
        with mock.patch.object(patchboard, "MAX_PCM_CLIENTS", 0):
            srv = patchboard.serve(
                FakeEngine(), CATS, patchboard.DEFAULT_HOST, 0)
        try:
            c = http.client.HTTPConnection(
                "127.0.0.1", srv.server_address[1], timeout=5)
            c.request("GET", "/pcm")
            self.assertEqual(c.getresponse().status, 503)
            c.close()
        finally:
            srv.shutdown()
            srv.server_close()

    def test_unknown_post_404(self):
        self.assertEqual(self.post("/nope", {})[0], 404)


class OpusBoundary(unittest.TestCase):
    def get(self, server, path):
        port = server.server_address[1]
        c = http.client.HTTPConnection("127.0.0.1", port, timeout=5)
        c.request("GET", path)
        r = c.getresponse()
        out = (r.status, json.loads(r.read() or b"{}"))
        c.close()
        return out

    def test_missing_ffmpeg_is_clean_503(self):
        engine = FakeEngine()
        with mock.patch.object(patchboard.shutil, "which", return_value=None):
            server = patchboard.serve(engine, CATS, "127.0.0.1", 0)
        try:
            self.assertEqual(self.get(server, "/opus")[0], 503)
            self.assertEqual(engine.subscribers, set())
        finally:
            server.shutdown()
            server.server_close()

    def test_ffmpeg_start_failure_does_not_subscribe(self):
        engine = FakeEngine()
        with mock.patch.object(
                patchboard.shutil, "which", return_value="/fake/ffmpeg"):
            server = patchboard.serve(engine, CATS, "127.0.0.1", 0)
        try:
            with mock.patch.object(
                    patchboard.subprocess, "Popen", side_effect=OSError):
                self.assertEqual(self.get(server, "/opus")[0], 503)
            self.assertEqual(engine.subscribers, set())
        finally:
            server.shutdown()
            server.server_close()

    def test_listener_limit_rejected_before_spawn(self):
        engine = FakeEngine()
        with mock.patch.object(patchboard, "MAX_OPUS_CLIENTS", 0), \
             mock.patch.object(
                 patchboard.shutil, "which", return_value="/fake/ffmpeg"):
            server = patchboard.serve(engine, CATS, "127.0.0.1", 0)
        try:
            with mock.patch.object(patchboard.subprocess, "Popen") as popen:
                self.assertEqual(self.get(server, "/opus")[0], 503)
                popen.assert_not_called()
            self.assertEqual(engine.subscribers, set())
        finally:
            server.shutdown()
            server.server_close()

    @unittest.skipUnless(patchboard.shutil.which("ffmpeg"),
                         "ffmpeg is not installed")
    def test_disconnect_reaps_and_releases_listener_slot(self):
        engine = StreamingEngine()
        processes = []
        original_popen = patchboard.subprocess.Popen

        def record_process(*args, **kwargs):
            process = original_popen(*args, **kwargs)
            processes.append(process)
            return process

        with mock.patch.object(patchboard, "MAX_OPUS_CLIENTS", 1), \
             mock.patch.object(
                 patchboard.subprocess, "Popen", side_effect=record_process):
            server = patchboard.serve(engine, CATS, "127.0.0.1", 0)
            try:
                for _ in range(2):
                    port = server.server_address[1]
                    connection = http.client.HTTPConnection(
                        "127.0.0.1", port, timeout=10)
                    connection.request("GET", "/opus")
                    response = connection.getresponse()
                    self.assertEqual(response.status, 200)
                    self.assertEqual(len(response.read(512)), 512)
                    response.close()
                    connection.close()
                    for _ in range(80):
                        feeders = [thread for thread in threading.enumerate()
                                   if thread.name == "opus-feed"]
                        with engine.lock:
                            subscriber_count = len(engine.subscribers)
                        if subscriber_count == 0 and not feeders:
                            break
                        time.sleep(0.05)
                    self.assertEqual(subscriber_count, 0)
                    self.assertEqual(feeders, [])
                    # the feeder can exit on its own stop-flag timeout while
                    # the handler is still inside proc.wait() — reaping is
                    # guaranteed but not ordered before the signals above,
                    # so poll for it rather than racing it
                    for _ in range(60):
                        if processes[-1].poll() is not None:
                            break
                        time.sleep(0.05)
                    self.assertIsNotNone(processes[-1].poll())
            finally:
                server.shutdown()
                server.server_close()
                engine.close()


class FakeSurge:
    """Stands in for a surgepy instance: logs every call and renders
    1.0 while any note is held, so block-quantised dispatch is visible
    in the audio itself."""

    def __init__(self, sr):
        self.sr = sr
        self.calls = []
        self.held = 0

    def loadSCLFile(self, path):
        self.calls.append(("scl", path))

    def loadPatch(self, path):
        self.calls.append(("patch", path))

    def createMultiBlock(self, nblocks):
        return numpy.zeros((2, nblocks * patchboard.BLOCK),
                           dtype=numpy.float32)

    def processMultiBlock(self, buf, start, n):
        self.calls.append(("process", start, n))
        b = patchboard.BLOCK
        buf[:, start * b:(start + n) * b] = 1.0 if self.held else 0.0

    def playNote(self, ch, pitch, vel, detune):
        self.calls.append(("on", pitch, vel))
        self.held += 1

    def releaseNote(self, ch, pitch, detune):
        self.calls.append(("off", pitch))
        self.held = max(0, self.held - 1)

    def pitchBend(self, ch, bend):
        self.calls.append(("bend", bend))

    def allNotesOff(self):
        self.calls.append(("allOff",))
        self.held = 0


def note(ch, on_s, dur_s, pitch=60, vel=100, whys=None):
    n = {"ch": ch, "onS": on_s, "durS": dur_s, "pitch": pitch,
         "vel": vel, "bend": 8192}
    if whys:
        n["whys"] = whys
    return n


@unittest.skipIf(numpy is None, "Engine tests need numpy")
class EngineTests(unittest.TestCase):
    """Engine internals with surgepy stubbed by FakeSurge (the real
    module is unimportable here; patchboard's own import guard nulls
    both np and surgepy, so both are patched in)."""

    SR = 48000
    B = None  # patchboard.BLOCK, bound in setUp for brevity

    def setUp(self):
        self.B = patchboard.BLOCK
        for name, val in (
                ("np", numpy),
                ("surgepy", types.SimpleNamespace(createSurge=FakeSurge))):
            patcher = mock.patch.object(patchboard, name, val)
            patcher.start()
            self.addCleanup(patcher.stop)

    def make(self, tracks=None, playlist=None, **kw):
        if playlist is None:
            tracks = tracks or [[note(0, 100 * self.B / self.SR,
                                      20 * self.B / self.SR)]]
            playlist = [("t", {"tracks": tracks})]
        return patchboard.Engine(playlist, self.SR, None, **kw)

    def test_render_splits_spans_at_event_blocks(self):
        # one note: on at block 100, off at block 120, inside one chunk
        e = self.make()
        out = e.render(128 * self.B)
        s = e.instances[0]
        # processMultiBlock spans split exactly at the event blocks,
        # events dispatched between them (bend precedes on: no scl)
        self.assertEqual(
            [c for c in s.calls if c[0] != "allOff"],
            [("process", 0, 100), ("bend", 0), ("on", 60, 100),
             ("process", 100, 20), ("off", 60), ("process", 120, 8)])
        # and the audio says the same: held only for blocks 100..119
        on, off = 100 * self.B, 120 * self.B
        self.assertEqual(float(out[0, on - 1]), 0.0)
        self.assertEqual(float(out[0, on]), 0.25)  # default ch gain
        self.assertEqual(float(out[0, off - 1]), 0.25)
        self.assertEqual(float(out[0, off]), 0.0)

    def test_events_land_in_their_chunk(self):
        # note-on at block 100 belongs to the second 64-block chunk
        e = self.make()
        e.render(64 * self.B)
        s = e.instances[0]
        self.assertNotIn(("on", 60, 100), s.calls)
        self.assertEqual(s.calls[-1], ("process", 0, 64))
        e.render(64 * self.B)
        self.assertIn(("on", 60, 100), s.calls)

    def test_loop_wraps_to_next_piece(self):
        e = self.make(playlist=[
            ("a", {"tracks": [[note(0, 0.0, 0.1)]]}),
            ("b", {"tracks": [[note(0, 0.0, 0.1)]]})])
        e.sample = e.loop_len  # at the end, all events consumed
        e.pos[0] = len(e.events[0])
        e.render(self.B)
        self.assertEqual(e.piece_i, 1)
        self.assertEqual(e.piece_name, "b")
        self.assertEqual(e.sample, self.B)
        # the wrap re-armed the events: piece b's on fired at block 0
        self.assertEqual(e.instances[0].calls[-2], ("on", 60, 100))

    def test_paused_still_applies_patches_and_jumps(self):
        e = self.make(playlist=[
            ("a", {"tracks": [[note(0, 0.0, 0.1)]]}),
            ("b", {"tracks": [[note(0, 0.0, 0.1)]]})])
        e.playing = False
        e.request_patch(0, "/lib/Leads/Good.fxp")
        e.jump = 1
        out = e.render(self.B)
        self.assertTrue(numpy.all(out == 0))  # paused stays silent
        self.assertIsNone(e.jump)
        self.assertEqual(e.piece_i, 1)  # /piece landed
        self.assertEqual(e.ch_patch[0], "Good")  # /patch landed

    def test_resolve_casting(self):
        with tempfile.TemporaryDirectory() as tmp:
            deep = os.path.join(tmp, "lib", "Leads", "Deep.fxp")
            warm = os.path.join(tmp, "lib", "Pads", "Warm.fxp")
            os.makedirs(os.path.dirname(deep))
            os.makedirs(os.path.dirname(warm))
            for p in (deep, warm):
                open(p, "w").close()
            castdir = os.path.join(tmp, "casting")
            os.makedirs(castdir)
            with open(os.path.join(castdir, "default.json"), "w") as f:
                json.dump({"0": deep}, f)
            with open(os.path.join(castdir, "t.json"), "w") as f:
                json.dump({"0": warm}, f)
            # one part on two channels: the piece file names only the
            # part's first channel yet must cast the whole part
            e = self.make(tracks=[[note(0, 0.0, 0.1), note(1, 0.0, 0.1)]],
                          casting_dir=castdir)
            e.render(self.B)  # applies the queued casting loads
            self.assertEqual(e.ch_patch_path[0], warm)
            self.assertEqual(e.ch_patch_path[1], warm)
            self.assertEqual(e.ch_patch[0], "Warm")
            # default.json alone (no piece file) seeds the first load
            e2 = self.make(playlist=[
                ("other", {"tracks": [[note(0, 0.0, 0.1)]]})],
                casting_dir=castdir)
            e2.render(self.B)
            self.assertEqual(e2.ch_patch_path[0], deep)

    def test_calibration_compensates_note_ends(self):
        with tempfile.TemporaryDirectory() as tmp:
            fxp = os.path.join(tmp, "lib", "Leads", "Good.fxp")
            os.makedirs(os.path.dirname(fxp))
            open(fxp, "w").close()
            castdir = os.path.join(tmp, "casting")
            os.makedirs(castdir)
            with open(os.path.join(castdir, "default.json"), "w") as f:
                json.dump({"0": fxp}, f)

            def off_sample(cal):
                e = self.make(tracks=[[note(0, 0.0, 1.0)]],
                              casting_dir=castdir, calibration=cal)
                return [ev[0] for ev in e.events[0] if ev[1] == 2][0]

            # keyed by another machine's path: the Category/Name.fxp
            # tail must still match, shaving half the release (0.2 s)
            cal = {"/mac/paths/Leads/Good.fxp": {"attackS": 0.01,
                                                 "releaseS": 0.4}}
            self.assertEqual(off_sample(cal), int(0.8 * self.SR))
            # a hand-edited entry without releaseS is a no-op, not a crash
            self.assertEqual(
                off_sample({"/mac/paths/Leads/Good.fxp": {"attackS": 0.01}}),
                self.SR)
            self.assertEqual(off_sample(None), self.SR)

    def test_limiter_instant_attack_never_clips(self):
        e = self.make()
        quiet = numpy.zeros((2, 256), dtype=numpy.float32)
        e._limit(quiet)  # applied gain settles at unity
        # loud transient right after the quiet chunk: the whole chunk
        # must come out at the new gain, not ramp down from unity
        loud = numpy.full((2, 256), 2.0, dtype=numpy.float32)
        out = e._limit(loud)
        self.assertLessEqual(float(numpy.max(numpy.abs(out))), 0.89 + 1e-5)

    def test_limiter_release_ramps_from_previous_gain(self):
        e = self.make()
        e._limit(numpy.full((2, 256), 2.0, dtype=numpy.float32))
        out = e._limit(numpy.full((2, 256), 0.5, dtype=numpy.float32))
        # no gain step at the chunk seam: the release starts where the
        # attack left off (0.89/2.0) and rises across the chunk
        self.assertAlmostEqual(float(out[0, 0]), 0.5 * 0.445, places=3)
        self.assertGreater(float(out[0, -1]), float(out[0, 0]))
        self.assertLessEqual(float(numpy.max(numpy.abs(out))), 0.89 + 1e-5)

    def test_whys_at(self):
        e = self.make(tracks=[[note(0, 0.0, 1.0, whys=["art:detache Q11"]),
                               note(0, 2.0, 1.0, whys=["orn:trill C8"])]])
        self.assertEqual(e.whys_at(0.5),
                         [{"ch": 0, "why": "art:detache Q11"}])
        self.assertEqual(e.whys_at(2.5), [{"ch": 0, "why": "orn:trill C8"}])
        self.assertEqual(e.whys_at(1.5), [])


if __name__ == "__main__":
    unittest.main()
