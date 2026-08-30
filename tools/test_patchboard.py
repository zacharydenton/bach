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
import threading
import time
import unittest
from unittest import mock

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
    @classmethod
    def setUpClass(cls):
        cls.engine = FakeEngine()
        cls.srv = patchboard.serve(cls.engine, CATS, "127.0.0.1", 0)
        cls.port = cls.srv.server_address[1]

    @classmethod
    def tearDownClass(cls):
        cls.srv.shutdown()
        cls.srv.server_close()

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
        self.assertEqual(self.srv.server_address[0], "127.0.0.1")

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
        self.assertEqual(
            self.get("/opus", {"Origin": "http://evil.example"})[0], 403)
        self.assertEqual(
            self.get("/opus", {"Sec-Fetch-Site": "cross-site"})[0], 403)

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


if __name__ == "__main__":
    unittest.main()
