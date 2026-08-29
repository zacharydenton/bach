#!/usr/bin/env python3
"""Tests for the patchboard's HTTP boundary — runnable without surgepy.

    python3 -m unittest tools/test_patchboard.py

License: GPL-2.0-or-later.
"""

import http.client
import json
import os
import sys
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import patchboard  # noqa: E402


class FakeEngine:
    def __init__(self):
        self.parts = [{"channels": [0, 1]}, {"channels": [2]}]
        self.playlist = [("a", {}), ("b", {})]
        self.ch_mute = {0: False, 1: False, 2: False}
        self.ch_gain = {0: 1.0, 1: 1.0, 2: 1.0}
        self.playing = False
        self.jump = None
        self.loaded = []

    def request_patch(self, pi, path):
        self.loaded.append((pi, path))

    def state(self):
        return {"playing": self.playing}


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

    def test_unknown_post_404(self):
        self.assertEqual(self.post("/nope", {})[0], 404)


if __name__ == "__main__":
    unittest.main()
