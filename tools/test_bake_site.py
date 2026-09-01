"""Tests for the static-site bake.  python3 -m unittest tools.test_bake_site"""
import json
import os
import sys
import tempfile
import unittest

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import bake_site as bs  # noqa: E402


class AlbumKey(unittest.TestCase):
    def test_book_order_prelude_before_fugue(self):
        paths = ["wtc2f01.json", "wtc1f01.json", "wtc1p02.json",
                 "wtc1p01.json", "xtra-ground-folia.json", "wtc2p01.json"]
        paths.sort(key=bs.album_key)
        self.assertEqual(paths, ["wtc1p01.json", "wtc1f01.json",
                                 "wtc1p02.json", "wtc2p01.json",
                                 "wtc2f01.json", "xtra-ground-folia.json"])


class PatchUrl(unittest.TestCase):
    def test_bank_tail(self):
        self.assertEqual(
            bs.patch_url("/usr/share/surge-xt/patches_factory/Leads/DNA.fxp"),
            "data/patches/Leads/DNA.fxp")

    def test_windows_separators(self):
        self.assertEqual(bs.patch_url("C:\\lib\\Pads\\Warm.fxp"),
                         "data/patches/Pads/Warm.fxp")


class CastingBake(unittest.TestCase):
    def test_casting_and_calibration_rewritten_to_urls(self):
        with tempfile.TemporaryDirectory() as tmp:
            castdir = os.path.join(tmp, "casting")
            os.makedirs(castdir)
            with open(os.path.join(castdir, "default.json"), "w") as f:
                json.dump({"_": "a comment, not a channel",
                           "0": "/mac/Leads/Deep.fxp"}, f)
            with open(os.path.join(castdir, "wtc1f01.json"), "w") as f:
                json.dump({"0": "/other/Pads/Warm.fxp"}, f)
            calfile = os.path.join(tmp, "calibration.json")
            with open(calfile, "w") as f:
                json.dump({"/mac/Leads/Deep.fxp": {"releaseS": 0.4}}, f)
            data = os.path.join(tmp, "data")
            os.makedirs(data)
            bs.bake_casting(data, castdir, calfile)
            with open(os.path.join(data, "casting.json")) as f:
                cast = json.load(f)
            self.assertEqual(cast["default"],
                             {"0": "data/patches/Leads/Deep.fxp"})
            self.assertEqual(cast["wtc1f01"],
                             {"0": "data/patches/Pads/Warm.fxp"})
            with open(os.path.join(data, "calibration.json")) as f:
                cal = json.load(f)
            self.assertEqual(cal, {"data/patches/Leads/Deep.fxp":
                                   {"releaseS": 0.4}})


class Manifest(unittest.TestCase):
    def test_manifest_orders_and_measures(self):
        with tempfile.TemporaryDirectory() as tmp:
            perf = os.path.join(tmp, "perf")
            os.makedirs(perf)
            # endS extends past the last note-off; maxCh from the notes
            a = {"piece": "wtc1p01", "endS": 9.0, "tracks": [[
                {"onS": 0.0, "durS": 1.0, "ch": 0},
                {"onS": 1.0, "durS": 1.0, "ch": 2}]]}
            b = {"piece": "wtc1f01", "tracks": [[
                {"onS": 0.0, "durS": 4.0, "ch": 0}]]}
            with open(os.path.join(perf, "wtc1p01.json"), "w") as f:
                json.dump(a, f)
            with open(os.path.join(perf, "wtc1f01.json"), "w") as f:
                json.dump(b, f)
            m = bs.bake_manifest(tmp)
            self.assertEqual([p["name"] for p in m["pieces"]],
                             ["wtc1p01", "wtc1f01"])
            self.assertEqual(m["pieces"][0]["endS"], 9.0)
            self.assertEqual(m["pieces"][0]["maxCh"], 3)
            self.assertEqual(m["pieces"][1]["endS"], 4.0)
            # piece a: chans 0,2 -> slots bass+soprano -> 2 instances;
            # piece b: one chan -> the top slot -> 1
            self.assertEqual(m["nInstances"], 2)
            self.assertEqual(m["scl"], "data/w3.scl")

    def test_piece_instances_mirrors_js_rounding(self):
        # seven ranked channels: ranks 1 and 5 sit exactly on .5 — JS
        # Math.round goes UP; per-slot counts must be [1,2,2,2] -> 4
        perf = {"tracks": [[{"ch": c, "pitch": 40 + c, "onS": 0.0,
                             "durS": 1.0} for c in range(7)]]}
        self.assertEqual(bs.piece_instances(perf), 4)


@unittest.skipUnless(
    os.path.isdir(os.path.join(os.path.dirname(HERE), "site", "data")),
    "site not baked")
class BakedSite(unittest.TestCase):
    """The baked output, if present, honours the contracts the client
    (site/app.js, site/routing.js) relies on."""

    DATA = os.path.join(os.path.dirname(HERE), "site", "data")

    def test_manifest_urls_resolve(self):
        with open(os.path.join(self.DATA, "manifest.json")) as f:
            m = json.load(f)
        self.assertGreater(len(m["pieces"]), 0)
        site = os.path.dirname(self.DATA)
        for p in m["pieces"][:3] + m["pieces"][-3:]:
            self.assertTrue(os.path.isfile(os.path.join(site, p["url"])))
        self.assertTrue(os.path.isfile(os.path.join(site, m["scl"])))
        # "(init)" loads real bytes now (scene merges need both patches)
        self.assertTrue(os.path.isfile(
            os.path.join(self.DATA, "init.fxp")))

    def test_casting_urls_exist_in_baked_bank(self):
        site = os.path.dirname(self.DATA)
        with open(os.path.join(self.DATA, "casting.json")) as f:
            castings = json.load(f)
        for cast in castings.values():
            for url in cast.values():
                self.assertTrue(os.path.isfile(os.path.join(site, url)),
                                url)


if __name__ == "__main__":
    unittest.main()
