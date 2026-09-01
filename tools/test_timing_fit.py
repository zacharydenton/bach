"""Tests for the micro-timing rig.  python3 -m unittest tools.test_timing_fit"""
import os
import subprocess
import sys
import tempfile
import unittest

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import note_align as na  # noqa: E402
import timing_fit as tf  # noqa: E402

ROOT = os.path.dirname(HERE)
BWV846F = os.path.join(na.ASAP, "Bach", "Fugue", "bwv_846")


class InterpTime(unittest.TestCase):
    def test_interior(self):
        self.assertAlmostEqual(na.interp_time([0, 1, 2], [0, 2, 6], 1.5), 4)

    def test_edge_slope_extrapolation(self):
        # before the first beat: slope of the FIRST segment, not a clamp
        self.assertAlmostEqual(na.interp_time([1, 2, 3], [2, 4, 8], 0.5), 1)
        # after the last: slope of the LAST segment
        self.assertAlmostEqual(na.interp_time([1, 2, 3], [2, 4, 8], 3.5), 10)

    def test_two_points(self):
        self.assertAlmostEqual(na.interp_time([0, 4], [0, 8], 3), 6)


class Deviations(unittest.TestCase):
    def test_otb_dev_definition(self):
        import asap_eval as ae
        tmap = [(0, 120.0)]  # 2 s per whole note
        # a note whose performed onset is 10 ms after its notated slot
        self.assertAlmostEqual(
            1.01 - ae.our_time_at(tmap, 0.505), 1.01 - 1.01, places=9)


class RuleParsing(unittest.TestCase):
    def make_rep(self, whys):
        ir = {"tracks": [[{
            "srcWn": 0.0, "srcPitch": 60, "vel": 90, "onS": 0.0,
            "durS": 0.2, "ch": 0, "pitch": 60, "onWn": 0.0,
            "durWn": 0.25, "whys": whys}]]}
        return na.load_ir_notes(ir)[(0, 60)][0]

    def test_rule_delta_extraction_strips_cite(self):
        rep = self.make_rep(
            ["double-duration: dur x0.965, onset -0.009 wn  [KTH x]"])
        self.assertEqual(rep["rules"]["double-duration"],
                         "dur x0.965, onset -0.009 wn")

    def test_short_vs_long_half_classification(self):
        short = self.make_rep(["double-duration: dur x1.07, onset -0.009 wn  [k]"])
        long_ = self.make_rep(["double-duration: dur x0.965  [k]"])
        in_short = dict(tf.CONTEXTS)["double-dur short half"]
        in_long = dict(tf.CONTEXTS)["double-dur long half"]
        srow = {"rules": short["rules"]}
        lrow = {"rules": long_["rules"]}
        self.assertTrue(in_short(srow) and not in_long(srow))
        self.assertTrue(in_long(lrow) and not in_short(lrow))

    def test_grace_main_vs_realised(self):
        main = {"rules": {"grace": "dur x0.9, onset +0.016 wn"}}
        realised = {"rules": {"grace": "realised at 0.016 wn"}}
        ctx = dict(tf.CONTEXTS)["grace-delayed main"]
        self.assertTrue(ctx(main))
        self.assertFalse(ctx(realised))

    def test_is_final(self):
        ir = {"tracks": [[
            {"srcWn": 0.0, "srcPitch": 60, "vel": 90, "onS": 0.0,
             "durS": 0.2, "ch": 0, "pitch": 60, "onWn": 0.0,
             "durWn": 0.25, "whys": []},
            {"srcWn": 1.0, "srcPitch": 64, "vel": 90, "onS": 2.0,
             "durS": 0.2, "ch": 0, "pitch": 64, "onWn": 1.0,
             "durWn": 0.25, "whys": []}]]}
        notes = na.load_ir_notes(ir)
        self.assertFalse(notes[(0, 60)][0]["is_final"])
        self.assertTrue(notes[(1920, 64)][0]["is_final"])


class Contrast(unittest.TestCase):
    def test_piece_clustered_contrast(self):
        # 4 pieces x 2 performances; in-context notes uniformly 20 ms
        # late — clustering aggregates to PIECE means (performances of
        # one composition are not independent)
        per_piece = []
        for p in range(4):
            for _ in range(2):
                rows = []
                for i in range(12):
                    rows.append({"rules": {}, "human_dev": 0.0,
                                 "otb_dur_wn": 0.125, "wn": i * 0.25,
                                 "is_final": False})
                for i in range(4):
                    rows.append({"rules": {"x": "onset"},
                                 "human_dev": 0.02,
                                 "otb_dur_wn": 0.125, "wn": i * 0.25,
                                 "is_final": False})
                per_piece.append((f"piece{p}", rows))
        c = tf.contrast(per_piece, lambda r: "x" in r["rules"])
        self.assertAlmostEqual(c["ms"], 20.0)
        self.assertEqual(c["n_pieces"], 4)
        self.assertEqual(c["n_perfs"], 8)


def otb_binary():
    try:
        root = subprocess.check_output(
            ["stack", "path", "--local-install-root"], cwd=ROOT,
            stderr=subprocess.DEVNULL, text=True).strip()
        p = os.path.join(root, "bin", "otb")
        return p if os.path.exists(p) else None
    except Exception:
        return None


@unittest.skipUnless(os.path.isdir(BWV846F) and otb_binary(),
                     "corpus/asap or otb binary absent")
class CorpusGated(unittest.TestCase):
    def test_wtc1f01_deviations_sane(self):
        import statistics
        with tempfile.TemporaryDirectory() as tmp:
            got = list(tf.piece_perf_rows(otb_binary(), "wtc1f01", tmp))
        self.assertTrue(got)
        _perf, rows = got[0]
        self.assertGreater(len(rows), 500)
        med = statistics.median(r["human_dev"] for r in rows)
        self.assertLess(abs(med), 0.05)
        self.assertTrue(any(r["rules"] for r in rows))


if __name__ == "__main__":
    unittest.main()
