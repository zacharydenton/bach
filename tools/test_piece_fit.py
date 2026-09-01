"""Tests for the per-piece fitting rig.

    python3 -m unittest tools.test_piece_fit
"""
import os
import sys
import unittest
from unittest import mock

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import piece_fit as pf  # noqa: E402


class Shrink(unittest.TestCase):
    def test_pulls_toward_global(self):
        # n=5, k=2: keep 5/7 of the deviation
        out = pf.shrink({"cadence_depth": 0.07}, {"cadence_depth": 0.0},
                        5, 2)
        self.assertAlmostEqual(out["cadence_depth"], 0.05, places=4)

    def test_n1_is_strongly_shrunk(self):
        out = pf.shrink({"open_push": 0.12}, {"open_push": 0.06}, 1, 2)
        self.assertAlmostEqual(out["open_push"], 0.08, places=4)


class Sections(unittest.TestCase):
    STATE = {
        "wtc1p03": {
            "piece": "wtc1p03", "n": 4, "performers": ["a", "b", "c", "d"],
            "tempo": {"human_median": 80.0, "authority": 100.0,
                      "fitted": 84.0},
            "timing": {"raw": {"cadence_depth": 0.06},
                       "shrunk": {"cadence_depth": 0.04},
                       "baseline_r": 0.1, "fitted_r": 0.3,
                       "lopo_delta": 0.05, "kept": True},
            "velocity": {"raw": {"vel_highloud": 1.2},
                         "shrunk": {"vel_highloud": 1.0},
                         "baseline_r": 0.3, "fitted_r": 0.35,
                         "lopo_delta": -0.02, "kept": False},
        },
    }

    def test_kept_fits_only(self):
        secs = pf.build_sections(self.STATE)
        keys = [k for k, _, _ in secs["wtc1p03"]]
        self.assertIn("tempo", keys)
        self.assertIn("cadence_depth", keys)
        self.assertNotIn("vel_highloud", keys)  # LOPO rejected
        for _, _, comment in secs["wtc1p03"]:
            self.assertIn(pf.MARK, comment)


class Apply(unittest.TestCase):
    def run_apply(self, toml, state):
        import tempfile
        with tempfile.NamedTemporaryFile(
                "w", suffix=".toml", delete=False) as f:
            f.write(toml)
        try:
            with mock.patch.object(pf, "CONFIG", f.name):
                pf.apply_fits(state, dry_run=False)
            return open(f.name).read()
        finally:
            os.unlink(f.name)

    def test_hand_keys_survive_and_win(self):
        toml = ("[agogics]\nrit_span = 2.0\n\n"
                "[piece.wtc1p03]\n"
                "cadence_depth = 0.09 # by ear, veto\n")
        out = self.run_apply(toml, Sections.STATE)
        # the hand cadence_depth stands, the fitted one is NOT added
        self.assertIn("cadence_depth = 0.09 # by ear, veto", out)
        self.assertEqual(out.count("cadence_depth"), 1)
        # tempo (no hand key) IS added to the existing section
        self.assertIn("tempo = 84.0 # FITTED", out)

    def test_new_piece_gets_banner_section(self):
        toml = "[agogics]\nrit_span = 2.0\n"
        out = self.run_apply(toml, Sections.STATE)
        self.assertIn("[piece.wtc1p03]", out)
        self.assertIn("fitted per piece", out)
        self.assertIn("cadence_depth = 0.04 # FITTED", out)

    def test_idempotent_regeneration(self):
        toml = "[agogics]\nrit_span = 2.0\n"
        once = self.run_apply(toml, Sections.STATE)
        twice = self.run_apply(once, Sections.STATE)
        self.assertEqual(once.count("tempo = 84.0"), 1)
        self.assertEqual(twice.count("tempo = 84.0"), 1)
        self.assertEqual(twice.count("[piece.wtc1p03]"), 1)


class BaseValues(unittest.TestCase):
    def test_reads_globals_not_piece_sections(self):
        vals = pf.base_values(pf.CONFIG)
        self.assertIn("cadence_depth", vals)
        self.assertEqual(vals["cadence_depth"], 0.0)  # the global veto
        self.assertIn("vel_highloud", vals)


class Prefit(unittest.TestCase):
    def test_strips_fitted_lines_and_fingerprints(self):
        import tempfile
        with tempfile.TemporaryDirectory() as tmp:
            path, sha = pf.prefit_config(tmp)
            text = open(path).read()
            self.assertNotIn(pf.MARK, text)
            self.assertNotIn("fitted per piece", text)
            # hand entries survive the strip
            self.assertIn("[piece.wtc1p01]", text)
            self.assertIn("overhold", text)
            # deterministic fingerprint
            path2, sha2 = pf.prefit_config(tmp)
            self.assertEqual(sha, sha2)


if __name__ == "__main__":
    unittest.main()
