"""Tests for the ASAP note bridge. Pure tests always run; corpus-gated
tests skip (with the clone hint) when corpus/asap or the otb binary is
absent.  python3 -m unittest tools.test_note_align
"""
import os
import subprocess
import sys
import tempfile
import unittest

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import note_align as na  # noqa: E402

ROOT = os.path.dirname(HERE)
BWV846F = os.path.join(na.ASAP, "Bach", "Fugue", "bwv_846")

SYNTH_MATCH = """info(midiClockUnits,480).
info(midiClockRate,500000).
scoreprop(timeSignature,3/8,1:1,0,0.0000).
snote(n1-1,[C,#],4,1:1,0,1/16,0.0000,0.5000,[v1,staff1])-note(n0,61,480,720,40,0,0).
snote(n2-1,[B,bb],3,1:1,1/16,1/16,0.5000,1.0000,[v1,staff1])-note(n1,57,720,960,50,0,0).
snote(n3-1,[E,x],4,1:2,0,1/16,1.0000,1.5000,[v1,staff1,trill-mark])-note(n2,66,960,1200,60,0,0).
snote(n4-1,[G,n],4,1:2,1/16,0,1.5000,1.5000,[v1,staff1])-note(n3,67,1200,1250,35,0,0).
snote(n5-1,[A,n],4,1:3,0,1/16,2.0000,2.5000,[v2,staff2])-deletion.
insertion-note(n9,68,1300,1350,20,0,0).
"""


def write_match(tmp, text=SYNTH_MATCH):
    p = os.path.join(tmp, "synth.match")
    with open(p, "w") as f:
        f.write(text)
    return p


class MatchParser(unittest.TestCase):
    def parse(self, text=SYNTH_MATCH):
        with tempfile.TemporaryDirectory() as tmp:
            return na.parse_match(write_match(tmp, text))

    def test_beat_unit_is_meter_denominator(self):
        mp = self.parse()
        # 3/8: onsetBeats 0.5 -> wn 0.5/8 -> key ticks 120
        self.assertEqual(mp.den, 8)
        keys = [r[0] for r in mp.rows]
        self.assertEqual(keys, [0, 120, 240, 360])

    def test_accidentals_and_pitch_check(self):
        mp = self.parse()
        # C#4=61, Bbb3=57, Ex4=66 (double sharp), Gn4=67 — all agree
        self.assertEqual([r[2] for r in mp.rows], [61, 57, 66, 67])
        self.assertEqual(mp.pitch_mismatch, 0)

    def test_pitch_mismatch_counted_not_trusted(self):
        bad = SYNTH_MATCH.replace("note(n0,61,", "note(n0,60,")
        mp = self.parse(bad)
        self.assertEqual(mp.pitch_mismatch, 1)
        self.assertEqual(mp.rows[0][2], 60)  # the note's own field wins

    def test_sentinels_and_grace(self):
        mp = self.parse()
        self.assertEqual(mp.deletions, 1)
        self.assertEqual(mp.insertions, 1)
        self.assertEqual(mp.grace_snotes, 1)  # n4: onset == offset

    def test_tick_to_seconds(self):
        mp = self.parse()
        # 480 ticks at 500000us/480units = 0.5 s
        self.assertAlmostEqual(mp.rows[0][3], 0.5)

    def test_tags_kept(self):
        mp = self.parse()
        self.assertIn("trill-mark", mp.rows[2][7])


def ir_note(src_wn, src_pitch, vel, on_s, ch=0, whys=()):
    return {"srcWn": src_wn, "srcPitch": src_pitch, "vel": vel,
            "onS": on_s, "durS": 0.2, "ch": ch, "pitch": src_pitch,
            "onWn": src_wn, "durWn": 1 / 32, "whys": list(whys)}


class Bridge(unittest.TestCase):
    def mp(self):
        with tempfile.TemporaryDirectory() as tmp:
            return na.parse_match(write_match(tmp))

    def test_exact_join_and_counters(self):
        ir = {"tracks": [[
            ir_note(0.0, 61, 90, 0.0,
                    whys=["articulation: gate 78% (détaché)  [x]"]),
            ir_note(0.5 / 8, 57, 80, 0.1),
            ir_note(1.0 / 8, 66, 70, 0.2),  # trill subnote 1
            ir_note(1.0 / 8, 66, 60, 0.25),  # trill subnote 2 (later)
            ir_note(1.5 / 8, 67, 50, 0.3),
        ]]}
        rows, c = na.bridge(na.load_ir_notes(ir), self.mp())
        self.assertEqual(c["matched"], 4)
        self.assertEqual(c["snote_unmatched"], 0)
        self.assertEqual(c["matched"] + c["snote_unmatched"]
                         + c["snote_deleted"], c["total_snotes"])
        trill = next(r for r in rows if r["pitch"] == 66)
        self.assertTrue(trill["is_orn"])
        self.assertEqual(trill["otb_vel"], 70)  # earliest keystroke wins
        det = next(r for r in rows if r["pitch"] == 61)
        self.assertEqual(det["gate_label"], "détaché")
        self.assertEqual(det["gate_pct"], 78)

    def test_unison_greedy_and_unmatched_counted(self):
        # two voices strike the same (wn, pitch); only one snote exists
        ir = {"tracks": [
            [ir_note(0.0, 61, 90, 0.0, ch=0)],
            [ir_note(0.0, 61, 85, 0.0, ch=1)],
        ]}
        rows, c = na.bridge(na.load_ir_notes(ir), self.mp())
        self.assertEqual(
            len([r for r in rows if r["pitch"] == 61]), 1)
        self.assertEqual(c["snote_unmatched"], 3)  # 57, 66, 67 unbridged

    def test_zscore(self):
        z = na.zscore([1.0, 2.0, 3.0])
        self.assertAlmostEqual(sum(z), 0.0)
        self.assertGreater(z[2], z[0])


def otb_binary():
    try:
        root = subprocess.check_output(
            ["stack", "path", "--local-install-root"], cwd=ROOT,
            stderr=subprocess.DEVNULL, text=True).strip()
        p = os.path.join(root, "bin", "otb")
        return p if os.path.exists(p) else None
    except Exception:
        return None


@unittest.skipUnless(os.path.isdir(BWV846F),
                     "corpus/asap absent — clone nASAP to run")
class CorpusGated(unittest.TestCase):
    def test_real_match_parses_with_invariants(self):
        mp = na.parse_match(os.path.join(BWV846F, "Shi05M.match"))
        self.assertGreater(len(mp.rows), 500)
        self.assertEqual(mp.extra_timesigs, 0)
        self.assertEqual(mp.den, 4)

    @unittest.skipUnless(otb_binary(), "otb binary not built")
    def test_wtc1f01_bridges_over_90pct(self):
        import asap_eval as ae
        kern = os.path.join(ROOT, "corpus", "bach-wtc", "kern")
        with tempfile.TemporaryDirectory() as tmp:
            ir = ae.compile_ir(otb_binary(), "wtc1f01", kern, tmp)
        irn = na.load_ir_notes(ir)
        rows, c = na.bridge(
            irn, na.parse_match(os.path.join(BWV846F, "Shi05M.match")))
        self.assertGreater(c["matched"] / c["total_snotes"], 0.9)
        self.assertEqual(c["offset_wn"], 0)
        self.assertEqual(c["matched"] + c["snote_unmatched"]
                         + c["snote_deleted"], c["total_snotes"])


if __name__ == "__main__":
    unittest.main()
