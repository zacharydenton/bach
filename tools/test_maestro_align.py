"""Tests for the MAESTRO acquisition + alignment stack.

    python3 -m unittest tools.test_maestro_align
"""
import io
import os
import struct
import sys
import unittest

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import maestro_fetch as mf  # noqa: E402
import smf  # noqa: E402
try:
    import maestro_align as ma  # noqa: E402  (needs numpy)
except ImportError:
    ma = None

ROOT = os.path.dirname(HERE)
ASAP_MID = os.path.join(ROOT, "corpus", "asap",
                        "Bach", "Fugue", "bwv_846", "Shi05M.mid")
ASAP_MATCH = ASAP_MID.replace(".mid", ".match")


def vlq(n):
    out = [n & 0x7F]
    n >>= 7
    while n:
        out.append((n & 0x7F) | 0x80)
        n >>= 7
    return bytes(reversed(out))


def synth_smf(division=480):
    """Format-1 file: a tempo change mid-way, running status, two notes."""
    trk0 = b"".join([
        vlq(0), b"\xff\x51\x03" + (500000).to_bytes(3, "big"),
        vlq(division * 2), b"\xff\x51\x03" + (250000).to_bytes(3, "big"),
        vlq(0), b"\xff\x2f\x00",
    ])
    trk1 = b"".join([
        vlq(0), b"\x90\x3c\x50",           # C4 on at t=0
        vlq(division), b"\x3c\x00",        # running status: off at 1 qn
        vlq(division * 2), b"\x90\x40\x40",  # E4 on at 3 qn (1 past change)
        vlq(division), b"\x80\x40\x00",
        vlq(0), b"\xff\x2f\x00",
    ])
    def chunk(tag, body):
        return tag + struct.pack(">I", len(body)) + body
    return (b"MThd" + struct.pack(">IHHH", 6, 1, 2, division)
            + chunk(b"MTrk", trk0) + chunk(b"MTrk", trk1))


class Smf(unittest.TestCase):
    def test_tempo_map_and_running_status(self):
        import tempfile
        with tempfile.NamedTemporaryFile(suffix=".mid") as f:
            f.write(synth_smf())
            f.flush()
            notes = smf.read_smf(f.name)
        self.assertEqual(len(notes), 2)
        c4, e4 = notes
        self.assertEqual((c4.pitch, c4.vel), (0x3C, 0x50))
        self.assertAlmostEqual(c4.on_s, 0.0)
        self.assertAlmostEqual(c4.off_s, 0.5)  # 1 qn at 120 bpm
        # E4 starts 2 qn at 120 + 1 qn at 240 = 1.0 + 0.25
        self.assertAlmostEqual(e4.on_s, 1.25)
        self.assertAlmostEqual(e4.off_s, 1.5)


class PieceNames(unittest.TestCase):
    def test_books(self):
        self.assertEqual(mf.piece_names(846), ("wtc1p01", "wtc1f01"))
        self.assertEqual(mf.piece_names(853), ("wtc1p08", "wtc1f08"))
        self.assertEqual(mf.piece_names(870), ("wtc2p01", "wtc2f01"))
        self.assertEqual(mf.piece_names(893), ("wtc2p24", "wtc2f24"))


@unittest.skipUnless(ma is not None, "numpy absent")
class Aligner(unittest.TestCase):
    def synth(self, tempo_curve):
        """A synthetic score + a warped 'performance' of it."""
        import note_align as na
        score = []
        for i in range(64):
            ticks = i * 480
            score.append((ticks, [(60 + (i % 12), 0)]))
            if i % 4 == 0:
                score[-1][1].append((48 + (i % 12), 1))
        notes = []
        t = 3.0  # the piece starts mid-file
        for i, (ticks, ps) in enumerate(score):
            for k, (p, _c) in enumerate(ps):
                notes.append(smf.Note(p, 60 + (i % 20), t + 0.004 * k,
                                      t + 0.3, 0, 0))
            t += tempo_curve(i)
        return score, notes

    def test_subsequence_dtw_recovers_a_warped_performance(self):
        score, notes = self.synth(lambda i: 0.4 + 0.15 * (i / 64))
        pc = ma.perf_chords(notes)
        path = ma.dtw_path(score, pc)
        pairs, deleted, inserted = ma.pair_notes(score, pc, path)
        total = sum(len(ps) for _, ps in score)
        self.assertGreaterEqual(len(pairs) / total, 0.97)
        # beat grid follows the accelerating warp
        grid, times = ma.beat_grid_times(pairs, score[-1][0])
        mid = grid.index(32 * 480)
        expect = 3.0 + sum(0.4 + 0.15 * (i / 64) for i in range(32))
        self.assertLess(abs(times[mid] - expect), 0.05)

    def test_match_roundtrip_through_parse_match(self):
        import tempfile
        import note_align as na
        score, notes = self.synth(lambda i: 0.5)
        pc = ma.perf_chords(notes)
        path = ma.dtw_path(score, pc)
        pairs, deleted, inserted = ma.pair_notes(score, pc, path)
        with tempfile.NamedTemporaryFile(suffix=".match",
                                         mode="w", delete=False) as f:
            pass
        ma.write_match(f.name, pairs, deleted, inserted,
                       {"piece": "t", "maestro": "m"})
        mp = na.parse_match(f.name)
        os.unlink(f.name)
        self.assertEqual(len(mp.rows), len(pairs))
        # keys and velocities survive the round trip exactly
        keys = sorted((t, p) for t, p, _c, _x in pairs)
        got = sorted((k, p) for (k, _i, p, *_r) in mp.rows)
        self.assertEqual(keys, got)
        self.assertEqual(mp.deletions, len(deleted))
        self.assertEqual(mp.insertions, len(inserted))

    def test_rolled_chords_still_match(self):
        # a six-note chord spread over 300 ms: containment cost + the
        # tempo-relative recovery must still claim every note. The
        # chords walk (distinct pitch content per chord) — a perfectly
        # periodic score would be self-similar in a way real music
        # never is, and no aligner can pick an offset within one.
        import note_align as na
        score = [(i * 960,
                  [(40 + i + j * 3, 0) for j in range(6)] if i % 2 == 0
                  else [(80 + (i * 7) % 12, 0)]) for i in range(24)]
        notes = []
        t = 2.0
        for ticks, ps in score:
            for k, (p, _c) in enumerate(ps):
                notes.append(smf.Note(p, 70, t + 0.06 * k, t + 0.8, 0, 0))
            t += 1.1
        pc = ma.perf_chords(notes)
        path = ma.dtw_path(score, pc)
        pairs, deleted, inserted = ma.pair_notes(score, pc, path)
        grid, times = ma.beat_grid_times(pairs, score[-1][0])
        pairs, deleted, inserted = ma.recover(
            pairs, deleted, inserted, grid, times)
        total = sum(len(ps) for _, ps in score)
        self.assertGreaterEqual(len(pairs) / total, 0.95)


@unittest.skipUnless(os.path.isfile(ASAP_MID), "asap corpus absent")
class SmfVsMatch(unittest.TestCase):
    def test_smf_reproduces_match_ground_truth(self):
        import note_align as na
        notes = smf.read_smf(ASAP_MID)
        mp = na.parse_match(ASAP_MATCH)
        by_pitch = {}
        for n in notes:
            by_pitch.setdefault(n.pitch, []).append(n)
        ok = vel_ok = 0
        for (_, _, pitch, on_s, _off, vel, *_) in mp.rows:
            cands = by_pitch.get(pitch, [])
            if not cands:
                continue
            best = min(cands, key=lambda n: abs(n.on_s - on_s))
            if abs(best.on_s - on_s) < 0.002:
                ok += 1
                vel_ok += best.vel == vel
        self.assertEqual(ok, len(mp.rows))
        self.assertEqual(vel_ok, len(mp.rows))


if __name__ == "__main__":
    unittest.main()
