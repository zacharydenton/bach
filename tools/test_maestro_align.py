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
