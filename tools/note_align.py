"""Note-level bridge: otb's IR notes <-> ASAP .match performed notes.

ASAP ships note-level ground truth per performance: a Vienna-format
.match file pairing every score note (MusicXML id, spelled pitch,
measure:beat, onset in meter-denominator beats, voice/staff/ornament
tags) with its performed note (MIDI pitch, onset/offset ticks,
VELOCITY), plus explicit deletion/insertion sentinels for performance
mistakes. otb's JSON IR carries each performed note's score-level
identity (srcWn, srcPitch) which survives ornament realisation and every
reshaper. The bridge is therefore a score-to-score join on
(notated onset, notated pitch) — no MIDI parsing, no alignment search.

Everything that fails to bridge is COUNTED, never silent:
  matched + snote_unmatched + snote_deleted == total snotes.

Library use (the asap_eval pattern):
    import note_align as na
    perf = na.parse_match(path)                    # cached per path
    rows, counters = na.bridge(na.load_ir_notes(ir), perf)

Report mode:
    python3 tools/note_align.py [wtc1f01 ...]      # coverage over pieces

Coverage at 2026-09-01: 53/58 overlap pieces bridge >= 90% (most 96-99).
Five are edition-divergent between the humdrum and MusicXML sources and
bridge partially — their matchable subsets still contribute:
  wtc2p11 60% (two mid-piece divergence points), wtc2p04 65%,
  wtc1p11 84% (xml writes ornaments out; the orn pass recovers most),
  wtc1p12 88%, wtc1f11 89% (anacrusis offset found automatically).
"""
import functools
import json
import math
import os
import re
import subprocess
import sys
import tempfile

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import asap_eval as ae  # noqa: E402

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
ASAP = os.path.join(ROOT, "corpus", "asap")

WN_TICKS = 1920  # ticks per whole note for the join key; exact for
                 # anything the corpus notates

SNOTE_RE = re.compile(
    r"snote\(([^,]+),\[([A-Ga-g]),([^\]]*)\],(-?\d+),(\d+):([^,]+),"
    r"([^,]+),([^,]+),([-\d.]+),([-\d.]+),\[([^\]]*)\]\)-(.*)")
NOTE_RE = re.compile(r"note\(([^,]+),(\d+),(\d+),(\d+),(\d+)")
INFO_RE = re.compile(r"info\((midiClockUnits|midiClockRate),(\d+)\)")
TSIG_RE = re.compile(r"scoreprop\(timeSignature,(\d+)/(\d+),")
GATE_RE = re.compile(r"articulation: gate (\d+)% \((.+?)\)")

PC = {"C": 0, "D": 2, "E": 4, "F": 5, "G": 7, "A": 9, "B": 11}
ACC = {"n": 0, "#": 1, "b": -1, "x": 2, "bb": -2, "": 0}


class MatchPerf:
    """One performance's parsed .match: rows keyed for the bridge."""

    def __init__(self):
        self.den = 4
        self.sec_per_tick = 500000 / 1e6 / 480
        self.rows = []  # (key, xml_id, pitch, on_s, off_s, vel, voice, tags)
        self.deletions = 0
        self.insertions = 0
        self.pitch_mismatch = 0
        self.extra_timesigs = 0
        self.grace_snotes = 0


def spelled_pitch(letter, acc, octave):
    return PC[letter.upper()] + ACC.get(acc, 0) + 12 * (octave + 1)


@functools.lru_cache(maxsize=None)
def parse_match(path):
    mp = MatchPerf()
    units, rate = 480, 500000
    tsigs = []
    for line in open(path):
        m = INFO_RE.match(line)
        if m:
            if m.group(1) == "midiClockUnits":
                units = int(m.group(2))
            else:
                rate = int(m.group(2))
            continue
        m = TSIG_RE.match(line)
        if m:
            tsigs.append((int(m.group(1)), int(m.group(2))))
            continue
        if line.startswith("insertion-note("):
            mp.insertions += 1
            continue
        m = SNOTE_RE.match(line)
        if not m:
            continue
        (xml_id, letter, acc, octave, _meas, _beat, _off, _dur,
         on_beats, off_beats, tags, rhs) = m.groups()
        if rhs.startswith("deletion"):
            mp.deletions += 1
            continue
        nm = NOTE_RE.match(rhs)
        if not nm:
            mp.deletions += 1  # unreadable pairing: treat as unplayed
            continue
        _nid, midi_pitch, on_t, off_t, vel = nm.groups()
        midi_pitch = int(midi_pitch)
        if spelled_pitch(letter, acc, int(octave)) != midi_pitch:
            mp.pitch_mismatch += 1  # trust the note's own midi field
        is_grace = float(on_beats) == float(off_beats)
        if is_grace:
            mp.grace_snotes += 1
        # voice identity must include the staff: 147/169 WTC matches
        # reuse a voice number across staves, and a gate measured
        # against a different staff's next note is meaningless
        tag_bits = tags.split(",") if tags else []
        voice_id = ",".join(tag_bits[:2]) if len(tag_bits) >= 2 else tags
        mp.rows.append((
            None,  # key filled below once den is known
            xml_id, midi_pitch,
            int(on_t), int(off_t), int(vel),
            voice_id,
            tags,
            float(on_beats), is_grace))
    mp.extra_timesigs = max(0, len(tsigs) - 1)
    if tsigs:
        mp.den = tsigs[0][1]
    mp.sec_per_tick = rate / 1e6 / units
    mp.rows = [
        (int(round(ob / mp.den * WN_TICKS)), xml_id, p,
         on_t * mp.sec_per_tick, off_t * mp.sec_per_tick, vel, voice, tags,
         gr)
        for (_, xml_id, p, on_t, off_t, vel, voice, tags, ob, gr)
        in mp.rows]
    return mp


def load_ir_notes(ir):
    """Group IR notes by score identity (srcWn ticks, srcPitch).

    The representative of an ornament group is its earliest keystroke —
    the one the ASAP aligner will have matched for a trill.
    Returns dict key -> list of representatives (unisons keep several).
    """
    groups = {}
    max_src = 0
    for track in ir["tracks"]:
        for n in track:
            key = (int(round(n["srcWn"] * WN_TICKS)), n["srcPitch"])
            groups.setdefault(key, []).append(n)
            max_src = max(max_src, key[0])
    out = {}
    for key, ns in groups.items():
        # split by track/channel: a unison between voices is two notes
        by_ch = {}
        for n in ns:
            by_ch.setdefault(n["ch"], []).append(n)
        reps = []
        for ch in sorted(by_ch):
            sub = sorted(by_ch[ch], key=lambda n: n["onS"])
            rep = dict(sub[0])
            rep["is_orn"] = len(sub) > 1
            # keep the whole realisation: an edition that WRITES OUT the
            # ornament (wtc1p11's xml spells the trill in 32nds) matches
            # keystroke-for-keystroke against these subnotes
            rep["group"] = sub if len(sub) > 1 else None
            g = next((GATE_RE.search(w) for w in sub[0].get("whys", [])
                      if GATE_RE.search(w)), None)
            rep["gate_pct"] = int(g.group(1)) if g else None
            rep["gate_label"] = g.group(2) if g else None
            # rule name -> delta text (cite stripped): the delta wording
            # distinguishes e.g. a double-duration SHORT half ("onset"
            # present) from the long half, and a realised grace
            # ("realised at") from the grace-delayed main ("onset +")
            rep["rules"] = {
                w.split(":", 1)[0]: w.split(":", 1)[1].split("  [")[0].strip()
                for w in sub[0].get("whys", []) if ":" in w}
            # the final chord (finalTag's own definition: notated onset
            # equals the piece's last) — the roll emits no why
            rep["is_final"] = key[0] == max_src
            reps.append(rep)
        out[key] = reps
    return out


# kern and MusicXML editions occasionally notate the same music at
# different value scales (wtc1p15: kern triplet-24ths against xml
# straight 16ths in 24/16 -> kern wn = 2/3 xml wn), and matches with an
# anacrusis count negative beats before bar 1 (wtc1f11). The bridge
# therefore estimates an AFFINE map ir_wn = scale * match_wn + offset
# over a small rational candidate set, choosing whatever matches most —
# and reports both, never silently.
SCALES = [(1, 1), (2, 3), (3, 2), (1, 2), (2, 1), (1, 3), (3, 1),
          (3, 4), (4, 3)]


def _affine(ir_keys, match_keys):
    best = (0, 1, 1, 0)  # (matches, num, den, offset)
    for num, den in SCALES:
        scaled = {}
        for (t, p) in match_keys:
            if (t * num) % den == 0:
                scaled.setdefault(p, []).append(t * num // den)
        first_ir, first_m = {}, {}
        for (t, p) in ir_keys:
            first_ir[p] = min(first_ir.get(p, t), t)
        for p, ts in scaled.items():
            first_m[p] = min(ts)
        diffs = [first_ir[p] - first_m[p] for p in first_ir if p in first_m]
        if not diffs:
            continue
        for off in {0, max(set(diffs), key=diffs.count)}:
            got = sum(1 for p, ts in scaled.items() for t in ts
                      if (t + off, p) in ir_keys)
            # identity wins ties: prefer no transform over an equal one
            if got > best[0]:
                best = (got, num, den, off)
    return best[1], best[2], best[3]


def _take(pool, key):
    cands = pool.get(key)
    if not cands:
        return None
    return cands.pop(0)  # greedy in channel order for unisons


def _row(key, snote, rep, how):
    (t, xml_id, p, on_s, off_s, vel, voice, tags, snote_grace) = snote
    return {
        "wn": key[0] / WN_TICKS, "pitch": p, "xml_id": xml_id,
        "voice": voice, "tags": tags, "pass": how,
        "human_vel": vel, "human_on_s": on_s, "human_off_s": off_s,
        "otb_vel": rep["vel"], "otb_on_s": rep["onS"],
        "otb_dur_s": rep["durS"],
        "otb_on_wn": rep.get("onWn"), "otb_dur_wn": rep.get("durWn"),
        "ch": rep.get("ch"),
        "rules": rep.get("rules", {}),
        "is_final": rep.get("is_final", False),
        "snote_grace": snote_grace,
        "gate_pct": rep["gate_pct"], "gate_label": rep["gate_label"],
        "is_orn": rep["is_orn"],
    }


def _segmented(pool, misses, rows):
    """Editions occasionally insert or reinterpret a passage, shifting
    everything after it by a constant (wtc2p11 drifts twice). Re-anchor:
    at each run of misses, propose offsets from same-pitch ir notes near
    the run's head, keep the one that carries the next dozen snotes, and
    extend until it stops working. Every recovery is counted as its own
    pass."""
    still = []
    i = 0
    seg_runs = 0
    while i < len(misses):
        head = misses[i]
        t0, p0 = head[0], head[2]
        cands = {it - t0 for (it, ip), reps in pool.items()
                 if ip == p0 and reps and abs(it - t0) <= 4 * WN_TICKS}
        probe = misses[i:i + 12]
        best_off, best_score = None, 0
        for off in cands:
            score = sum(1 for r in probe if pool.get((r[0] + off, r[2])))
            if score > best_score:
                best_off, best_score = off, score
        if best_off is None or best_score < max(3, (3 * len(probe)) // 4):
            still.append(head)
            i += 1
            continue
        seg_runs += 1
        fails = 0
        while i < len(misses) and fails < 4:
            r = misses[i]
            rep = _take(pool, (r[0] + best_off, r[2]))
            if rep is None:
                still.append(r)
                fails += 1
            else:
                rows.append(_row((r[0] + best_off, r[2]), r, rep, "seg"))
                fails = 0
            i += 1
    return still, seg_runs


def _fuzzy(pool, misses, rows, window=WN_TICKS // 16):
    """Fine notational divergence (triplet vs straight spelling,
    written-out ornaments): match a leftover snote to the nearest
    same-pitch ir note within a sixteenth. One-to-one, nearest first."""
    by_pitch = {}
    for (t, p), reps in pool.items():
        if reps:
            by_pitch.setdefault(p, []).append(t)
    still = []
    for r in sorted(misses, key=lambda r: r[0]):
        ts = by_pitch.get(r[2], [])
        near = sorted((abs(t - r[0]), t) for t in ts
                      if abs(t - r[0]) <= window and pool.get((t, r[2])))
        if not near:
            still.append(r)
            continue
        t = near[0][1]
        rep = _take(pool, (t, r[2]))
        if rep is None:
            still.append(r)
        else:
            rows.append(_row((t, r[2]), r, rep, "fuzzy"))
    return still


def _ornament(ir_notes, misses, rows):
    """An edition that writes an ornament OUT has many snotes where kern
    has one marked note; otb's realised subnotes are those keystrokes.
    Pair leftover snotes with unused subnotes of the same pitch inside
    the parent note's notated span — the right velocity comparison for
    trills on both sides."""
    groups = []
    for (t, _p), reps in ir_notes.items():
        for rep in reps:
            if rep.get("group"):
                subs = rep["group"]
                end = max(int(round((n["onWn"] + n["durWn"]) * WN_TICKS))
                          for n in subs)
                groups.append((t, end, subs))
    still = []
    used = set()  # local: ir_notes is shared across performances
    for r in sorted(misses, key=lambda r: r[0]):
        t, p = r[0], r[2]
        best = None
        for (s, e, subs) in groups:
            if s - 60 <= t <= e + 60:
                for n in subs:
                    if id(n) in used or n["pitch"] != p:
                        continue
                    d = abs(int(round(n["onWn"] * WN_TICKS)) - t)
                    if best is None or d < best[0]:
                        best = (d, n)
        if best is None or best[0] > WN_TICKS // 4:
            still.append(r)
            continue
        n = best[1]
        used.add(id(n))
        rep = {"vel": n["vel"], "onS": n["onS"], "durS": n["durS"],
               "gate_pct": None, "gate_label": None, "is_orn": True}
        rows.append(_row((t, p), r, rep, "orn"))
    return still


def bridge(ir_notes, mp):
    """Join IR representatives to match rows on the score key: exact
    affine pass, then segmented re-anchoring, a small pitch-anchored
    fuzzy pass, and a written-out-ornament pass. Rows carry which pass
    matched them ("pass": exact | seg | fuzzy | orn) so measurements can
    restrict to exact when position precision matters.
    Returns (rows, counters)."""
    num, den, offset = _affine(
        set(ir_notes), {(r[0], r[2]) for r in mp.rows})

    pool = {key: list(reps) for key, reps in ir_notes.items()}
    rows = []
    misses = []
    for snote in mp.rows:
        (t, _xml_id, p, *_rest) = snote
        # nearest-int transform; misses carry the TRANSFORMED tick so
        # the later passes work in one coordinate system
        kt = int(round(t * num / den)) + offset
        key = (kt, p)
        rep = _take(pool, key)
        if rep is None:
            misses.append((kt,) + tuple(snote[1:]))
        else:
            rows.append(_row(key, snote, rep, "exact"))
    exact = len(rows)
    misses, seg_runs = _segmented(pool, misses, rows)
    misses = _fuzzy(pool, misses, rows)
    misses = _ornament(ir_notes, misses, rows)

    counters = {
        "matched": len(rows),
        "exact": exact,
        "seg": sum(1 for r in rows if r["pass"] == "seg"),
        "fuzzy": sum(1 for r in rows if r["pass"] == "fuzzy"),
        "orn": sum(1 for r in rows if r["pass"] == "orn"),
        "seg_runs": seg_runs,
        "snote_unmatched": len(misses),
        "snote_deleted": mp.deletions,
        "insertions": mp.insertions,
        "pitch_mismatch": mp.pitch_mismatch,
        "grace_snotes": mp.grace_snotes,
        "extra_timesigs": mp.extra_timesigs,
        "offset_wn": offset / WN_TICKS,
        "scale": (num, den),
        "total_snotes": len(mp.rows) + mp.deletions,
    }
    assert counters["matched"] + counters["snote_unmatched"] \
        + counters["snote_deleted"] == counters["total_snotes"]
    return rows, counters


def interp_time(xs, ys, x):
    """Piecewise-linear ys(x) with edge-SLOPE extrapolation: clamping
    the time outside the annotated span would fabricate huge
    deviations. xs strictly increasing, len >= 2."""
    import bisect
    i = bisect.bisect_right(xs, x)
    i = max(1, min(i, len(xs) - 1))
    x0, x1 = xs[i - 1], xs[i]
    y0, y1 = ys[i - 1], ys[i]
    return y0 + (y1 - y0) * (x - x0) / (x1 - x0)


def zscore(vals):
    n = len(vals)
    if n < 2:
        return [0.0] * n
    mu = sum(vals) / n
    sd = math.sqrt(sum((v - mu) ** 2 for v in vals) / n) or 1.0
    return [(v - mu) / sd for v in vals]


def piece_performances(piece, asap_dir=ASAP):
    pdir = os.path.join(asap_dir, "Bach", *ae.bwv_of(piece))
    if not os.path.isdir(pdir):
        return []
    return [(f[:-len(".match")], os.path.join(pdir, f))
            for f in sorted(os.listdir(pdir)) if f.endswith(".match")]


def main():
    kern = os.path.join(ROOT, "corpus", "bach-wtc", "kern")
    pieces = sys.argv[1:] or sorted(
        p[:-4] for p in os.listdir(kern) if p.endswith(".krn"))
    pieces = [p for p in pieces if piece_performances(p)]
    otb = subprocess.check_output(
        ["stack", "path", "--local-install-root"], cwd=ROOT,
        stderr=subprocess.DEVNULL, text=True).strip() + "/bin/otb"
    worst = []
    with tempfile.TemporaryDirectory() as tmp:
        print(f"{'piece':<10} {'perfs':>5} {'bridged%':>9} "
              f"{'unmatch':>8} {'deleted':>8} {'offset':>7}")
        for piece in pieces:
            ir = ae.compile_ir(otb, piece, kern, tmp)
            irn = load_ir_notes(ir)
            rates, unm, dele, notes = [], 0, 0, set()
            for _, mpath in piece_performances(piece):
                rows, c = bridge(irn, parse_match(mpath))
                rates.append(c["matched"] / max(1, c["total_snotes"]))
                unm += c["snote_unmatched"]
                dele += c["snote_deleted"]
                if c["offset_wn"]:
                    notes.add(f"off={c['offset_wn']}")
                if c["scale"] != (1, 1):
                    notes.add(f"scale={c['scale'][0]}/{c['scale'][1]}")
            rate = sum(rates) / len(rates)
            worst.append((rate, piece))
            print(f"{piece:<10} {len(rates):>5} {100 * rate:>8.1f}% "
                  f"{unm:>8} {dele:>8}  {' '.join(sorted(notes))}")
    worst.sort()
    print("\nworst bridges:", [(p, f"{100 * r:.1f}%") for r, p in worst[:5]])
    below = [p for r, p in worst if r < 0.9]
    print(f"pieces below 90%: {below or 'none'}")


if __name__ == "__main__":
    main()
