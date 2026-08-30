#!/usr/bin/env python3
"""Rule induction, stage one: mine what humans do that no rule explains.

For every ASAP performance of a WTC piece, compute the per-beat
residual between the human's tempo curve and the compiler's — both as
zero-mean log local tempo, so global tempo choice cancels and only
*shape* remains. Then bucket the residuals by musical context read from
the score annotations, the IR's own analysis (cadences), and the whys
(subject entries, breaths) — and report each context's mean effect with
its support and a t-statistic.

A large consistent effect in a nameable context IS a candidate rule:
"humans are 6% slower than the compiler on cadence arrivals" is a
falsifiable claim about practice, discovered rather than cited, and
ready to be implemented, re-run, and watched shrinking toward zero.
This inverts the project's method exactly once: everywhere else the
literature proposes and the corpus disposes; here the corpus proposes.

  tools/residual_mine.py                 # all overlapping pieces
  tools/residual_mine.py wtc1f01 ...     # a selection

License: GPL-2.0-or-later.
"""

import math
import os
import sys
import tempfile

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from asap_eval import (ROOT, bwv_of, compile_ir, local_tempo,  # noqa: E402
                       our_time_at, perf_beat_times, score_beat_positions)

ASAP = os.path.join(ROOT, "corpus", "asap")
KERN = os.path.join(ROOT, "corpus", "bach-wtc", "kern")


def beat_kinds(path):
    """'db' / 'b' per beat, straight from the score annotations."""
    kinds = []
    with open(path) as f:
        for line in f:
            parts = line.rstrip("\n").split("\t")
            if len(parts) >= 3:
                kinds.append(parts[2].split(",")[0])
    return kinds


def zero_mean_log(tempi):
    logs = [math.log(t) if t and t > 0 and not math.isnan(t) else None
            for t in tempi]
    vals = [x for x in logs if x is not None]
    if not vals:
        return [None] * len(logs)
    m = sum(vals) / len(vals)
    return [None if x is None else x - m for x in logs]


def note_whys_by_beat(ir, positions):
    """Sets of active rule names per beat, from the IR's whys."""
    step = positions[1] - positions[0] if len(positions) > 1 else 0.25
    rules = [set() for _ in positions]
    for tr in ir["tracks"]:
        for n in tr:
            if not n.get("whys"):
                continue
            k = int(n["onWn"] / step) if step > 0 else 0
            if 0 <= k < len(rules):
                for w in n["whys"]:
                    rules[k].add(w.split(":")[0])
    return rules


def contexts_for(ir, positions, kinds):
    """One list of context tags per beat."""
    cadences = ir.get("cadences", [])
    rules = note_whys_by_beat(ir, positions)
    end = positions[-1] if positions else 0
    out = []
    for k, pos in enumerate(positions):
        tags = []
        kind = kinds[k] if k < len(kinds) else "b"
        nxt = kinds[k + 1] if k + 1 < len(kinds) else "db"
        tags.append("downbeat" if kind == "db" else
                    ("bar-final beat" if nxt == "db" else "mid-bar beat"))
        dist = min((c - pos for c in cadences if c >= pos),
                   default=float("inf"))
        if dist <= 0.05:
            tags.append("cadence arrival")
        elif dist <= 0.55:
            tags.append("approaching cadence")
        if "subject-entry" in rules[k]:
            tags.append("subject sounding")
        if "breath" in rules[k]:
            tags.append("breath here")
        if end - pos <= 2.0:
            tags.append("final two bars")
        elif pos <= 2.0:
            tags.append("first two bars")
        out.append(tags)
    return out


def main():
    pieces = sys.argv[1:]
    if not pieces:
        pieces = sorted(p[:-4] for p in os.listdir(KERN)
                        if p.endswith(".krn"))
    pieces = [p for p in pieces
              if os.path.isdir(os.path.join(ASAP, "Bach", *bwv_of(p)))]

    import subprocess
    root = subprocess.check_output(
        ["stack", "path", "--local-install-root"], cwd=ROOT,
        stderr=subprocess.DEVNULL, text=True).strip()
    otb = os.path.join(root, "bin", "otb")

    buckets = {}  # tag -> [residuals]
    moments = []  # (|residual|, piece, beat wn, residual, tags)
    nperf = 0
    with tempfile.TemporaryDirectory() as tmp:
        for piece in pieces:
            kind, bwv = bwv_of(piece)
            pdir = os.path.join(ASAP, "Bach", kind, bwv)
            ann = os.path.join(pdir, "midi_score_annotations.txt")
            positions = score_beat_positions(ann)
            kinds = beat_kinds(ann)
            ir = compile_ir(otb, piece, KERN, tmp)
            ctxs = contexts_for(ir, positions, kinds)
            tempo_map = [(t["wn"], t["bpm"]) for t in ir["tempoMap"]]
            ours = zero_mean_log(local_tempo(
                [our_time_at(tempo_map, w) for w in positions]))
            piece_res = {}
            for f in sorted(os.listdir(pdir)):
                if (not f.endswith("_annotations.txt")
                        or f.startswith("midi_score")):
                    continue
                human = zero_mean_log(local_tempo(
                    perf_beat_times(os.path.join(pdir, f))))
                nperf += 1
                for k in range(min(len(ours), len(human))):
                    if ours[k] is None or human[k] is None:
                        continue
                    r = human[k] - ours[k]
                    piece_res.setdefault(k, []).append(r)
                    for tag in ctxs[k]:
                        buckets.setdefault(tag, []).append(r)
            # unexplained moments: beats where every performer agrees
            # and the residual is large — the corpus proposing a rule
            for k, rs in piece_res.items():
                if len(rs) >= 2:
                    m = sum(rs) / len(rs)
                    spread = max(rs) - min(rs)
                    if abs(m) > 0.15 and spread < abs(m):
                        moments.append(
                            (abs(m), piece, positions[k], m, ctxs[k]))

    print(f"{nperf} performances, {len(pieces)} pieces\n")
    print(f"{'context':<22} {'n':>7} {'mean effect':>12} {'t':>7}")
    rows = []
    for tag, rs in buckets.items():
        n = len(rs)
        m = sum(rs) / n
        sd = math.sqrt(sum((x - m) ** 2 for x in rs) / max(1, n - 1))
        t = m / (sd / math.sqrt(n)) if sd > 0 else 0
        rows.append((abs(t), tag, n, m, t))
    for _, tag, n, m, t in sorted(rows, reverse=True):
        pct = (math.exp(m) - 1) * 100
        print(f"{tag:<22} {n:>7} {pct:>+10.1f}% {t:>7.1f}")
    print("\n(positive = humans faster than the compiler there;"
          " negative = humans slower)")

    print("\nunexplained moments (all performers agree, no context"
          " explains it):")
    for _, piece, wn, m, tags in sorted(moments, reverse=True)[:15]:
        pct = (math.exp(m) - 1) * 100
        print(f"  {piece} @ {wn:.2f}wn: humans {pct:+.0f}%"
              f"  ctx={','.join(tags) or 'none'}")


if __name__ == "__main__":
    main()
