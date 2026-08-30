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
    import bisect
    rules = [set() for _ in positions]
    for tr in ir["tracks"]:
        for n in tr:
            if not n.get("whys"):
                continue
            # the beat containing this onset, on the ACTUAL grid —
            # pickups and meter changes make it non-uniform
            k = bisect.bisect_right(positions, n["onWn"] + 1e-9) - 1
            if 0 <= k < len(rules):
                for w in n["whys"]:
                    rules[k].add(w.split(":")[0])
    return rules


def bar_len_at(kinds, positions, k):
    """Length of the bar containing beat k, from the annotations."""
    lo = k
    while lo > 0 and kinds[lo] != "db":
        lo -= 1
    hi = k + 1
    while hi < len(kinds) and kinds[hi] != "db":
        hi += 1
    if hi < len(positions):
        return positions[hi] - positions[lo]
    if lo > 0:
        return positions[lo] - positions[lo - 1] \
            if kinds[lo - 1] == "db" else 1.0
    return 1.0


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
        two_bars = 2 * bar_len_at(kinds, positions, k)
        if end - pos <= two_bars:
            tags.append("final two bars")
        elif pos <= two_bars:
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

    # collect everything first; statistics happen in a second pass so
    # moments can be tested AGAINST the fitted context effects
    buckets = {}  # tag -> perf_key -> [residuals]
    beat_data = []  # (piece, beat wn, tags, [residuals across perfs])
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
                perf_key = (piece, f)
                human = zero_mean_log(local_tempo(
                    perf_beat_times(os.path.join(pdir, f))))
                nperf += 1
                for k in range(min(len(ours), len(human))):
                    if ours[k] is None or human[k] is None:
                        continue
                    r = human[k] - ours[k]
                    piece_res.setdefault(k, []).append(r)
                    for tag in ctxs[k]:
                        buckets.setdefault(tag, {}).setdefault(
                            perf_key, []).append(r)
            for k, rs in piece_res.items():
                if len(rs) >= 2:
                    beat_data.append((piece, positions[k], ctxs[k], rs))

    # per-context effects; the t-statistic clusters by PERFORMANCE
    # (beats within a performance are serially correlated — treating
    # them as independent overstated the evidence enormously)
    effect = {}
    print(f"{nperf} performances, {len(pieces)} pieces\n")
    print(f"{'context':<22} {'beats':>7} {'perfs':>6} "
          f"{'mean effect':>12} {'t':>6}")
    rows = []
    for tag, by_perf in buckets.items():
        per_perf = [sum(rs) / len(rs) for rs in by_perf.values()]
        n_beats = sum(len(rs) for rs in by_perf.values())
        np_ = len(per_perf)
        mu = sum(per_perf) / np_
        effect[tag] = mu
        sd = math.sqrt(sum((x - mu) ** 2 for x in per_perf)
                       / max(1, np_ - 1))
        t = mu / (sd / math.sqrt(np_)) if sd > 0 and np_ > 1 else 0
        rows.append((abs(t), tag, n_beats, np_, mu, t))
    for _, tag, n_beats, np_, mu, t in sorted(rows, reverse=True):
        pct = (math.exp(mu) - 1) * 100
        print(f"{tag:<22} {n_beats:>7} {np_:>6} {pct:>+10.1f}% {t:>6.1f}")
    print("\n(positive = humans faster than the compiler there; "
          "negative = humans slower;\n t is clustered by performance)")

    # moments that survive the fitted context effects: subtract every
    # applicable context's mean before judging the beat unexplained
    moments = []
    for piece, wn, tags, rs in beat_data:
        mu = sum(rs) / len(rs)
        adj = mu - sum(effect.get(tg, 0) for tg in tags)
        spread = max(rs) - min(rs)
        if abs(adj) > 0.15 and spread < abs(adj):
            moments.append((abs(adj), piece, wn, adj, tags))
    print("\nunexplained moments (performer consensus AFTER removing "
          "the modeled\ncontext effects):")
    for _, piece, wn, adj, tags in sorted(moments, reverse=True)[:15]:
        pct = (math.exp(adj) - 1) * 100
        print(f"  {piece} @ {wn:.2f}wn: humans {pct:+.0f}% beyond context"
              f"  ctx={','.join(tags) or 'none'}")


if __name__ == "__main__":
    main()
