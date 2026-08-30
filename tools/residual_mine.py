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

STATUS: research instrument, not an oracle. The effects are
observational associations on one corpus; the marginal table is
descriptive (contexts overlap — trust the joint model's coefficients);
a "discovered rule" here is a HYPOTHESIS that earns rule status only
by being implemented, re-run, and surviving on held-out pieces.

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


def contexts_for(ir, positions, kinds):
    """One list of context tags per beat."""
    cadences = ir.get("cadences", [])
    rules = note_whys_by_beat(ir, positions)
    # bar membership by COUNTING annotated downbeats — pickups and
    # meter changes make anything duration-based lie: "first two bars"
    # means the pickup plus the first two full bars, "final two bars"
    # means from the second-to-last downbeat on
    dbs_before = []
    n = 0
    for kd in kinds:
        if kd == "db":
            n += 1
        dbs_before.append(n)
    total_dbs = n
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
        db_k = dbs_before[k] if k < len(dbs_before) else total_dbs
        if db_k >= total_dbs - 1:
            tags.append("final two bars")
        elif db_k <= 2:
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

    # moments that survive a JOINT context model, fitted leave-one-
    # piece-out: marginal means double-count shared effects (every beat
    # has a metrical tag plus possible cadence/subject/boundary tags),
    # so a design-matrix least squares is fitted over all OTHER pieces
    # and only its residual may call a beat unexplained
    # mid-bar beat is the REFERENCE category: the three metrical
    # indicators are exhaustive, so keeping all of them alongside an
    # intercept is unidentifiable (their columns sum to the intercept
    # and ridge splits the effect arbitrarily); coefficients read
    # "relative to a mid-bar beat"
    feats = sorted(f for f in effect if f != "mid-bar beat")
    fi = {f: i for i, f in enumerate(feats)}
    p_dim = len(feats) + 1

    def xvec(tags):
        x = [0.0] * p_dim
        for tg in tags:
            if tg in fi:
                x[fi[tg]] = 1.0
        x[-1] = 1.0
        return x

    def solve(a, b):
        n = len(b)
        a = [row[:] + [b[i]] for i, row in enumerate(a)]
        for c in range(n):
            piv = max(range(c, n), key=lambda r: abs(a[r][c]))
            a[c], a[piv] = a[piv], a[c]
            if abs(a[c][c]) < 1e-12:
                continue
            for r in range(n):
                if r != c and a[r][c] != 0:
                    f = a[r][c] / a[c][c]
                    a[r] = [x - f * y for x, y in zip(a[r], a[c])]
        return [a[i][-1] / a[i][i] if abs(a[i][i]) > 1e-12 else 0.0
                for i in range(n)]

    # accumulate XtX / Xty globally and per piece (ridge for stability)
    xtx = [[0.0] * p_dim for _ in range(p_dim)]
    xty = [0.0] * p_dim
    per_piece = {}
    for piece, _, tags, rs in beat_data:
        x = xvec(tags)
        y = sum(rs) / len(rs)
        pp = per_piece.setdefault(
            piece, ([[0.0] * p_dim for _ in range(p_dim)], [0.0] * p_dim))
        for i in range(p_dim):
            if x[i] == 0:
                continue
            xty[i] += x[i] * y
            pp[1][i] += x[i] * y
            for j in range(p_dim):
                if x[j] != 0:
                    xtx[i][j] += x[i] * x[j]
                    pp[0][i][j] += x[i] * x[j]
    for i in range(p_dim):
        xtx[i][i] += 1e-6

    beta_full = solve([row[:] for row in xtx], xty[:])
    print("\njoint model coefficients (all pieces; reference category "
          "= mid-bar beat):")
    for f in feats:
        pct = (math.exp(beta_full[fi[f]]) - 1) * 100
        print(f"  {f:<22} {pct:+.1f}%")

    moments = []
    for piece, wn, tags, rs in beat_data:
        if len(rs) < 2:
            continue
        pxx, pxy = per_piece[piece]
        a = [[xtx[i][j] - pxx[i][j] for j in range(p_dim)]
             for i in range(p_dim)]
        b = [xty[i] - pxy[i] for i in range(p_dim)]
        beta = solve(a, b)
        x = xvec(tags)
        mu = sum(rs) / len(rs)
        adj = mu - sum(xi * bi for xi, bi in zip(x, beta))
        spread = max(rs) - min(rs)
        if abs(adj) > 0.15 and spread < abs(adj):
            moments.append((abs(adj), piece, wn, adj, tags))
    print("\nunexplained moments (performer consensus AFTER a joint "
          "context model,\nfitted leave-one-piece-out):")
    for _, piece, wn, adj, tags in sorted(moments, reverse=True)[:15]:
        pct = (math.exp(adj) - 1) * 100
        print(f"  {piece} @ {wn:.2f}wn: humans {pct:+.0f}% beyond context"
              f"  ctx={','.join(tags) or 'none'}")


if __name__ == "__main__":
    main()
