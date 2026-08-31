#!/usr/bin/env python3
"""The structural optimizer: fit tempo shape to song structure directly.

The knob fitter can only rescale shapes that already exist in the
compiler. This stage removes that ceiling: each piece's STRUCTURE —
boundary strengths, grouping tree, cadences, subject entries, harmonic
charge, note density, meter — becomes a per-beat design matrix (with
lagged copies, so the data itself can choose between a ramp INTO a
boundary and a hold ON it), and human tempo deviation is regressed on
it. Grachten & Widmer's basis-function move, pointed at our own
analysis: the compiler's rules are candidate basis functions, and the
coefficients say which shapes carry human practice.

Discipline as ever: ridge fitted on the Book I overlap, Book II held
out; the target metric is MIDDLE-ONLY r (first/final two bars
excluded), because the full-curve metric is 90% saturated and the
middle is the frontier (see "The Corpus Speaks", 2026-08-31: compiler
0.144 vs human ceiling 0.356 there).

STATUS: research instrument, and its first finding is NEGATIVE
(2026-08-31): a ridge over these features — dense OR sparse-event
variants — does not beat the existing compiler in the middle (test
0.108-0.111 vs compiler 0.140, ceiling 0.356). The coefficient SHAPES
confirmed the hold hypothesis (slowing on the boundary beat and the
next, not a ramp before), but the magnitudes are tiny: the structural
vocabulary we currently extract (boundaries, grouping tree, cadences,
charge, density, meter, subject) is linearly EXHAUSTED by the rules
already implemented. The middle frontier needs new ANALYSIS — melodic
parallelism/sequence detection, harmonic rhythm, phrase structure at
finer grain — not better optimization over the current features.

  tools/structure_fit.py          # ridge (linear baseline)
  tools/structure_fit.py --gbm    # gradient boosting, +/-4 beat window

SECOND FINDING (--gbm, same day): the features are NOT exhausted, only
linearly. Gradient boosting over a +/-4-beat context window reaches
test middle r = 0.177 vs the compiler's 0.140 (ceiling 0.356). Its
attention is ~35% on note-density windows — textural rhythm, which no
implemented rule uses — but the signal is interactive: linear pulls
top out at r 0.07 and the best derived feature (anticipatory braking
before texture thickens) at 0.06 on both books, so it resists
distillation into a single named rule at meaningful magnitude. The
+0.037 sits on the table as the price of staying fully symbolic.

License: GPL-2.0-or-later.
"""

import json
import math
import os
import subprocess
import sys
import tempfile

import numpy as np

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from asap_eval import (ROOT, bwv_of, compile_ir, local_tempo,  # noqa: E402
                       our_time_at, perf_beat_times,
                       score_beat_positions, pearson)
from residual_mine import beat_kinds, zero_mean_log  # noqa: E402

ASAP = os.path.join(ROOT, "corpus", "asap")
KERN = os.path.join(ROOT, "corpus", "bach-wtc", "kern")

FEATURES = [
    "bnd[k-1]", "bnd[k]", "bnd[k+1]",
    "STRONG[k-1]", "STRONG[k]", "STRONG[k+1]",
    "tree-end[k]", "tree-end[k+1]",
    "cadence[k]", "cadence[k-1]",
    "subject-on[k]",
    "charge[k]",
    "density[k]",
    "downbeat", "bar-final",
    "pos", "arch",
    "seq-active[k]", "seam[k]", "seam[k+1]", "seam[k-1]",
    "novelty[k]", "novelty[k-1]",
    "exchange[k]", "take[k]", "take[k+1]",
]


def analyze(otb, piece):
    r = subprocess.run(
        [otb, "analyze", os.path.join(KERN, piece + ".krn")],
        capture_output=True, text=True, cwd=ROOT)
    if r.returncode != 0:
        raise RuntimeError(f"analyze {piece}: {r.stderr.strip()}")
    return json.loads(r.stdout)


def in_beat(items, lo, hi, val=lambda x: x[1], pos=lambda x: x[0]):
    return sum(val(x) for x in items if lo <= pos(x) < hi)


def design(piece, ana, positions, kinds):
    """Feature matrix, one row per beat transition k -> k+1."""
    n = len(positions) - 1
    end = ana["end"]
    bounds = ana["boundaries"]
    cadences = [(c, 1.0) for c in ana["cadences"]]
    subj = [(s, 1.0) for s in ana["subject"]]
    onsets = [(o, 1.0) for o in ana["onsets"]]
    charge = ana["charge"]
    tree_ends = [(b, 1.0 / d) for a, b, d in ana["tree"]]
    seq_spans = ana.get("sequences", [])
    ex_spans = ana.get("exchanges", [])
    takes = [(t, 1.0) for t, _, _ in ana.get("takes", [])]
    seams = [(t, 1.0) for t in ana.get("seams", [])]
    nov = ana.get("novelty", [])

    def beat_span(k):
        lo = positions[k]
        hi = positions[k + 1] if k + 1 < len(positions) else end
        return lo, hi

    rows = np.zeros((n, len(FEATURES)))
    bnd = np.zeros(n + 2)
    strong = np.zeros(n + 2)
    cad = np.zeros(n + 1)
    ten = np.zeros(n + 2)
    seam = np.zeros(n + 2)
    novb = np.zeros(n + 2)
    for k in range(n):
        lo, hi = beat_span(k)
        bnd[k + 1] = in_beat(bounds, lo, hi)
        cad[k] = in_beat(cadences, lo, hi)
        ten[k + 1] = in_beat(tree_ends, lo, hi)
    # the deep holds are sparse, large EVENTS: a binary top-decile
    # boundary feature lets the model say "mostly nothing, here a lot"
    thresh = np.quantile(bnd[bnd > 0], 0.9) if (bnd > 0).any() else 1e9
    strong[bnd >= thresh] = 1.0
    take_next = np.zeros(n + 1)
    for k in range(n):
        lo, hi = beat_span(k)
        seam[k + 1] = in_beat(seams, lo, hi)
        take_next[k - 1 if k > 0 else 0] += in_beat(takes, lo, hi)
        tot = in_beat(nov, lo, hi, val=lambda x: 1.0)
        novb[k + 1] = (in_beat(nov, lo, hi) / tot) if tot > 0 else 0.0
    for k in range(n):
        lo, hi = beat_span(k)
        w = max(1e-6, hi - lo)
        x = positions[k] / max(1e-6, end)
        rows[k] = [
            bnd[k], bnd[k + 1], bnd[k + 2],
            strong[k], strong[k + 1], strong[k + 2],
            ten[k + 1], ten[k + 2],
            cad[k], cad[k - 1] if k > 0 else 0.0,
            in_beat(subj, lo, hi),
            in_beat(charge, lo, hi) * (0.25 / w),
            in_beat(onsets, lo, hi) / w / 16.0,
            1.0 if (k < len(kinds) and kinds[k] == "db") else 0.0,
            1.0 if (k + 1 < len(kinds) and kinds[k + 1] == "db") else 0.0,
            x, 4 * x * (1 - x),
            1.0 if any(a <= positions[k] < b for a, b in seq_spans) else 0.0,
            seam[k + 1], seam[k + 2], seam[k],
            novb[k + 1], novb[k],
            1.0 if any(a <= positions[k] < b for a, b in ex_spans) else 0.0,
            in_beat(takes, lo, hi), take_next[k],
        ]
    return rows


def middle_mask(kinds, n):
    dbs = 0
    db_before = []
    for kd in kinds:
        if kd == "db":
            dbs += 1
        db_before.append(dbs)
    return np.array([0 <= k < len(db_before)
                     and db_before[k] > 2 and db_before[k] < dbs - 1
                     for k in range(n)])


def piece_data(otb, piece, tmp):
    pdir = os.path.join(ASAP, "Bach", *bwv_of(piece))
    ann = os.path.join(pdir, "midi_score_annotations.txt")
    positions = score_beat_positions(ann)
    kinds = beat_kinds(ann)
    ana = analyze(otb, piece)
    X = design(piece, ana, positions, kinds)
    n = X.shape[0]
    humans = []
    for f in sorted(os.listdir(pdir)):
        if f.endswith("_annotations.txt") and not f.startswith("midi_score"):
            h = zero_mean_log(local_tempo(
                perf_beat_times(os.path.join(pdir, f))))
            humans.append([x if x is not None else np.nan for x in h[:n]]
                          + [np.nan] * max(0, n - len(h)))
    H = np.array(humans) if humans else np.zeros((0, n))
    y = np.nanmean(H, axis=0) if len(humans) else np.full(n, np.nan)
    ir = compile_ir(otb, piece, KERN, tmp)
    tm = [(t["wn"], t["bpm"]) for t in ir["tempoMap"]]
    ours = zero_mean_log(local_tempo(
        [our_time_at(tm, w) for w in positions]))
    ours = np.array([x if x is not None else np.nan for x in ours[:n]])
    return X, y, H, ours, middle_mask(kinds, n)


def rs_against(H, pred, mask):
    out = []
    for h in H:
        ok = mask & ~np.isnan(h) & ~np.isnan(pred)
        if ok.sum() >= 8:
            r = pearson(list(pred[ok]), list(h[ok]))
            if not math.isnan(r):
                out.append(r)
    return out


def widen(X):
    """+/-4 beats of context per feature: temporal shapes in tree reach."""
    cols = [X]
    for lag in (1, 2, 3, 4):
        cols.append(np.roll(X, lag, axis=0))
        cols.append(np.roll(X, -lag, axis=0))
    return np.hstack(cols)


def main():
    root = subprocess.check_output(
        ["stack", "path", "--local-install-root"], cwd=ROOT,
        stderr=subprocess.DEVNULL, text=True).strip()
    otb = os.path.join(root, "bin", "otb")
    pieces = sorted(p[:-4] for p in os.listdir(KERN) if p.endswith(".krn"))
    pieces = [p for p in pieces
              if os.path.isdir(os.path.join(ASAP, "Bach", *bwv_of(p)))]
    train = [p for p in pieces if p.startswith("wtc1")]
    test = [p for p in pieces if p.startswith("wtc2")]

    data = {}
    with tempfile.TemporaryDirectory() as tmp:
        for p in pieces:
            data[p] = piece_data(otb, p, tmp)

    if "--gbm" in sys.argv:
        from sklearn.ensemble import GradientBoostingRegressor
        Xs, ys = [], []
        for p in train:
            X, y, _, _, mid = data[p]
            ok = mid & ~np.isnan(y)
            Xs.append(widen(X)[ok])
            ys.append(y[ok])
        model = GradientBoostingRegressor(
            n_estimators=300, max_depth=3, learning_rate=0.05,
            subsample=0.7, random_state=7)
        model.fit(np.vstack(Xs), np.concatenate(ys))
        for gname, group in [("train", train), ("TEST", test)]:
            mrs, crs = [], []
            for p in group:
                X, _, H, ours, mid = data[p]
                mrs += rs_against(H, model.predict(widen(X)), mid)
                crs += rs_against(H, ours, mid)
            print(f"gbm {gname}: middle r = {sum(mrs)/len(mrs):.3f} | "
                  f"compiler {sum(crs)/len(crs):.3f}")
        print("(human-human middle ceiling: 0.356)")
        return

    # ridge on TRAIN middle beats, mean-human target
    Xs, ys = [], []
    for p in train:
        X, y, _, _, mid = data[p]
        ok = mid & ~np.isnan(y)
        Xs.append(X[ok])
        ys.append(y[ok])
    Xtr = np.vstack(Xs)
    ytr = np.concatenate(ys)
    mu, sd = Xtr.mean(0), Xtr.std(0) + 1e-9
    Xn = (Xtr - mu) / sd
    A = Xn.T @ Xn + 1.0 * np.eye(Xn.shape[1])
    beta = np.linalg.solve(A, Xn.T @ (ytr - ytr.mean()))

    print(f"trained on {len(ytr)} middle beats of {len(train)} pieces\n")
    print("coefficients (standardized; % tempo per 1 sd of feature):")
    order = np.argsort(-np.abs(beta))
    for i in order:
        pct = (math.exp(beta[i]) - 1) * 100
        print(f"  {FEATURES[i]:<14} {pct:+6.2f}%")

    def report(name, group):
        model_rs, comp_rs = [], []
        for p in group:
            X, _, H, ours, mid = data[p]
            pred = ((X - mu) / sd) @ beta
            model_rs += rs_against(H, pred, mid)
            comp_rs += rs_against(H, ours, mid)
        m = sum(model_rs) / len(model_rs)
        c = sum(comp_rs) / len(comp_rs)
        print(f"{name}: structural model middle r = {m:.3f} | "
              f"compiler middle r = {c:.3f}")

    print()
    report("train (Book I)", train)
    report("TEST  (Book II)", test)
    print("\n(human-human middle ceiling: 0.356)")


if __name__ == "__main__":
    main()
