"""Fit and verify OTB.TempoGiusto against ASAP human mean tempos.

The regeneration path for the constants committed in
src/OTB/TempoGiusto.hs (2026-09-01):

    stack runghc tools/GiustoFeatures.hs > /tmp/feats.txt
    python tools/giusto_fit.py /tmp/feats.txt            # refit
    python tools/giusto_fit.py /tmp/feats.txt --verify   # shipped formula

Discipline matches asap_eval --fit-defaults: log-linear ridge on the
Book I overlap, Book II held out; the reported test numbers are the
honest ones. The straw man is the old constant-72 fallback. Human
tempo per performance = 240 * score-wn-span / performance-seconds,
median across a piece's performers.

Committed run (58-piece overlap): fit train n=28 r=0.610, TEST n=30
r=0.373, |log err| 0.352 vs 0.421 const-72; shipped-formula verify
over all 58: r=0.466, |log err| 0.290 vs 0.375.
"""
import math
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import asap_eval as ae  # noqa: E402

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
ASAP = os.path.join(ROOT, "corpus", "asap")

NAMES = ["const", "fast", "vfast", "broad", "den8", "den2", "compound"]


def feats(fast, vfast, broad, num, den):
    return [1.0, fast, vfast, broad,
            1.0 if den >= 8 else 0.0,
            1.0 if den <= 2 else 0.0,
            1.0 if (num > 3 and num % 3 == 0) else 0.0]


def human_mean_bpm(piece):
    """Median across performers of 240 * score-wn-span / perf-seconds."""
    pdir = os.path.join(ASAP, "Bach", *ae.bwv_of(piece))
    score_ann = os.path.join(pdir, "midi_score_annotations.txt")
    if not os.path.exists(score_ann):
        return None
    wns = ae.score_beat_positions(score_ann)
    if len(wns) < 3:
        return None
    bpms = []
    for f in sorted(os.listdir(pdir)):
        if not f.endswith("_annotations.txt") or f.startswith("midi_score"):
            continue
        ts = ae.perf_beat_times(os.path.join(pdir, f))
        n = min(len(ts), len(wns))
        if n < 3:
            continue
        dt, dw = ts[n - 1] - ts[0], wns[n - 1] - wns[0]
        if dt > 0 and dw > 0:
            bpms.append(240.0 * dw / dt)
    if not bpms:
        return None
    bpms.sort()
    return bpms[len(bpms) // 2]


def solve(rows_x, rows_y, lam=1e-3):
    """Ridge least squares by Gauss-Jordan (no numpy dependency)."""
    k = len(rows_x[0])
    a = [[sum(x[i] * x[j] for x in rows_x) + (lam if i == j else 0)
          for j in range(k)] for i in range(k)]
    b = [sum(x[i] * y for x, y in zip(rows_x, rows_y)) for i in range(k)]
    for col in range(k):
        piv = max(range(col, k), key=lambda r: abs(a[r][col]))
        a[col], a[piv] = a[piv], a[col]
        b[col], b[piv] = b[piv], b[col]
        d = a[col][col]
        a[col] = [v / d for v in a[col]]
        b[col] /= d
        for r in range(k):
            if r != col and a[r][col]:
                f = a[r][col]
                a[r] = [v - f * w for v, w in zip(a[r], a[col])]
                b[r] -= f * b[col]
    return b


def load(path):
    data = []
    for line in open(path):
        p, *vs = line.split()
        h = human_mean_bpm(p)
        if h is None:
            continue
        fast, vfast, broad = map(float, vs[:3])
        num, den = int(vs[3]), int(vs[4])
        pred = float(vs[5])
        data.append((p, feats(fast, vfast, broad, num, den),
                     math.log(h), pred, h))
    if not data:
        sys.exit("no WTC/ASAP overlap found (is corpus/asap present?)")
    return data


def logerr(pairs):
    return sum(abs(a - b) for a, b in pairs) / len(pairs)


def main():
    if len(sys.argv) < 2:
        sys.exit(__doc__)
    data = load(sys.argv[1])

    if "--verify" in sys.argv:
        # the SHIPPED formula's predictions vs the humans, all pieces
        xs = [math.log(pred) for _, _, _, pred, _ in data]
        ys = [y for _, _, y, _, _ in data]
        print(f"{len(data)} pieces | shipped-formula log-log r = "
              f"{ae.pearson(xs, ys):.3f}")
        print(f"mean |log err|: giusto "
              f"{logerr(list(zip(xs, ys))):.3f} vs constant-72 "
              f"{logerr([(math.log(72), y) for y in ys]):.3f}")
        return

    train = [(x, y) for p, x, y, _, _ in data if p.startswith("wtc1")]
    test = [(x, y) for p, x, y, _, _ in data if p.startswith("wtc2")]
    w = solve([x for x, _ in train], [y for _, y in train])

    def stats(rows):
        preds = [sum(wi * xi for wi, xi in zip(w, x)) for x, _ in rows]
        ys = [y for _, y in rows]
        return (ae.pearson(preds, ys), logerr(list(zip(preds, ys))),
                logerr([(math.log(72), y) for y in ys]))

    print("weights (log-space) — merge into src/OTB/TempoGiusto.hs:")
    for n, wi in zip(NAMES, w):
        print(f"  {n:<9} {wi:+.4f}")
    for label, rows in [("train", train), ("TEST", test)]:
        r, e, e72 = stats(rows)
        print(f"{label}: n={len(rows)} r={r:.3f} |log err|={e:.3f} "
              f"(const-72: {e72:.3f})")


if __name__ == "__main__":
    main()
