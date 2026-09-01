"""Micro-timing faces the humans: measure and fit the KTH timing rules.

The observable is each bridged note's SUB-BEAT onset deviation against
the performer's OWN beat grid (piecewise-linear through their beat
annotations) — beat-level tempo removed, leaving exactly the layer the
micro-timing rules shape: faster-uphill, double-duration, grace
realisation, melody lead, the final-chord roll, inegales.

HONESTY FRAMING. Human sub-beat deviations carry ~10 ms IQR of
unexplained jitter; several rules move less. The paired, phase- and
duration-matched, performance-clustered CONTRAST measurements are the
primary evidence; the deviation-r descent is secondary — a null there
reads "unresolvable at this noise floor", and a knob is only vetoed
when the contrast is also null or opposite-signed. jitter_ms is not
fitted at all: seeded noise correlates with nothing by construction,
so the descent would always drive it to 0 as a statement about the
objective, not about humans; --measure prints the empirical noise
floor it should be judged against by ear. Every fit trial pins
jitter_ms = 0 so otb's own noise cannot attenuate the objective.

Modes:
    python3 tools/timing_fit.py --measure           # primary evidence
    python3 tools/timing_fit.py --fit-defaults      # secondary
        [--knobs uphill,double_dur] [--from-prefit]
    python3 tools/timing_fit.py --dump-residuals F  # boundary-hunt feed

Eligibility everywhere: exact-pass bridge rows only (seg/fuzzy/orn are
re-anchored), non-ornament representatives, performances passing the
|median deviation| < 30 ms calibration gate (dropped ones counted).

Committed run (2026-09-01, 58-piece overlap, 166 performances;
--measure compiles against PREFIT so the contexts of vetoed rules
still fire — the numbers regenerate from HEAD; t statistics are
PIECE-clustered, since performances of one composition share notes
and often pianists):
CONTRASTS (primary): uphill -0.1 ms t=-0.2 over 56 pieces (null ->
vetoed); double-dur short half +2.6 ms LATE t=3.6 over 55 pieces with
direct 2:1 IOI ratio 2.041 vs notated 2.0 vs otb's prefit 1.807
(measured from the same triples; the rule alone gives (2-k)/(1+k) =
1.804 at k = 0.07) — the KTH softening is opposite-signed, humans
slightly overdot
(-> vetoed, overdot shape recorded); post-leap arrival +1.2 ms t=4.2
(unmodelled discovery); final-chord contrast +5.4 ms t=1.0 (weak) and
spread -2.1 ms/rank (roll_ms -> 0, harpsichord idiom kept per-piece);
inegales 0.931 over 57 pieces (no swing; inegal = 0 confirmed); grace
instances too few to measure (grace_ms stays at the literature 70);
noise floor: signed deviation-minus-performance-median IQR 15.6 ms
(jitter_ms = 3 conservative).
DESCENT (secondary): baseline train r 0.001 / test -0.005, "fitted"
0.021 / 0.003 — no held-out signal; unresolvable at this noise floor,
verdicts carried by the contrasts per the decision rule above.

License: GPL-2.0-or-later.
"""
import argparse
import math
import os
import statistics
import subprocess
import sys
import tempfile

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import asap_eval as ae  # noqa: E402
import note_align as na  # noqa: E402

ROOT = os.path.dirname(HERE)
KERN = os.path.join(ROOT, "corpus", "bach-wtc", "kern")

TIMING_KNOB_SECTIONS = {
    "uphill": "performance", "double_dur": "performance",
    "lead_ms": "performance", "roll_ms": "performance",
    "grace_ms": "ornaments",
    "jitter_ms": "performance",  # pinned to 0 in trials, never swept
}

PREFIT = {"uphill": 0.03, "double_dur": 0.07, "lead_ms": 2,
          "roll_ms": 12, "grace_ms": 70}

TIMING_KNOB_GRIDS = {
    "uphill": [0, 0.015, 0.03, 0.06, 0.12],
    "double_dur": [0, 0.035, 0.07, 0.14, 0.28],
    "lead_ms": [0, 2, 5, 10, 20],
    "roll_ms": [0, 6, 12, 24, 40],
    "grace_ms": [30, 50, 70, 100, 140],  # config forbids 0
}


def beat_grid(piece, scale=(1, 1)):
    """(wn positions, score-annotation identity) for interpolation.
    Positions are scale-corrected when the kern and xml editions
    notate at different value scales (the bridge's num/den)."""
    pdir = os.path.join(na.ASAP, "Bach", *ae.bwv_of(piece))
    wns = ae.score_beat_positions(
        os.path.join(pdir, "midi_score_annotations.txt"))
    num, den = scale
    return [w * num / den for w in wns]


def perf_devs(piece, ir, irn, perf, mpath, tempo_map):
    """Eligible rows with human_dev/otb_dev, plus counters."""
    rows, c = na.bridge(irn, na.parse_match(mpath))
    wns = beat_grid(piece, c["scale"])
    pdir = os.path.dirname(mpath)
    ts = ae.perf_beat_times(
        os.path.join(pdir, perf + "_annotations.txt"))
    n = min(len(wns), len(ts))
    if n < 3:
        return None, {"no_beats": 1}
    bw, bt = wns[:n], ts[:n]
    span = bw[-1] - bw[0]
    out = []
    extrapolated = 0
    for r in rows:
        if r["pass"] != "exact" or r["is_orn"]:
            continue
        w = r["wn"]
        if w < bw[0] - span / max(1, n - 1) or \
                w > bw[-1] + span / max(1, n - 1):
            continue  # more than ~a beat outside the annotated span
        if w < bw[0] or w > bw[-1]:
            extrapolated += 1
        hdev = r["human_on_s"] - na.interp_time(bw, bt, w)
        odev = r["otb_on_s"] - ae.our_time_at(tempo_map, w)
        r = dict(r)
        r["human_dev"] = hdev
        r["otb_dev"] = odev
        out.append(r)
    if not out:
        return None, {"no_rows": 1}
    med = statistics.median(r["human_dev"] for r in out)
    if abs(med) > 0.030:
        return None, {"calibration_dropped": 1}
    return out, {"extrapolated": extrapolated}


def piece_perf_rows(otb, piece, tmp, config=None):
    """Yield (performer, rows) per calibrated performance."""
    ir = ae.compile_ir(otb, piece, KERN, tmp, config)
    irn = na.load_ir_notes(ir)
    tempo_map = [(t["wn"], t["bpm"]) for t in ir["tempoMap"]]
    dropped = 0
    for perf, mpath in na.piece_performances(piece):
        rows, c = perf_devs(piece, ir, irn, perf, mpath, tempo_map)
        if rows is None:
            dropped += c.get("calibration_dropped", 0)
            continue
        yield perf, rows
    if dropped:
        print(f"  ({piece}: {dropped} performance(s) failed the "
              f"calibration gate)", file=sys.stderr)


# ---------------------------------------------------------------------
# --measure: contrasts (primary evidence) and direct readouts

def phase_bucket(wn):
    return int((wn * 4) % 4)


def dur_bucket(dur_wn):
    if dur_wn is None:
        return 0
    for i, lim in enumerate((1 / 32, 1 / 16, 1 / 8, 1 / 4)):
        if dur_wn <= lim:
            return i
    return 4


def contrast(per_piece, in_ctx):
    """Per-performance median(in) - median(matched out), aggregated to
    PIECE means, then t across pieces: performances of one composition
    share both the notes and often the pianist, so clustering at the
    performance level overstates certainty (the double-duration t
    dropped from 6.6 to ~3.6 under piece clustering)."""
    by_piece = {}
    n_in = n_perfs = 0
    for piece, rows in per_piece:
        ins = [r for r in rows if in_ctx(r)]
        if len(ins) < 3:
            continue
        keys = {(dur_bucket(r.get("otb_dur_wn")), phase_bucket(r["wn"]))
                for r in ins}
        outs = [r for r in rows if not in_ctx(r)
                and (dur_bucket(r.get("otb_dur_wn")),
                     phase_bucket(r["wn"])) in keys]
        if len(outs) < 3:
            continue
        eff = (statistics.median(r["human_dev"] for r in ins)
               - statistics.median(r["human_dev"] for r in outs))
        by_piece.setdefault(piece, []).append(eff)
        n_in += len(ins)
        n_perfs += 1
    effs = [sum(v) / len(v) for v in by_piece.values()]
    if len(effs) < 3:
        return None
    mean = sum(effs) / len(effs)
    sd = math.sqrt(sum((e - mean) ** 2 for e in effs)
                   / max(1, len(effs) - 1))
    t = mean / (sd / math.sqrt(len(effs))) if sd > 0 else float("inf")
    return {"ms": 1000 * mean, "t": t, "n_pieces": len(effs),
            "n_perfs": n_perfs, "n_notes": n_in}


CONTEXTS = [
    ("uphill (onset-shifted)",
     lambda r: "onset" in r["rules"].get("faster-uphill", "")),
    ("double-dur short half",
     lambda r: "onset" in r["rules"].get("double-duration", "")),
    ("double-dur long half",
     lambda r: "double-duration" in r["rules"]
     and "onset" not in r["rules"]["double-duration"]),
    ("grace-delayed main",
     lambda r: r["rules"].get("grace", "").startswith("dur x")),
    ("pre-leap (leap-pause)",
     lambda r: "leap-pause" in r["rules"]),
    ("post-leap arrival (unmodelled)",
     lambda r: "leap-arrival" in r["rules"]),
    ("final chord",
     lambda r: r["is_final"]),
]


def measure(otb, args, pieces, tmp):
    # measurement probes the PRE-FIT hand model: the verdicts switched
    # uphill/double_dur off in the shipped config, which would erase the
    # very contexts being measured — the committed contrasts regenerate
    # from HEAD only against PREFIT
    cfg = ae.with_knobs(args.config, PREFIT, tmp,
                        sections=TIMING_KNOB_SECTIONS)
    per_piece = []
    graces, dd_ratios, dd_otb, roll_incs = [], [], [], []
    inegal_by_piece = {}
    for piece in pieces:
        for perf, rows in piece_perf_rows(otb, piece, tmp, cfg):
            per_piece.append((piece, rows))
            by_ch = {}
            for r in sorted(rows, key=lambda r: r["wn"]):
                by_ch.setdefault(r["ch"], []).append(r)
            # grace sounding lengths (the human's own performed graces)
            for r in rows:
                if r["snote_grace"] and \
                        r["rules"].get("grace", "").startswith("realised"):
                    graces.append((1000 * (r["human_off_s"]
                                           - r["human_on_s"]),
                                   1000 * r["otb_dur_s"]))
            for lane in by_ch.values():
                # notated-2:1 candidates measured from srcWn spacing
                for a, b, c2 in zip(lane, lane[1:], lane[2:]):
                    la = b["wn"] - a["wn"]
                    lb = c2["wn"] - b["wn"]
                    if lb > 0 and abs(la / lb - 2.0) < 1e-6 \
                            and lb <= 1 / 4:
                        ha = b["human_on_s"] - a["human_on_s"]
                        hb = c2["human_on_s"] - b["human_on_s"]
                        oa = b["otb_on_s"] - a["otb_on_s"]
                        ob = c2["otb_on_s"] - b["otb_on_s"]
                        if hb > 0.01 and ha > 0.01:
                            dd_ratios.append(ha / hb)
                        if ob > 0.01 and oa > 0.01:
                            dd_otb.append(oa / ob)
                # inegales candidates: conjunct equal pairs <= eighth,
                # contiguous, long half on the even multiple
                # (inegalLane's own preconditions, in python)
                for j in range(len(lane) - 2):
                    a, b, nxt = lane[j], lane[j + 1], lane[j + 2]
                    d = b["wn"] - a["wn"]
                    if not (0 < d <= 1 / 8):
                        continue
                    if abs((nxt["wn"] - b["wn"]) - d) > 1e-9:
                        continue
                    if abs(a["pitch"] - b["pitch"]) > 2:
                        continue
                    if int(round(a["wn"] / d)) % 2 != 0:
                        continue
                    i1 = b["human_on_s"] - a["human_on_s"]
                    i2 = nxt["human_on_s"] - b["human_on_s"]
                    if i1 > 0.01 and i2 > 0.01:
                        inegal_by_piece.setdefault(piece, []).append(i1 / i2)
            # final-chord roll: onset increments between pitch ranks
            fin = sorted((r for r in rows if r["is_final"]),
                         key=lambda r: r["pitch"])
            for a, b in zip(fin, fin[1:]):
                roll_incs.append(1000 * (b["human_on_s"] - a["human_on_s"]))

    print("== contrasts (human deviation in-context vs matched "
          "baseline; piece-clustered) ==")
    print(f"{'context':<32} {'effect ms':>10} {'t':>7} {'pieces':>7} "
          f"{'perfs':>6} {'notes':>7}")
    for name, ctx in CONTEXTS:
        c = contrast(per_piece, ctx)
        if c is None:
            print(f"{name:<32} {'—':>10}   (too few instances)")
        else:
            print(f"{name:<32} {c['ms']:>+10.1f} {c['t']:>7.1f} "
                  f"{c['n_pieces']:>7} {c['n_perfs']:>6} "
                  f"{c['n_notes']:>7}")

    print("\n== direct readouts ==")
    if graces:
        hs = sorted(g[0] for g in graces)
        os_ = sorted(g[1] for g in graces)
        print(f"grace sounding length: human median "
              f"{hs[len(hs) // 2]:.0f} ms (n={len(hs)}, IQR "
              f"[{hs[len(hs) // 4]:.0f}, {hs[3 * len(hs) // 4]:.0f}]) "
              f"vs otb realised {os_[len(os_) // 2]:.0f} ms "
              f"(grace_ms = 70)")
    if dd_ratios:
        ds = sorted(dd_ratios)
        os2 = sorted(dd_otb)
        otb_med = os2[len(os2) // 2] if os2 else float("nan")
        # otb's comparator is MEASURED from the same triples in the
        # PREFIT render, not embedded (the rule alone gives
        # (2-k)/(1+k) = 1.804 at k = 0.07; interactions move it)
        print(f"2:1 pair IOI ratio: human median "
              f"{ds[len(ds) // 2]:.3f} (n={len(ds)}) — notated 2.0, "
              f"otb prefit {otb_med:.3f}")
    if roll_incs:
        rs = sorted(roll_incs)
        print(f"final-chord spread: human median "
              f"{rs[len(rs) // 2]:+.1f} ms/rank (n={len(rs)}) "
              f"vs roll_ms = 12")
    ratios = [statistics.median(v) for v in inegal_by_piece.values()
              if len(v) >= 8]
    if ratios:
        rr = sorted(ratios)
        print(f"inegales long:short IOI ratio, per-piece medians: "
              f"median {rr[len(rr) // 2]:.3f} over {len(rr)} pieces "
              f"(1.0 = no swing; config inegal = 0)")
        for pc, v in sorted(inegal_by_piece.items()):
            if len(v) >= 8 and pc == "wtc1p05":
                vv = sorted(v)
                print(f"  wtc1p05 (per-piece inegal 0.18): human "
                      f"{vv[len(vv) // 2]:.3f} over {len(v)} pairs")
    # the empirical noise floor jitter_ms should be judged against
    resid = []
    for _piece, rows in per_piece:
        med = statistics.median(r["human_dev"] for r in rows)
        resid.extend(r["human_dev"] - med for r in rows)
    resid.sort()
    q1 = resid[len(resid) // 4]
    q3 = resid[3 * len(resid) // 4]
    print(f"\nnoise floor: signed deviation-minus-performance-median "
          f"IQR {1000 * (q3 - q1):.1f} ms "
          f"(jitter_ms = 3 is conservative)")


# ---------------------------------------------------------------------
# --fit-defaults: the secondary evidence

def mean_dev_r(otb, pieces, config, tmp):
    rs = []
    for piece in pieces:
        prs = []
        for _perf, rows in piece_perf_rows(otb, piece, tmp, config):
            if len(rows) < 30:
                continue
            r = ae.pearson([x["otb_dev"] for x in rows],
                           [x["human_dev"] for x in rows])
            if not math.isnan(r):
                prs.append(r)
        if prs:
            rs.append(statistics.median(prs))
    return sum(rs) / len(rs) if rs else float("nan")


def fit_defaults(otb, args, pieces, tmp):
    grids = TIMING_KNOB_GRIDS
    if args.knobs:
        wanted = args.knobs.split(",")
        unknown = [k for k in wanted if k not in grids]
        if unknown:
            sys.exit(f"unknown knobs: {unknown}")
        grids = {k: TIMING_KNOB_GRIDS[k] for k in wanted}
    train = [p for p in pieces if p.startswith("wtc1")]
    test = [p for p in pieces if p.startswith("wtc2")]
    print(f"train: {len(train)} Book I | test: {len(test)} Book II "
          f"(held out)")

    def cfg_for(knobs):
        trial = dict(knobs)
        trial["jitter_ms"] = 0  # otb's own noise must not attenuate r
        if args.from_prefit:
            trial = {**PREFIT, **trial}
        return ae.with_knobs(args.config, trial, tmp,
                             sections=TIMING_KNOB_SECTIONS)

    base_tr = mean_dev_r(otb, train, cfg_for({}), tmp)
    base_te = mean_dev_r(otb, test, cfg_for({}), tmp)
    print(f"defaults: train r = {base_tr:.3f}, test r = {base_te:.3f}")
    knobs = {}
    for sweep in range(2):
        for knob, grid in grids.items():
            best_v, best_r = None, -2
            for v in grid:
                trial = dict(knobs)
                trial[knob] = v
                r = mean_dev_r(otb, train, cfg_for(trial), tmp)
                if not math.isnan(r) and r > best_r:
                    best_v, best_r = v, r
            if best_v is None:
                knobs.pop(knob, None)
                print(f"  sweep {sweep + 1} {knob:<12} -> all NaN")
                continue
            knobs[knob] = best_v
            print(f"  sweep {sweep + 1} {knob:<12} -> {best_v}"
                  f"   train r = {best_r:.3f}")
    final_tr = mean_dev_r(otb, train, cfg_for(knobs), tmp)
    final_te = mean_dev_r(otb, test, cfg_for(knobs), tmp)
    print(f"\nfitted: train r = {final_tr:.3f} (was {base_tr:.3f}) | "
          f"TEST r = {final_te:.3f} (was {base_te:.3f})")
    print("\nfitted values (merge with the --measure verdicts):")
    for k, v in sorted(knobs.items()):
        print(f"{TIMING_KNOB_SECTIONS[k]}.{k} = {v}")


def dump_residuals(otb, pieces, tmp, path):
    n = 0
    with open(path, "w") as f:
        f.write("piece\tperformer\tsrc_wn\thuman_dev_s\totb_dev_s\t"
                "ch\tpitch\trules\n")
        for piece in pieces:
            for perf, rows in piece_perf_rows(otb, piece, tmp):
                for r in rows:
                    f.write(f"{piece}\t{perf}\t{r['wn']:.6f}\t"
                            f"{r['human_dev']:.6f}\t{r['otb_dev']:.6f}\t"
                            f"{r['ch']}\t{r['pitch']}\t"
                            f"{';'.join(sorted(r['rules']))}\n")
                    n += 1
    print(f"{n} residual rows -> {path}")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("pieces", nargs="*")
    ap.add_argument("--config",
                    default=os.path.join(ROOT, "config", "default.toml"))
    ap.add_argument("--otb", default=None)
    ap.add_argument("--measure", action="store_true")
    ap.add_argument("--fit-defaults", action="store_true")
    ap.add_argument("--knobs", metavar="K1,K2")
    ap.add_argument("--from-prefit", action="store_true")
    ap.add_argument("--dump-residuals", metavar="FILE")
    args = ap.parse_args()

    otb = args.otb
    if not otb:
        root = subprocess.check_output(
            ["stack", "path", "--local-install-root"], cwd=ROOT,
            stderr=subprocess.DEVNULL, text=True).strip()
        otb = os.path.join(root, "bin", "otb")

    pieces = args.pieces or sorted(
        p[:-4] for p in os.listdir(KERN) if p.endswith(".krn"))
    pieces = [p for p in pieces if na.piece_performances(p)]
    if not pieces:
        sys.exit("no WTC/ASAP overlap (is corpus/asap present?)")

    with tempfile.TemporaryDirectory() as tmp:
        if args.measure:
            measure(otb, args, pieces, tmp)
        elif args.fit_defaults:
            fit_defaults(otb, args, pieces, tmp)
        elif args.dump_residuals:
            dump_residuals(otb, pieces, tmp, args.dump_residuals)
        else:
            sys.exit(__doc__)


if __name__ == "__main__":
    main()
