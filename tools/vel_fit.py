"""Fit otb's velocity rule stack against ASAP human key velocities.

The first note-level fit of the rule stack: the bridge (note_align.py)
pairs every performed keystroke with otb's note for the same score
event, so the entire velocity family — Sloboda meter weights, the Todd
arch, high-loud, dissonance/melodic/harmonic charge, subject entries,
dialogue, echo, suspension softening — can face the humans the way the
agogic knobs did.

Metric: per performance, the Pearson r between otb's velocities and the
human's over bridged pairs (>= 30 pairs, else NaN); per piece the
MEDIAN across performers; the objective is the mean across pieces.
Pearson r is affine-invariant, so vel_base (and any overall scale) is
deliberately not fitted — the metric cannot see it, and that is a
feature: level is the instrument's business, shape is the
interpretation's.

Modes:
    python3 tools/vel_fit.py                    # scoreboard, all pieces
    python3 tools/vel_fit.py --fit-defaults     # coordinate descent
        [--knobs vel_bar,dis_vel]               # targeted refit
    python3 tools/vel_fit.py --measure          # direct readouts:
        melody-lead distribution vs lead_ms, human gate distributions
        by otb's own articulation labels (KEY-RELEASE gates, no pedal
        model: trust the contrast between labels, not absolute levels)
    python3 tools/vel_fit.py --exclude-orn ...  # sensitivity: drop
        ornament-realisation rows (trill keystrokes)

Discipline: train Book I (wtc1*), Book II held out; every grid carries
an OFF value; a knob that only helps train keeps its default and the
veto is recorded. Regeneration: run the commands above from the repo
root with corpus/asap and corpus/bach-wtc present and otb built.

Committed run (2026-09-01, 58-piece overlap): baseline train r = 0.164,
test 0.161; fitted train 0.408, TEST 0.421. Winners: vel_highloud 0.8
(the dominant driver), vel_beat 3 and subject_vel 5 (at their hand
values), dialogue_vel 2. Vetoed beside register: vel_bar, vel_halfbar,
vel_arch, dis_vel, sus_soft, mel_charge, harm_charge, dialogue_yield,
seq_echo. --measure committed lead_ms 20 -> 2 and repeated 0.60 -> 0.45.

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

VEL_KNOB_SECTIONS = {
    "vel_bar": "dynamics", "vel_halfbar": "dynamics",
    "vel_beat": "dynamics", "vel_arch": "dynamics",
    "vel_highloud": "dynamics",
    "dis_vel": "dissonance", "sus_soft": "dissonance",
    "mel_charge": "performance", "harm_charge": "performance",
    "subject_vel": "performance", "dialogue_vel": "performance",
    "dialogue_yield": "performance", "seq_echo": "performance",
}

VEL_KNOB_GRIDS = {
    "vel_bar": [0, 4, 8, 12, 18],
    "vel_halfbar": [0, 3, 6, 9],
    "vel_beat": [0, 1.5, 3, 6],
    "vel_arch": [0, 4, 8, 16],
    "vel_highloud": [0, 0.12, 0.25, 0.5, 0.8],
    "dis_vel": [0, 5, 10, 20],
    "sus_soft": [0, 2, 4, 8],
    "mel_charge": [0, 0.2, 0.4, 0.8],
    "harm_charge": [0, 0.15, 0.3, 0.6],
    "subject_vel": [0, 2.5, 5, 10],
    "dialogue_vel": [0, 2, 4, 8],
    "dialogue_yield": [0, 1, 2, 4],
    "seq_echo": [0, 1, 2, 4],
}


def piece_r(ir, piece, exclude_orn=False):
    """Median across performers of per-performance velocity r."""
    irn = na.load_ir_notes(ir)
    rs = []
    for _perf, mpath in na.piece_performances(piece):
        rows, _c = na.bridge(irn, na.parse_match(mpath))
        if exclude_orn:
            rows = [r for r in rows if not r["is_orn"]]
        if len(rows) < 30:
            continue
        rs.append(ae.pearson([r["otb_vel"] for r in rows],
                             [r["human_vel"] for r in rows]))
    rs = [r for r in rs if not math.isnan(r)]
    return statistics.median(rs) if rs else float("nan"), len(rs)


def mean_piece_r(otb, pieces, config, tmp, exclude_orn=False):
    rs = []
    for piece in pieces:
        ir = ae.compile_ir(otb, piece, KERN, tmp, config)
        r, _n = piece_r(ir, piece, exclude_orn)
        if not math.isnan(r):
            rs.append(r)
    return sum(rs) / len(rs) if rs else float("nan")


def fit_defaults(otb, args, pieces, tmp):
    grids = VEL_KNOB_GRIDS
    if args.knobs:
        wanted = args.knobs.split(",")
        unknown = [k for k in wanted if k not in grids]
        if unknown:
            sys.exit(f"unknown knobs: {unknown}")
        grids = {k: VEL_KNOB_GRIDS[k] for k in wanted}
    train = [p for p in pieces if p.startswith("wtc1")]
    test = [p for p in pieces if p.startswith("wtc2")]
    print(f"train: {len(train)} Book I | test: {len(test)} Book II (held out)")
    base_tr = mean_piece_r(otb, train, args.config, tmp, args.exclude_orn)
    base_te = mean_piece_r(otb, test, args.config, tmp, args.exclude_orn)
    print(f"defaults: train r = {base_tr:.3f}, test r = {base_te:.3f}")
    knobs = {}
    for sweep in range(2):
        for knob, grid in grids.items():
            best_v, best_r = None, -2
            for v in grid:
                trial = dict(knobs)
                trial[knob] = v
                cfg = ae.with_knobs(args.config, trial, tmp,
                                    sections=VEL_KNOB_SECTIONS)
                r = mean_piece_r(otb, train, cfg, tmp, args.exclude_orn)
                if not math.isnan(r) and r > best_r:
                    best_v, best_r = v, r
            if best_v is None:
                knobs.pop(knob, None)
                print(f"  sweep {sweep + 1} {knob:<14} -> all NaN, "
                      f"keeping the config default")
                continue
            knobs[knob] = best_v
            print(f"  sweep {sweep + 1} {knob:<14} -> {best_v}"
                  f"   train r = {best_r:.3f}")
    cfg = ae.with_knobs(args.config, knobs, tmp, sections=VEL_KNOB_SECTIONS)
    final_tr = mean_piece_r(otb, train, cfg, tmp, args.exclude_orn)
    final_te = mean_piece_r(otb, test, cfg, tmp, args.exclude_orn)
    print(f"\nfitted: train r = {final_tr:.3f} (was {base_tr:.3f}) | "
          f"TEST r = {final_te:.3f} (was {base_te:.3f})")
    print("\nfitted values (merge into config/default.toml):")
    by_sec = {}
    for k, v in knobs.items():
        by_sec.setdefault(VEL_KNOB_SECTIONS[k], []).append((k, v))
    for sec, kvs in sorted(by_sec.items()):
        print(f"[{sec}]")
        for k, v in sorted(kvs):
            print(f"{k} = {v}")


def measure(otb, pieces, tmp):
    leads_all, leads_marked = [], []
    gates = {}
    for piece in pieces:
        ir = ae.compile_ir(otb, piece, KERN, tmp)
        irn = na.load_ir_notes(ir)
        for _perf, mpath in na.piece_performances(piece):
            rows, _c = na.bridge(irn, na.parse_match(mpath))
            exact = [r for r in rows if r["pass"] == "exact"]
            # melody lead: at each score onset with notes in >1 track,
            # the top note's onset vs the mean of the others (ms;
            # negative = the melody sounds early)
            by_on = {}
            for r in exact:
                by_on.setdefault(round(r["wn"] * 1920), []).append(r)
            for _on, grp in by_on.items():
                if len(grp) < 2:
                    continue
                top = max(grp, key=lambda r: r["pitch"])
                rest = [r for r in grp if r is not top]
                if not rest:
                    continue
                lead = 1000 * (top["human_on_s"]
                               - sum(r["human_on_s"] for r in rest)
                               / len(rest))
                leads_all.append(lead)
            # gates: human sounding fraction of the IOI to the next
            # matched note in the same xml voice
            by_voice = {}
            for r in exact:
                by_voice.setdefault(r["voice"], []).append(r)
            for _v, vr in by_voice.items():
                vr.sort(key=lambda r: r["human_on_s"])
                for a, b in zip(vr, vr[1:]):
                    ioi = b["human_on_s"] - a["human_on_s"]
                    if ioi <= 0 or ioi > 2 or a["is_orn"]:
                        continue
                    g = (a["human_off_s"] - a["human_on_s"]) / ioi
                    if a["gate_label"] and 0 < g < 3:
                        gates.setdefault(a["gate_label"], []).append(
                            (min(g, 2.0), a["gate_pct"]))
    if leads_all:
        q = statistics.quantiles(leads_all, n=4)
        print(f"melody lead (ms, negative = early): n={len(leads_all)} "
              f"median={statistics.median(leads_all):+.1f} "
              f"IQR=[{q[0]:+.1f}, {q[2]:+.1f}]  (config lead_ms = 20)")
    print(f"\n{'label':<26} {'n':>7} {'human med':>10} {'IQR':>16} "
          f"{'configured':>11}")
    for label in sorted(gates, key=lambda k: -len(gates[k])):
        gs = sorted(g for g, _ in gates[label])
        n = len(gs)
        med = gs[n // 2]
        q1, q3 = gs[n // 4], gs[(3 * n) // 4]
        conf = gates[label][0][1]
        print(f"{label:<26} {n:>7} {med:>10.2f} "
              f"[{q1:>6.2f}, {q3:>5.2f}] {conf:>10}%")
    print("\nNOTE: key-release gates, no pedal model — read the CONTRAST"
          "\nbetween labels, not absolute levels.")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("pieces", nargs="*")
    ap.add_argument("--config",
                    default=os.path.join(ROOT, "config", "default.toml"))
    ap.add_argument("--otb", default=None)
    ap.add_argument("--fit-defaults", action="store_true")
    ap.add_argument("--knobs", metavar="K1,K2")
    ap.add_argument("--measure", action="store_true")
    ap.add_argument("--exclude-orn", action="store_true")
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
        if args.fit_defaults:
            fit_defaults(otb, args, pieces, tmp)
            return
        if args.measure:
            measure(otb, pieces, tmp)
            return
        print(f"{'piece':<10} {'perfs':>5} {'median r':>9}")
        rs = []
        for piece in pieces:
            ir = ae.compile_ir(otb, piece, KERN, tmp, args.config)
            r, n = piece_r(ir, piece, args.exclude_orn)
            if not math.isnan(r):
                rs.append(r)
            print(f"{piece:<10} {n:>5} {r:>9.3f}")
        print(f"\n{len(rs)} pieces, grand mean velocity r = "
              f"{sum(rs) / len(rs):.3f}")


if __name__ == "__main__":
    main()
