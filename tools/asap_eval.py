#!/usr/bin/env python3
"""Evaluate otb's interpretation against human performances (ASAP).

The ASAP dataset (Foscarin et al. 2020, CPJKU/asap-dataset) aligns
scores to human piano performances with per-beat annotations — and a
large chunk of it is the WTC itself. This tool compares the compiler's
tempo curve, beat for beat, against every human performance of the
same piece and reports the Pearson correlation of local tempo.

This is the honest scoreboard for the rule stack: "sounds good" as a
number that can regress. A correlation ~0 means our timing story is
orthogonal to human practice; the sign and per-piece spread say where
the rules disagree with players.

Beat positions come from ASAP's own score annotations (db/b markers +
meter tags), so meter changes are handled by the dataset's ground
truth, not re-derived. Our beat times come from integrating the IR's
tempoMap (piecewise-constant, same convention as Emit.Json).

  tools/asap_eval.py                     # all WTC pieces present
  tools/asap_eval.py wtc1f01 wtc1p01     # a selection
  tools/asap_eval.py --fit wtc1f01       # grid the expression knob

STATUS: the evaluation is dependable (exact beat ground truth, ~166
performances); the FITS are research-grade — performer profiles are
in-sample over at most four pieces and say "this stance correlates
with this pianist here", not "this is how they play".

Requires: corpus/asap (sparse clone of CPJKU/asap-dataset, Bach/),
a built otb (stack build), and the kern corpus.

License: GPL-2.0-or-later.
"""

import argparse
import json
import math
import os
import subprocess
import sys
import tempfile

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(HERE)


def bwv_of(piece):
    """wtc1f01 -> (Fugue, bwv_846); wtc2p12 -> (Prelude, bwv_881)."""
    book = int(piece[3])
    kind = {"p": "Prelude", "f": "Fugue"}[piece[4]]
    num = int(piece[5:7])
    bwv = (845 if book == 1 else 869) + num
    return kind, f"bwv_{bwv}"


def score_beat_positions(path):
    """Whole-note position of every annotated beat, from the file's own
    score-MIDI times.

    ASAP's score MIDIs are deadpan constant-tempo renders, so score
    position is PROPORTIONAL to the annotation's first time column —
    the one mapping that survives pickups (20 of the 58 WTC files
    open off the downbeat; their first annotated beat is not at zero)
    and partial final bars. The whole-notes-per-second factor comes
    from full downbeat-to-downbeat bars measured against their stated
    meter (median across the piece; these files are constant-tempo, so
    the spread is zero).
    """
    rows = []
    cur_meter = None
    with open(path) as f:
        for line in f:
            parts = line.rstrip("\n").split("\t")
            if len(parts) < 3:
                continue
            bits = parts[2].split(",")
            if len(bits) >= 2 and "/" in bits[1]:
                n, d = bits[1].split("/")
                cur_meter = (int(n), int(d))
            rows.append((float(parts[0]), bits[0], cur_meter))
    # wn/second from each full bar (db at t1, next db at t2, meter m)
    factors = []
    dbs = [(t, m) for t, k, m in rows if k == "db"]
    for (t1, m), (t2, _) in zip(dbs, dbs[1:]):
        if m and t2 > t1:
            factors.append((m[0] / m[1]) / (t2 - t1))
    if not factors:
        # single-bar oddity: fall back to beat spacing as quarters
        ts = [t for t, _, _ in rows]
        gaps = [b - a for a, b in zip(ts, ts[1:]) if b > a]
        factors = [0.25 / (sorted(gaps)[len(gaps) // 2])] if gaps else [0.5]
    factors.sort()
    wn_per_s = factors[len(factors) // 2]
    return [t * wn_per_s for t, _, _ in rows]


def perf_beat_times(path):
    times = []
    with open(path) as f:
        for line in f:
            parts = line.split("\t")
            if parts and parts[0]:
                times.append(float(parts[0]))
    return times


def our_time_at(tempo_map, wn):
    """Integrate a piecewise-constant (wn, bpm) map to seconds at wn."""
    t = 0.0
    for (w0, bpm0), (w1, _) in zip(tempo_map, tempo_map[1:]):
        if wn <= w0:
            break
        seg_end = min(wn, w1)
        t += (seg_end - w0) * 240.0 / bpm0
        if wn <= w1:
            return t
    w_last, bpm_last = tempo_map[-1]
    if wn > w_last:
        t += (wn - w_last) * 240.0 / bpm_last
    return t


def pearson(xs, ys):
    n = len(xs)
    if n < 3:
        return float("nan")
    mx, my = sum(xs) / n, sum(ys) / n
    num = sum((x - mx) * (y - my) for x, y in zip(xs, ys))
    den = math.sqrt(sum((x - mx) ** 2 for x in xs)) * math.sqrt(
        sum((y - my) ** 2 for y in ys))
    return num / den if den > 0 else float("nan")


def local_tempo(beat_times):
    """Inverse inter-beat intervals, one per beat transition."""
    out = []
    for a, b in zip(beat_times, beat_times[1:]):
        ibi = b - a
        out.append(1.0 / ibi if ibi > 1e-4 else float("nan"))
    return out


def compile_ir(otb, piece, kern_dir, out_dir, config=None):
    krn = os.path.join(kern_dir, piece + ".krn")
    ir = os.path.join(out_dir, piece + ".json")
    cmd = [otb, "compile", krn, "-o", os.devnull, "--emit-json", ir]
    if config:
        cmd += ["--config", config]
    r = subprocess.run(cmd, capture_output=True, text=True)
    if r.returncode != 0:
        raise RuntimeError(f"otb compile {piece}: {r.stderr.strip()}")
    with open(ir) as f:
        return json.load(f)


def eval_piece(ir, asap_dir, piece):
    """[(performer, r)] for every human performance of the piece."""
    kind, bwv = bwv_of(piece)
    pdir = os.path.join(asap_dir, "Bach", kind, bwv)
    if not os.path.isdir(pdir):
        return None
    score_ann = os.path.join(pdir, "midi_score_annotations.txt")
    positions = score_beat_positions(score_ann)
    tempo_map = [(t["wn"], t["bpm"]) for t in ir["tempoMap"]]
    ours = local_tempo([our_time_at(tempo_map, w) for w in positions])
    results = []
    for f in sorted(os.listdir(pdir)):
        if not f.endswith("_annotations.txt") or f.startswith("midi_score"):
            continue
        human = local_tempo(perf_beat_times(os.path.join(pdir, f)))
        n = min(len(ours), len(human))
        pairs = [(o, h) for o, h in zip(ours[:n], human[:n])
                 if not (math.isnan(o) or math.isnan(h))]
        r = pearson([p[0] for p in pairs], [p[1] for p in pairs])
        results.append((f[: -len("_annotations.txt")], r))
    return results


def with_expression(base_config, value, tmpdir):
    return with_knobs(base_config, {"expression": value}, tmpdir)


KNOB_SECTIONS = {
    "expression": "interpretation",
    "arch_piece": "arches",
    "arch_group": "arches",
    "rit_floor": "agogics",
    "rit_span": "agogics",
    "cadence_depth": "agogics",
    "boundary_ease": "agogics",
    "open_push": "agogics",
    "open_span": "agogics",
    "subject_push": "agogics",
    "novelty_brake": "agogics",
    "mid_drift": "agogics",
    "sus_lean": "dissonance",
}

# every grid contains an OFF value: fitting may disable a rule the
# residuals proposed — the data giveth and the data taketh away
KNOB_GRIDS = {
    "expression": [0.4, 0.6, 0.8, 1.0, 1.3],
    "arch_piece": [0.0, 0.015, 0.03, 0.06],
    "arch_group": [0.0, 0.01, 0.02, 0.04],
    "rit_floor": [0.3, 0.4, 0.5, 0.6, 0.75],
    "rit_span": [0.5, 1.0, 2.0, 3.0],
    "cadence_depth": [0.0, 0.04, 0.08, 0.15],
    "boundary_ease": [0.0, 0.03, 0.06, 0.12],
    "open_push": [0.0, 0.03, 0.06, 0.1],
    "open_span": [1.0, 2.0, 4.0],
    "subject_push": [0.0, 0.015, 0.03],
    "novelty_brake": [0.0, 0.02, 0.04, 0.08],
    "mid_drift": [0.0, 0.01, 0.02, 0.04],
    "sus_lean": [0.0, 0.02, 0.05, 0.1],
}


def with_knobs(base_config, knobs, tmpdir):
    # a None would serialize as invalid TOML and kill the whole fit at
    # load time; absent means "keep the config's default", which is
    # what an unfittable knob deserves
    knobs = {k: v for k, v in knobs.items() if v is not None}
    tag = "_".join(f"{k}{v}" for k, v in sorted(knobs.items()))
    path = os.path.join(tmpdir, f"knobs_{abs(hash(tag))}.toml")
    with open(path, "w") as out:
        if os.path.isfile(base_config):
            out.write(open(base_config).read())
            out.write("\n")
        by_sec = {}
        for k, v in knobs.items():
            by_sec.setdefault(KNOB_SECTIONS[k], []).append((k, v))
        for sec, kvs in by_sec.items():
            out.write(f"[{sec}]\n")
            for k, v in kvs:
                out.write(f"{k} = {v}\n")
    return path


def merge_toml(base, knobs):
    """Merge fitted values into the base config's existing tables.

    Appending duplicate [table] headers is invalid TOML (the config
    module promises validity); instead each fitted key is written into
    its section in place — replacing the line if the key exists,
    otherwise inserted at the section's end — and only truly missing
    sections are appended.
    """
    by_sec = {}
    for k, v in knobs.items():
        by_sec.setdefault(KNOB_SECTIONS[k], {})[k] = v
    lines = base.splitlines()
    out = []
    sec = None
    done = set()

    def flush_section(s):
        for k, v in sorted(by_sec.get(s, {}).items()):
            if (s, k) not in done:
                out.append(f"{k} = {v}")
                done.add((s, k))

    for ln in lines:
        stripped = ln.split("#")[0].strip()
        if stripped.startswith("[") and stripped.endswith("]"):
            flush_section(sec)
            sec = stripped[1:-1]
            out.append(ln)
            continue
        key = stripped.split("=")[0].strip() if "=" in stripped else None
        if sec in by_sec and key in by_sec[sec]:
            out.append(f"{key} = {by_sec[sec][key]}")
            done.add((sec, key))
        else:
            out.append(ln)
    flush_section(sec)
    for s in sorted(by_sec):
        if any((s, k) not in done for k in by_sec[s]):
            out.append(f"\n[{s}]")
            flush_section(s)
    return "\n".join(out) + "\n"


def performer_pieces(asap_dir, kern_dir, performer):
    """Pieces (in our naming) this performer recorded."""
    out = []
    for p in sorted(f[:-4] for f in os.listdir(kern_dir)
                    if f.endswith(".krn")):
        pdir = os.path.join(asap_dir, "Bach", *bwv_of(p))
        if os.path.isfile(os.path.join(
                pdir, performer + "_annotations.txt")):
            out.append(p)
    return out


def performer_objective(otb, args, performer, pieces, config, tmp):
    rs = []
    for piece in pieces:
        ir = compile_ir(otb, piece, args.kern, tmp, config)
        for who, r in eval_piece(ir, args.asap, piece):
            if who == performer and not math.isnan(r):
                rs.append(r)
    return sum(rs) / len(rs) if rs else float("nan")


def mean_r(otb, args, pieces, config, tmp):
    rs = []
    for piece in pieces:
        ir = compile_ir(otb, piece, args.kern, tmp, config)
        rs.extend(r for _, r in eval_piece(ir, args.asap, piece)
                  if not math.isnan(r))
    return sum(rs) / len(rs) if rs else float("nan")


def fit_defaults(otb, args, pieces, tmp):
    """Fit the tempo-side defaults against the human population.

    Discipline: coordinate descent on the Book I overlap only; Book II
    is held out and only ever evaluated — the reported test r is the
    honest number. Discovered-rule knobs all carry 0 in their grids, so
    a rule that does not survive contact with the humans gets switched
    off rather than defended.
    """
    train = [p for p in pieces if p.startswith("wtc1")]
    test = [p for p in pieces if p.startswith("wtc2")]
    print(f"train: {len(train)} Book I pieces | "
          f"test: {len(test)} Book II pieces (held out)")
    base_tr = mean_r(otb, args, train, args.config, tmp)
    base_te = mean_r(otb, args, test, args.config, tmp)
    print(f"defaults: train r = {base_tr:.3f}, test r = {base_te:.3f}")
    grids = KNOB_GRIDS
    if getattr(args, "knobs", None):
        wanted = args.knobs.split(",")
        unknown = [k for k in wanted if k not in KNOB_GRIDS]
        if unknown:
            sys.exit(f"unknown knobs: {unknown} (know: {list(KNOB_GRIDS)})")
        grids = {k: KNOB_GRIDS[k] for k in wanted}
    knobs = {}
    for sweep in range(2):
        for knob, grid in grids.items():
            best_v, best_r = None, -2
            for v in grid:
                trial = dict(knobs)
                trial[knob] = v
                cfg = with_knobs(args.config, trial, tmp)
                r = mean_r(otb, args, train, cfg, tmp)
                if not math.isnan(r) and r > best_r:
                    best_v, best_r = v, r
            if best_v is None:  # every grid value was NaN: keep default
                knobs.pop(knob, None)
                print(f"  sweep {sweep + 1} {knob:<14} -> all NaN, "
                      f"keeping the config default")
                continue
            knobs[knob] = best_v
            print(f"  sweep {sweep + 1} {knob:<14} -> {best_v}"
                  f"   train r = {best_r:.3f}")
    cfg = with_knobs(args.config, knobs, tmp)
    final_tr = mean_r(otb, args, train, cfg, tmp)
    final_te = mean_r(otb, args, test, cfg, tmp)
    print(f"\nfitted: train r = {final_tr:.3f} (was {base_tr:.3f}) | "
          f"TEST r = {final_te:.3f} (was {base_te:.3f})")
    print("\nfitted values (merge into config/default.toml):")
    by_sec = {}
    for k, v in knobs.items():
        by_sec.setdefault(KNOB_SECTIONS[k], []).append((k, v))
    for sec, kvs in sorted(by_sec.items()):
        print(f"[{sec}]")
        for k, v in sorted(kvs):
            print(f"{k} = {v}")


def fit_performer(otb, args, performer, tmp):
    pieces = performer_pieces(args.asap, args.kern, performer)
    if not pieces:
        sys.exit(f"no WTC recordings by {performer} in ASAP")
    knobs = {}
    base_r = performer_objective(
        otb, args, performer, pieces, args.config, tmp)
    print(f"{performer}: {len(pieces)} pieces "
          f"({', '.join(pieces)}), default r = {base_r:.3f}")
    # two sweeps of coordinate descent over the knob grids
    for sweep in range(2):
        for knob, grid in KNOB_GRIDS.items():
            best_v, best_r = None, -2
            for v in grid:
                trial = dict(knobs)
                trial[knob] = v
                cfg = with_knobs(args.config, trial, tmp)
                r = performer_objective(
                    otb, args, performer, pieces, cfg, tmp)
                if not math.isnan(r) and r > best_r:
                    best_v, best_r = v, r
            if best_v is None:  # every grid value was NaN: keep default
                knobs.pop(knob, None)
                print(f"  sweep {sweep + 1} {knob:<14} -> all NaN, "
                      f"keeping the config default")
                continue
            knobs[knob] = best_v
            print(f"  sweep {sweep + 1} {knob:<14} -> {best_v}"
                  f"   r = {best_r:.3f}")
    final_r = performer_objective(
        otb, args, performer, pieces,
        with_knobs(args.config, knobs, tmp), tmp)
    # write the performer as a config
    outdir = os.path.join(ROOT, "config", "performers")
    os.makedirs(outdir, exist_ok=True)
    path = os.path.join(outdir, performer + ".toml")
    header = (
        f"# {performer}, fitted from ASAP beat annotations "
        f"({len(pieces)} WTC pieces: {', '.join(pieces)}).\n"
        f"# In-sample tempo-shape correlation: {final_r:.3f} "
        f"(defaults: {base_r:.3f}).\n"
        f"# Self-contained: the base configuration with the fitted\n"
        f"# stance merged in — --config {os.path.basename(path)} alone "
        f"reproduces the number.\n")
    base = open(args.config).read() if os.path.isfile(args.config) else ""
    with open(path, "w") as out:
        out.write(header + merge_toml(base, knobs))
    try:
        import tomllib
        with open(path, "rb") as f:
            tomllib.load(f)
    except Exception as e:  # noqa: BLE001
        print(f"  WARNING: generated profile is not valid TOML: {e}")
    print(f"  fitted r = {final_r:.3f} (default {base_r:.3f}) -> {path}")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("pieces", nargs="*", help="wtc1f01 ... (default: all)")
    ap.add_argument("--asap", default=os.path.join(ROOT, "corpus", "asap"))
    ap.add_argument("--kern",
                    default=os.path.join(ROOT, "corpus", "bach-wtc", "kern"))
    ap.add_argument("--config",
                    default=os.path.join(ROOT, "config", "default.toml"))
    ap.add_argument("--otb", default=None)
    ap.add_argument("--fit", action="store_true",
                    help="grid the [interpretation] expression knob")
    ap.add_argument("--fit-defaults", action="store_true",
                    help="coordinate-descent the tempo knobs against ALL "
                         "performances: train on Book I, validate on "
                         "Book II, print the fitted [sections]")
    ap.add_argument("--knobs", metavar="K1,K2",
                    help="restrict --fit-defaults to these knobs (a "
                         "targeted refit after a rule's SHAPE changed)")
    ap.add_argument("--performer-fit", metavar="NAME",
                    help="fit the interpretation knobs to one ASAP "
                         "performer; write config/performers/NAME.toml")
    args = ap.parse_args()

    otb = args.otb
    if not otb:
        root = subprocess.check_output(
            ["stack", "path", "--local-install-root"], cwd=ROOT,
            stderr=subprocess.DEVNULL, text=True).strip()
        otb = os.path.join(root, "bin", "otb")

    pieces = args.pieces
    if not pieces:
        pieces = sorted(
            p[:-4] for p in os.listdir(args.kern) if p.endswith(".krn"))
    # keep only pieces ASAP has
    pieces = [p for p in pieces
              if os.path.isdir(os.path.join(
                  args.asap, "Bach", *bwv_of(p)))]
    if not pieces:
        sys.exit("no overlapping pieces found (is corpus/asap present?)")

    with tempfile.TemporaryDirectory() as tmp:
        if args.fit_defaults:
            fit_defaults(otb, args, pieces, tmp)
            return
        if args.performer_fit:
            fit_performer(otb, args, args.performer_fit, tmp)
            return
        if args.fit:
            grid = [0.5, 0.75, 1.0, 1.25, 1.5, 2.0]
            print(f"{'piece':<10}" + "".join(f"  x{v:<6}" for v in grid))
            best_count = {v: 0 for v in grid}
            for piece in pieces:
                row = []
                for v in grid:
                    cfg = with_expression(args.config, v, tmp)
                    ir = compile_ir(otb, piece, args.kern, tmp, cfg)
                    rs = [r for _, r in eval_piece(ir, args.asap, piece)
                          if not math.isnan(r)]
                    row.append(sum(rs) / len(rs) if rs else float("nan"))
                best = max(range(len(grid)), key=lambda i: row[i])
                best_count[grid[best]] += 1
                print(f"{piece:<10}"
                      + "".join(f"  {r:+.3f}" + ("*" if i == best else " ")
                                for i, r in enumerate(row)))
            print("\nbest-expression histogram:",
                  {v: c for v, c in best_count.items() if c})
            return

        all_rs = []
        print(f"{'piece':<10} {'performances':>12} {'mean r':>8} "
              f"{'min':>7} {'max':>7}")
        for piece in pieces:
            ir = compile_ir(otb, piece, args.kern, tmp, args.config)
            results = eval_piece(ir, args.asap, piece)
            rs = [r for _, r in results if not math.isnan(r)]
            if not rs:
                continue
            all_rs.extend(rs)
            print(f"{piece:<10} {len(rs):>12} {sum(rs)/len(rs):>8.3f} "
                  f"{min(rs):>7.3f} {max(rs):>7.3f}")
        if all_rs:
            print(f"\n{len(all_rs)} performances, "
                  f"grand mean r = {sum(all_rs)/len(all_rs):.3f}")


if __name__ == "__main__":
    main()
