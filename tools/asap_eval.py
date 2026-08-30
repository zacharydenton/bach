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
    """Whole-note position of every annotated beat.

    The score annotation file marks each beat b/db, with the meter
    stated at downbeats ("db,4/4,0"). Beats between downbeats divide
    the bar evenly, which is how ASAP defines them.
    """
    rows = []
    with open(path) as f:
        for line in f:
            parts = line.rstrip("\n").split("\t")
            if len(parts) < 3:
                continue
            tag = parts[2]
            kind = tag.split(",")[0]
            meter = None
            bits = tag.split(",")
            if len(bits) >= 2 and "/" in bits[1]:
                n, d = bits[1].split("/")
                meter = (int(n), int(d))
            rows.append((kind, meter))
    # group into bars at downbeats
    positions = []
    pos = 0.0
    bar_len = None
    i = 0
    while i < len(rows):
        kind, meter = rows[i]
        if meter:
            bar_len = meter[0] / meter[1]
        # count beats to the next downbeat (this bar's beats)
        j = i + 1
        while j < len(rows) and rows[j][0] != "db":
            j += 1
        nbeats = j - i
        if bar_len is None:
            bar_len = nbeats / 4  # assume quarter beats before any meter
        step = bar_len / nbeats
        for k in range(nbeats):
            positions.append(pos + k * step)
        pos += bar_len
        i = j
    return positions


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
    path = os.path.join(tmpdir, f"expr_{value}.toml")
    with open(path, "w") as out:
        if os.path.isfile(base_config):
            out.write(open(base_config).read())
            out.write("\n")
        out.write(f"[interpretation]\nexpression = {value}\n")
    return path


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
