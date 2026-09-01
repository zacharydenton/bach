#!/usr/bin/env python3
"""Per-piece hierarchical fitting against the widened human corpus.

Global fits stay the prior; each piece earns deviations only to the
extent its data supports them. Per piece, three layers:

  tempo   — identifiable even from one performance: the human body
            tempo (middle 80% of annotated beats), median across
            performances, SHRUNK toward the compiler's own authority
            (declared *MM / giusto, read from the IR's tempo plateau)
            by n/(n+1). Written as `tempo = ...` in [piece.X].
  timing  — coordinate descent over agogic shape knobs against the
            piece's beat-level tempo r (ae.eval_piece), knobs written
            into the [piece.X] section (with_knobs sections=piece.X).
  velocity— same descent over dynamics knobs against note-level
            velocity r (vel_fit.piece_r).

Fitted knob deltas are shrunk toward the global value by n/(n+k)
(k = --shrink-k, default 2), then the shrunk config is re-scored and
kept only if it does not lose to the baseline. Where a piece has n>=3
performances, LEAVE-ONE-PERFORMER-OUT decides: the fit must not
degrade the held-out performer's objective. n<3 pieces keep tempo
plus the shrunk shape only.

State lives in corpus/piece-fits.json (regenerable; resumable), and
`--apply` merges [piece.X] sections into config/default.toml with
per-key provenance comments. Keys already present WITHOUT the FITTED
marker are hand-authored vetoes and are never touched.

    python3 tools/piece_fit.py --fit            # all pieces with humans
    python3 tools/piece_fit.py --fit wtc1p08
    python3 tools/piece_fit.py --apply --dry-run
    python3 tools/piece_fit.py --apply
"""
import argparse
import datetime
import hashlib
import json
import os
import statistics
import subprocess
import sys
import tempfile

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(HERE)
sys.path.insert(0, HERE)
import asap_eval as ae  # noqa: E402
import note_align as na  # noqa: E402
import vel_fit as vf  # noqa: E402

KERN = os.path.join(ROOT, "corpus", "bach-wtc", "kern")
CONFIG = os.path.join(ROOT, "config", "default.toml")
STATE = os.path.join(ROOT, "corpus", "piece-fits.json")
MARK = "# FITTED"

# knobs a per-piece fit may move, with their global-default fallbacks
# read from the base config at run time; grids include the global value
TIMING_KNOBS = {
    "arch_piece": [0.0, 0.012, 0.024, 0.05, 0.08],
    "boundary_ease": [0.0, 0.02, 0.04, 0.08],
    "cadence_depth": [0.0, 0.03, 0.06, 0.1],
    "open_push": [0.0, 0.03, 0.06, 0.12],
    "rit_span": [0.5, 1.0, 2.0, 4.0],
    "rit_floor": [0.2, 0.3, 0.45, 0.6],
}
VEL_KNOBS = {
    "vel_highloud": [0.0, 0.4, 0.8, 1.2],
    "vel_bar": [0.0, 4.0, 8.0],
    "vel_beat": [0.0, 3.0, 6.0],
    "dialogue_vel": [0.0, 2.0, 5.0],
    "subject_vel": [0.0, 5.0, 10.0],
}
GATE_KNOBS = set()  # velocities/timing only; gates stay global


def prefit_config(tmp):
    """default.toml with every FITTED line and the generated banner
    stripped: the IMMUTABLE pre-fit baseline. Fitting against the live
    config would use the previous run's output as authority and
    baseline — a rerun would double-fit every piece (p08's authority
    drifted 98.6 -> 72.3 that way). Returns (path, sha256 of the
    stripped text) — the sha fingerprints resumable state."""
    lines = [l for l in open(CONFIG)
             if MARK not in l
             and not l.startswith("# ---- fitted per piece")]
    text = "".join(lines)
    path = os.path.join(tmp, "prefit.toml")
    open(path, "w").write(text)
    return path, hashlib.sha256(text.encode()).hexdigest()


def base_values(cfg):
    """Global values for every fit knob, from the pre-fit config."""
    vals = {}
    section = None
    for line in open(cfg):
        s = line.split("#")[0].strip()
        if s.startswith("[") and s.endswith("]"):
            section = s[1:-1]
        elif "=" in s and not section.startswith("piece."):
            k, v = s.split("=", 1)
            try:
                vals[k.strip()] = float(v)
            except ValueError:
                pass
    return vals


def perf_dirs(piece):
    """[(pdir, perf)] across sources — from the .match enumeration."""
    out = []
    for perf, mpath in na.piece_performances(piece):
        out.append((os.path.dirname(mpath), perf))
    return out


def body_tempo(pdir, perf):
    """Quarter-note bpm over the middle 80% of a performance's beats."""
    sw = ae.score_beat_positions(
        os.path.join(pdir, "midi_score_annotations.txt"))
    ts = ae.perf_beat_times(os.path.join(pdir, perf + "_annotations.txt"))
    n = min(len(sw), len(ts))
    if n < 5:
        return None
    lo, hi = int(n * 0.1), int(n * 0.9)
    dw = sw[hi] - sw[lo]
    dt = ts[hi] - ts[lo]
    if dt <= 0 or dw <= 0:
        return None
    return dw / dt * 240.0


def authority_tempo(ir):
    """The compiler's own base tempo: the tempo map's plateau."""
    return statistics.median(t["bpm"] for t in ir["tempoMap"])


class Evaluator:
    """Compile-once-per-candidate objectives: the IR for a knob tuple
    is cached, so LOPO folds re-use every compile the full fit made.
    Everything scores against the PRE-FIT config, never the live one."""

    def __init__(self, otb, piece, tmp, cfg):
        self.otb, self.piece, self.tmp, self.cfg = otb, piece, tmp, cfg
        self.irs = {}
        self.vel_rows = {}

    def ir_for(self, trial, sections):
        key = tuple(sorted(trial.items()))
        if key not in self.irs:
            cfg = (ae.with_knobs(self.cfg, trial, self.tmp,
                                 sections=sections)
                   if trial else self.cfg)
            self.irs[key] = ae.compile_ir(
                self.otb, self.piece, KERN, self.tmp, cfg)
        return key, self.irs[key]

    def timing(self, trial, sections, only=None):
        _, ir = self.ir_for(trial, sections)
        rows = ae.eval_piece(
            ir, os.path.join(ROOT, "corpus", "asap"), self.piece)
        if not rows:
            return None
        rows = [(p, r) for p, r in rows
                if r == r and (only is None or p in only)]
        return statistics.mean(r for _, r in rows) if rows else None

    def velocity(self, trial, sections, only=None):
        key, ir = self.ir_for(trial, sections)
        if key not in self.vel_rows:
            irn = na.load_ir_notes(ir)
            per = {}
            for perf, mpath in na.piece_performances(self.piece):
                rows, _ = na.bridge(irn, na.parse_match(mpath))
                rows = [r for r in rows if not r["is_orn"]]
                if len(rows) >= 30:
                    per[perf] = ae.pearson(
                        [r["otb_vel"] for r in rows],
                        [r["human_vel"] for r in rows])
            self.vel_rows[key] = per
        per = self.vel_rows[key]
        rs = [r for p, r in per.items()
              if r == r and (only is None or p in only)]
        return statistics.mean(rs) if rs else None


def descend(ev, piece, knobs, base, objective, only=None):
    """Two-sweep per-piece coordinate descent in the [piece.X] section."""
    sections = {k: f"piece.{piece}" for k in knobs}
    current = {}
    best = objective({}, sections, only)
    if best is None:
        return None, None
    baseline = best
    for _sweep in range(2):
        for k, grid in knobs.items():
            for v in grid:
                if v == current.get(k, base.get(k)):
                    continue
                trial = dict(current)
                trial[k] = v
                r = objective(trial, sections, only)
                if r is not None and r > best + 1e-4:
                    best = r
                    current = trial
    return current, (baseline, best)


def shrink(current, base, n, k):
    return {key: round(base.get(key, 0.0)
                       + (v - base.get(key, 0.0)) * n / (n + k), 4)
            for key, v in current.items()}


def fit_piece(otb, piece, base, shrink_k, tmp, cfg):
    perfs = perf_dirs(piece)
    n = len(perfs)
    if n == 0:
        return None
    out = {"piece": piece, "n": n, "performers": [p for _, p in perfs]}

    # --- tempo ---
    ir = ae.compile_ir(otb, piece, KERN, tmp, cfg)
    tempos = [t for t in (body_tempo(d, p) for d, p in perfs) if t]
    if tempos:
        human = statistics.median(tempos)
        auth = authority_tempo(ir)
        out["tempo"] = {
            "human_median": round(human, 1), "authority": round(auth, 1),
            "fitted": round(auth + (human - auth) * n / (n + 1), 1),
        }

    # --- shape fits (timing, velocity), each with LOPO where possible ---
    ev = Evaluator(otb, piece, tmp, cfg)
    for name, knobs, objective in (
            ("timing", TIMING_KNOBS, ev.timing),
            ("velocity", VEL_KNOBS, ev.velocity)):
        fitted, scores = descend(ev, piece, knobs, base, objective)
        if fitted is None:
            continue
        rec = {"raw": fitted, "baseline_r": round(scores[0], 4),
               "fitted_r": round(scores[1], 4)}
        shrunk = shrink(fitted, base, n, shrink_k)
        rec["shrunk"] = shrunk
        # the candidate that DEPLOYS is the shrunk one — score it, and
        # never ship a config that loses to its own baseline
        sections = {k: f"piece.{piece}" for k in knobs}
        deployed = objective(shrunk, sections) if shrunk else scores[0]
        rec["deployed_r"] = (round(deployed, 4)
                             if deployed is not None else None)
        beats_base = (deployed is not None
                      and deployed >= scores[0] - 1e-4)
        if n >= 3 and fitted:
            # LOPO: refit on n-1 (compiles shared via the cache), score
            # the held-out performer on the shrunk fold fit
            deltas = []
            names = [p for _, p in perfs]
            sections = {k: f"piece.{piece}" for k in knobs}
            for hold in names:
                keep = [p for p in names if p != hold]
                f2, _ = descend(ev, piece, knobs, base, objective,
                                only=set(keep))
                if f2 is None:
                    continue
                s2 = shrink(f2, base, n - 1, shrink_k)
                r_fit = objective(s2, sections, {hold})
                r_base = objective({}, sections, {hold})
                if r_fit is not None and r_base is not None:
                    deltas.append(r_fit - r_base)
            if deltas:
                rec["lopo_delta"] = round(statistics.mean(deltas), 4)
                rec["kept"] = beats_base and rec["lopo_delta"] >= -1e-3
            else:
                rec["kept"] = False
        else:
            # too little data to hold anything out: the shrunk deltas
            # deploy only if the deployed score itself holds up
            rec["kept"] = bool(fitted) and beats_base
            rec["lopo_delta"] = None
        out[name] = rec
    return out


# ---------------------------------------------------------------------
# applying fits to config/default.toml

def build_sections(state):
    """piece -> [(key, value, comment)] from the fit state."""
    date = datetime.date.today().isoformat()
    out = {}
    for piece, rec in sorted(state.items()):
        if piece == "_meta":
            continue
        lines = []
        t = rec.get("tempo")
        if t:
            lines.append(("tempo", t["fitted"],
                          f"{MARK} {date} n={rec['n']} human "
                          f"{t['human_median']} vs authority "
                          f"{t['authority']}"))
        for name in ("timing", "velocity"):
            r = rec.get(name)
            if not r or not r.get("kept"):
                continue
            lopo = (f" LOPO {r['lopo_delta']:+}" if r.get("lopo_delta")
                    is not None else " unvalidated(n<3)")
            shown = r.get("deployed_r", r["fitted_r"])
            for k, v in sorted(r["shrunk"].items()):
                lines.append((k, v,
                              f"{MARK} {date} n={rec['n']} "
                              f"r {r['baseline_r']}->{shown}"
                              f"{lopo}"))
        if lines:
            out[piece] = lines
    return out


def apply_fits(state, dry_run):
    src = open(CONFIG).read().rstrip("\n").split("\n")
    sections = build_sections(state)

    # existing [piece.X] spans and their hand-authored keys
    spans = {}
    cur, start = None, None
    for i, line in enumerate(src + ["[end]"]):
        s = line.split("#")[0].strip()
        if s.startswith("[") and s.endswith("]"):
            if cur is not None:
                spans[cur] = (start, i)
            cur = s[1:-1] if s[1:-1].startswith("piece.") else None
            start = i
    new = list(src)

    # update existing sections in place (bottom-up so spans stay valid)
    for piece in sorted(sections, reverse=True):
        sec = "piece." + piece
        if sec not in spans:
            continue
        a, b = spans[sec]
        body = new[a + 1:b]
        hand_keys = {l.split("=")[0].strip() for l in body
                     if "=" in l and MARK not in l}
        kept = [l for l in body if MARK not in l]
        while kept and not kept[-1].strip():
            kept.pop()
        add = [f"{k} = {v} {c}" for k, v, c in sections[piece]
               if k not in hand_keys]
        new[a + 1:b] = kept + add + [""]
        del sections[piece]

    if sections:
        new.append("")
        new.append(f"# ---- fitted per piece (piece_fit.py, "
                   f"{datetime.date.today().isoformat()}) ----")
        for piece, lines in sorted(sections.items()):
            new.append("")
            new.append(f"[piece.{piece}]")
            new.extend(f"{k} = {v} {c}" for k, v, c in lines)

    text = "\n".join(new) + "\n"
    if dry_run:
        import difflib
        old = open(CONFIG).read()
        sys.stdout.writelines(difflib.unified_diff(
            old.splitlines(True), text.splitlines(True),
            "default.toml", "fitted"))
    else:
        open(CONFIG, "w").write(text)
        print(f"applied {sum(len(v) for v in build_sections(state).values())}"
              f" keys — validate with a compile")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("pieces", nargs="*")
    ap.add_argument("--fit", action="store_true")
    ap.add_argument("--apply", action="store_true")
    ap.add_argument("--dry-run", action="store_true")
    ap.add_argument("--shrink-k", type=float, default=2.0)
    ap.add_argument("--otb", default=None)
    args = ap.parse_args()

    state = json.load(open(STATE)) if os.path.isfile(STATE) else {}

    if args.fit:
        otb = args.otb
        if not otb:
            r = subprocess.check_output(
                ["stack", "path", "--local-install-root"], cwd=ROOT,
                text=True).strip()
            otb = os.path.join(r, "bin", "otb")
        pieces = args.pieces or sorted(
            p[:-4] for p in os.listdir(KERN) if p.endswith(".krn"))
        pieces = [p for p in pieces if na.piece_performances(p)]
        with tempfile.TemporaryDirectory() as tmp:
            cfg, sha = prefit_config(tmp)
            meta = state.get("_meta", {})
            if meta.get("prefit_sha256") not in (None, sha):
                print("pre-fit config changed since the saved state — "
                      "starting fresh")
                state = {}
            state["_meta"] = {"prefit_sha256": sha}
            base = base_values(cfg)
            for piece in pieces:
                if piece in state and not args.pieces:
                    continue  # resumable
                rec = fit_piece(otb, piece, base, args.shrink_k, tmp,
                                cfg)
                if rec:
                    state[piece] = rec
                    json.dump(state, open(STATE, "w"), indent=1)
                    print(f"{piece}: n={rec['n']} "
                          f"tempo {rec.get('tempo', {}).get('fitted')} "
                          f"timing {rec.get('timing', {}).get('kept')} "
                          f"vel {rec.get('velocity', {}).get('kept')}")

    if args.apply or args.dry_run:
        apply_fits(state, args.dry_run)


if __name__ == "__main__":
    main()
