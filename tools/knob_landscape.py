#!/usr/bin/env python3
"""Multi-start search over the global knob families: are the zeros real?

The committed fits came from ONE two-sweep coordinate-descent path, so
a zero may mean "unsupported by humans" — or credit taken by a
correlated rule, a path-dependent local optimum, a wrong shape over a
right idea, or an effect Pearson cannot see. This tool reruns the
global fit from N randomized starts (random init on the same grids,
randomized coordinate order) and reports, per knob:

  * how many starts end at zero / at each grid value;
  * the spread of equally-predictive optima (within --slack of the
    best train objective) — the EXPRESSIVE MANIFOLD, if it exists:
    musically distinct knob allocations the human data cannot tell
    apart;
  * train (Book I) and held-out (Book II) objectives per final.

A zero that survives twenty independent searches is evidence. A knob
that lands nonzero in half the equally-predictive finals is a credit
war, not a veto.

    python3 tools/knob_landscape.py --family timing --starts 20
    python3 tools/knob_landscape.py --family velocity --starts 20

State accumulates in corpus/knob-landscape-<family>.json (resumable;
one start at a time). Objective evaluations cache per (knob-tuple,
piece) and compile in parallel.
"""
import argparse
import concurrent.futures
import json
import os
import hashlib
import random
import statistics
import subprocess
import sys
import tempfile

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(HERE)
sys.path.insert(0, HERE)
import asap_eval as ae  # noqa: E402
import note_align as na  # noqa: E402
import piece_fit as pf  # noqa: E402

KERN = os.path.join(ROOT, "corpus", "bach-wtc", "kern")
CONFIG = os.path.join(ROOT, "config", "default.toml")

FAMILIES = {
    "timing": (ae.KNOB_GRIDS, ae.KNOB_SECTIONS),
}
import vel_fit as vf
FAMILIES["velocity"] = (vf.VEL_KNOB_GRIDS, vf.VEL_KNOB_SECTIONS)


class Objective:
    """Mean per-performance r over a piece set, cached per knob tuple.

    Timing: beat-level tempo r (ae.eval_piece). Velocity: note-level
    velocity r via the bridge. Compiles run in a thread pool; results
    cache per (tuple, piece) so every start shares every lattice point
    any start has visited.
    """

    def __init__(self, otb, family, pieces, tmp, jobs=8):
        self.otb, self.family = otb, family
        self.pieces, self.tmp, self.jobs = pieces, tmp, jobs
        self.cache = {}  # (knob_tuple, piece) -> [r, ...]
        self.grids, self.sections = FAMILIES[family]
        # evaluate against the PRE-FIT config: the committed per-piece
        # sections would otherwise mask every global knob under test
        self.base, _sha = pf.prefit_config(tmp)

    def _piece_rs(self, key, cfg, piece):
        """Per-piece statistic. Compile failures RAISE — a bad config
        must stop the search, not bias the objective as a silent empty
        piece. Timing: per-performance r list (flat-mean overall, the
        fit_defaults objective). Velocity: vel_fit.piece_r's exact
        statistic — ornaments INCLUDED, median across performers."""
        ck = (key, piece)
        if ck in self.cache:
            return self.cache[ck]
        ir = ae.compile_ir(self.otb, piece, KERN, self.tmp, cfg)
        if self.family == "timing":
            rows = ae.eval_piece(ir, os.path.join(ROOT, "corpus", "asap"),
                                 piece) or []
            out = [r for _, r in rows if r == r]
        else:
            med, n = vf.piece_r(ir, piece)
            out = [med] if n and med == med else []
        self.cache[ck] = out
        return out

    def __call__(self, knobs, pieces=None):
        pieces = pieces or self.pieces
        key = tuple(sorted(knobs.items()))
        # the candidate config is written ONCE, before any worker runs:
        # per-worker writes of the same knob-hashed filename raced, and
        # a torn read became a silently-cached empty piece
        cfg = (ae.with_knobs(self.base, knobs, self.tmp,
                             sections=self.sections)
               if knobs else self.base)
        with concurrent.futures.ThreadPoolExecutor(self.jobs) as ex:
            all_rs = list(ex.map(
                lambda p: self._piece_rs(key, cfg, p), pieces))
        if self.family == "timing":
            flat = [r for rs in all_rs for r in rs]
        else:
            flat = [rs[0] for rs in all_rs if rs]  # one median per piece
        return statistics.mean(flat) if flat else None


def descend(obj, grids, rng, init, max_sweeps=8):
    """Randomized-order coordinate descent to a FIXPOINT: sweeps until
    one full pass improves nothing (capped), so observed spread across
    starts is landscape structure, not incomplete convergence."""
    current = dict(init)
    best = obj(current)
    if best is None:
        return None, None, 0
    sweeps = 0
    for sweeps in range(1, max_sweeps + 1):
        improved = False
        order = list(grids)
        rng.shuffle(order)
        for k in order:
            for v in grids[k]:
                if v == current.get(k):
                    continue
                trial = dict(current)
                trial[k] = v
                r = obj(trial)
                if r is not None and r > best + 1e-4:
                    best = r
                    current = trial
                    improved = True
        if not improved:
            break
    return current, best, sweeps


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--family", choices=list(FAMILIES), default="timing")
    ap.add_argument("--starts", type=int, default=20)
    ap.add_argument("--slack", type=float, default=0.005)
    ap.add_argument("--jobs", type=int, default=8)
    ap.add_argument("--seed", type=int, default=1)
    ap.add_argument("--otb", default=None)
    args = ap.parse_args()

    otb = args.otb
    if not otb:
        r = subprocess.check_output(
            ["stack", "path", "--local-install-root"], cwd=ROOT,
            text=True).strip()
        otb = os.path.join(r, "bin", "otb")

    grids, _ = FAMILIES[args.family]
    pieces = sorted(p[:-4] for p in os.listdir(KERN) if p.endswith(".krn"))
    pieces = [p for p in pieces if na.piece_performances(p)]
    train = [p for p in pieces if p.startswith("wtc1")]
    test = [p for p in pieces if p.startswith("wtc2")]

    state_path = os.path.join(
        ROOT, "corpus", f"knob-landscape-{args.family}.json")
    state = (json.load(open(state_path))
             if os.path.isfile(state_path) else {"finals": []})

    with tempfile.TemporaryDirectory() as tmp:
        obj = Objective(otb, args.family, train, tmp, args.jobs)
        # the CONFIG values of every knob (compiler defaults for the
        # two that live only in code): starts are fully materialized
        # dicts, so a missing key can never masquerade as zero
        CODE_DEFAULTS = {"novelty_brake": 0.0, "mid_drift": 0.0}
        cfg_vals = pf.base_values(obj.base)
        base_knobs = {k: cfg_vals.get(k, CODE_DEFAULTS.get(k, 0.0))
                      for k in grids}
        fp = hashlib.sha256(
            (repr(sorted(grids.items())) + repr(sorted(base_knobs.items()))
             + args.family + str(args.seed)).encode()).hexdigest()
        if state.get("_meta", {}).get("fingerprint") not in (None, fp):
            print("landscape inputs changed — starting fresh")
            state = {"finals": []}
        state["_meta"] = {"fingerprint": fp}
        while len(state["finals"]) < args.starts:
            i = len(state["finals"])
            rng = random.Random(args.seed * 1000 + i)
            init = (dict(base_knobs) if i == 0  # start 0 = committed
                    else {k: rng.choice(g) for k, g in grids.items()})
            final, train_r, sweeps = descend(obj, grids, rng, init)
            if final is None:
                continue
            test_r = obj(final, test)
            state["finals"].append({
                "start": i, "init": init, "final": final,
                "sweeps": sweeps,
                "train_r": round(train_r, 4),
                "test_r": round(test_r, 4) if test_r else None,
            })
            json.dump(state, open(state_path, "w"), indent=1)
            print(f"start {i}: train {train_r:.4f} "
                  f"test {test_r:.4f} sweeps {sweeps} final {final}")

    # ---- report ----
    finals = state["finals"]
    best = max(f["train_r"] for f in finals)
    elite = [f for f in finals if f["train_r"] >= best - args.slack]
    print(f"\n{len(finals)} starts; best train {best:.4f}; "
          f"{len(elite)} within slack {args.slack} "
          f"(test spread {min(f['test_r'] for f in elite):.4f}"
          f"..{max(f['test_r'] for f in elite):.4f})")
    print(f"\n{'knob':16} zero-rate(all/elite)  elite values")
    for k in grids:
        allv = [f["final"][k] for f in finals]
        ev = [f["final"][k] for f in elite]
        z_all = sum(1 for v in allv if v == 0) / len(allv)
        z_el = sum(1 for v in ev if v == 0) / len(ev)
        vals = sorted(set(ev))
        print(f"{k:16} {z_all:.2f} / {z_el:.2f}          {vals}")


if __name__ == "__main__":
    main()
