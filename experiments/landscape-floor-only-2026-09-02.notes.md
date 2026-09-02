# The floor-only series: which timing zero carries the collapse

Seven single-rule ablations (zero-floor 0.25 applied to ONE knob, the
rest free to reoptimize; 20 starts each) against the c4dd5ac-era
baseline whose unconstrained optimum is train 0.6304 / held-out 0.5801.
These are CONDITIONAL MARGINAL costs, not an attribution under
interactions — but the answer barely needs one:

| floored rule  | floor  | train  | Δtrain  | held-out    | Δtest   |
|---------------|--------|--------|---------|-------------|---------|
| boundary_ease | 0.0075 | 0.5791 | -0.0513 | 0.5315-.5341| -0.048  |
| cadence_depth | 0.01   | 0.6304 | ±0      | 0.5815      | +0.0014 |
| subject_push  | 0.0038 | 0.6286 | -0.0018 | 0.5778-.5789| -0.002  |
| novelty_brake | 0.005  | 0.6338 | +0.0034 | 0.5833      | +0.0032 |
| sus_lean      | 0.005  | 0.6299 | -0.0005 | 0.5789      | -0.0012 |
| arch_group    | 0.0025 | 0.6311 | +0.0007 | 0.5831      | +0.0030 |
| open_push     | 0.0075 | 0.6299 | -0.0005 | 0.5782-.5861| ~0      |

**boundary_ease alone reproduces essentially the entire all-rules-on
collapse** (-0.051 of the collective -0.054): even 0.0075 of linear
boundary easing is poison, consistent with its config comment — the
deep boundary holds are real in the residuals, but this SHAPE of
easing is not how humans do them, so any forced amount anticorrelates.
Every other zero is soft at this resolution.

Two floors look mildly BENEFICIAL (novelty_brake at 0.005: train
0.6338 > the unconstrained optimum's 0.6304, held-out 0.5833 > 0.5801;
arch_group at 0.0025 similar) — sub-grid-resolution optima the
unconstrained lattice could not express, since these knobs' smallest
nonzero grid values (0.02, 0.01) overshoot. Caveats before anyone
moves a global: single split, Book II is development data, and the
deltas (~+0.003) sit inside the basin-comparison CIs (±0.005–0.009).
A finer-grid unconstrained run around those knobs is the honest next
instrument.

## Artifact status

- cadence_depth: REGISTERED
  (landscape-timing-2026-09-02-08d06cd1b353….json, producer 76a4a0b).
- The other six were refused registration mid-series: a commit that
  touched config/casting (the standing-rig change) moved the
  build-scoped trees, and config/ was then part of the build scope — a
  false refusal, since casting shapes the board, not the binary or the
  objective (the experiment's own prefit/corpus digests cover what is
  evaluated). The scope now covers only what shapes the binary
  (src, app, otb.cabal, stack.yaml{,.lock}, cabal.project). The six
  are preserved from their intact per-experiment checkpoints as legacy
  artifacts (same 76a4a0b-built binary as the registered one, per
  their metadata digests):

  - boundary_ease  …-8061dfdc67a6.json  sha256 82ac6fb536f68cfe…
  - subject_push   …-73123642e1ed.json  sha256 67079e5b297a0609…
  - novelty_brake  …-b4777b9560d7.json  sha256 69238246392d17c6…
  - sus_lean       …-f3b9e1dcc82a.json  sha256 3801aacd77ac4288…
  - arch_group     …-84ebd83bd80e.json  sha256 4d33953ba8837f59…
  - open_push      …-ad78810a8220.json  sha256 66b8d57fa8bfad33…

  (Full digests: sha256sum the files; the truncations here are
  fingerprints for prose, the files are the record.)
