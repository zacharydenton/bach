# Notes: landscape-timing-2026-09-02-d3a8a0a9ecbf

Corrections to how this experiment was first summarized (the manifest
itself is immutable; these notes carry the defensible reading).

## What the data shows, precisely

- 20 starts: 1 committed-baseline init (start 0) + 19 randomized.
- **17 of the 19 randomized starts** reached the primary optimum
  (train r 0.6304). Start 0 — the committed baseline — did **not**:
  it stalled with randomized starts 1 and 8 in a secondary basin.
- Within the elite, **14 endpoints are literally identical**; the
  other 3 differ only in open_span (1/2/4), which is inert while
  open_push = 0. The defensible statement is therefore: **one active
  allocation at this lattice resolution, modulo one unidentifiable
  inert dimension** — not "17/20 identical".

## The secondary basin, and why "unique" is a train-set claim

| basin      | train  | test   | allocation                                  |
|------------|--------|--------|---------------------------------------------|
| primary    | 0.6304 | 0.5801 | expression 0.6, mid_drift 0.01, open_push 0 |
| secondary  | 0.6243 | 0.5794 | expression 0.8, mid_drift 0, open_push 0.03 |

Held-out separation is **0.0007** — far inside any plausible noise
band for a 34-piece test set. Training r separates the basins (0.006)
but the held-out data does not meaningfully prefer one. So "the human
data picks a unique allocation" is overstated as a generalization
claim: the honest version is that the TRAIN objective has one active
optimum, while the held-out data is consistent with (at least) two
allocations that trade expression intensity against settling-in push.
An uncertainty analysis (10,000-sample paired bootstrap over the 94
held-out performances, and clustered by piece; 2026-09-02; inputs
and method archived in experiments/bootstrap-2026-09-02/) bounds it:
mean held-out difference +0.0006, 95% CI [-0.0054, +0.0061]
per-performance and [-0.0091, +0.0089] clustered by piece,
P(diff <= 0) = 0.41-0.42. Two distinct statements follow, and only
the first is licensed: (1) the held-out data does not distinguish
the basins — the uniqueness claim is train-set-only; (2) the basins
are NOT thereby shown equivalent — the clustered interval (+/-0.009)
is wider than the program's 0.005 slack, so differences of practical
size are not excluded. Statistically the pair is
indistinguishable-with-present-data; demonstrating equivalence would
require the interval inside a prespecified margin (more data, or a
tighter design). The perceptual comparison needs no such license —
the ear can arbitrate between candidates either way.
Both basins share six of the seven zeros — they disagree only on
whether open_push participates. The pair is the first concrete
target for the perceptual condition (A/B at the board; the deployed
config carries basin A).

## Provenance for this manifest

Produced by the otb binary built from commit 95d5fba (clean tree) —
recorded here because this manifest predates the producer block;
manifests minted after commit "the manifest learns who made it" carry
commit/dirty/compiler/platform inline and are named by a digest of
their complete content, so they cannot be overwritten or misattributed.
