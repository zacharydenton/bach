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
held-out performances, and clustered by piece; 2026-09-02, from
otb eval runs of both basin configs at the prefit level) settles it:
mean held-out difference +0.0006, 95% CI [-0.0054, +0.0061]
per-performance and [-0.0091, +0.0089] clustered by piece,
P(diff <= 0) = 0.41-0.42. The held-out data does NOT distinguish the
basins; the uniqueness claim is train-set-only, and the basin pair is
a real equivalence class whose resolution is legitimately perceptual.
Both basins share six of the seven zeros — they disagree only on
whether open_push participates. Notably, this pair is itself a candidate equivalence class — the
first concrete target for the perceptual condition (--emit-elite,
widened slack to include 0.6243, A/B at the board).

## Provenance for this manifest

Produced by the otb binary built from commit 95d5fba (clean tree) —
recorded here because this manifest predates the producer block;
manifests minted after commit "the manifest learns who made it" carry
commit/dirty/compiler/platform inline and are named by a digest of
their complete content, so they cannot be overwritten or misattributed.
