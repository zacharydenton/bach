# The conditions program, first pass (2026-09-02)

Five 20-start runs against the c4dd5ac config state (expression 0.6
baseline). Registration bookkeeping first, then what the data says.

## Artifact status

| run | condition | artifact | status |
|-----|-----------|----------|--------|
| 1 | velocity unconstrained | …velocity-…-5bf7cb3a31cd.json | legacy (95d5fba binary), enveloped |
| 2 | timing zero-floor 0.25 | …timing-…-c8caf6c9….json | **registered** (292a83e, producer block) |
| 3 | timing diversity 0.05 | …timing-…-aec576ff436b.json | legacy: preserved from checkpoints |
| 4 | velocity zero-floor 0.25 | none — see below | report-only (log survives) |
| 5 | velocity diversity 0.05 | …velocity-…-ce203c2a0199.json | legacy: preserved from checkpoints |

Runs 3–5 were refused registration by the build/runtime cross-check:
a mid-queue commit of experiment artifacts moved HEAD past the
binary's stamp — a FALSE refusal (nothing build-relevant changed),
now fixed by comparing build-scoped git tree hashes instead of
commits. Their producer is recoverable: both preserved states carry
the same binary digest, distinct from the 95d5fba binary — the
executable built clean at 292a83e. Run 4's checkpoint state was
clobbered by run 5 (the state path was per-family, not
per-experiment — also now fixed); its joint finals are lost, its
per-knob report survives in the run log, and it is being rerun under
the fixed binary to mint a registered manifest.

sha256 of preserved files:
- landscape-timing-2026-09-02-aec576ff436b.json:
  0ed83a8fc32fb0f6… (see git object for the full hash)
- landscape-velocity-2026-09-02-ce203c2a0199.json:
  9f10dd4c3d1400f7…

## The verdict the five runs converge on

**Timing zeros are load-bearing; velocity zeros are soft.**

- Timing zero-floor 0.25: forcing every rule to participate costs
  train 0.6304 → 0.5767 and held-out 0.5801 → ~0.533 — a collapse,
  and the constrained elite fragments across wide value ranges (an
  equivalence class of compromises, not of solutions). The timing
  zeros are structure.
- Velocity zero-floor 0.25: the same constraint costs almost nothing
  — train 0.3997 → 0.3927, held-out 0.4111–0.4113 → 0.4091–0.4117
  (overlapping). The velocity zeros are preferences.
- Timing diversity 0.05: near-optimal solutions exist with MORE
  active rules — cadence_depth 0.04 and sus_lean 0.02 turn on in
  every elite final at ~0.004 raw train cost, and parts of the elite
  reach held-out 0.5868, ABOVE the unconstrained optimum's 0.5801.
  One split, no uncertainty analysis — but it flags that the
  unconstrained train optimum is not obviously the held-out optimum.
- Velocity diversity 0.05: turns on vel_beat 1.5, dialogue_yield 1,
  seq_echo 1 at ~0.004 train and ~0.0006 held-out — again nearly
  free.

Together with the unconstrained runs: the beat-level tempo grammar is
rigid (one active allocation, forced participation is expensive), the
note-level velocity coloring is a manifold (an equivalence class in
the unconstrained elite, participation nearly free). The perceptual
condition has its targets: the two timing basins (expression 0.6 +
mid_drift vs 0.8 + open_push), and the velocity class's free
dimensions (sus_soft 2 vs 4, harm_charge 0 vs 0.15).
