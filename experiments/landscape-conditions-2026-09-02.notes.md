# The conditions program, first pass (2026-09-02)

Five 20-start runs against the c4dd5ac config state (expression 0.6
baseline). Registration bookkeeping first, then what the data says.

## Artifact status

| run | condition | artifact | status |
|-----|-----------|----------|--------|
| 1 | velocity unconstrained | …velocity-…-5bf7cb3a31cd.json | legacy (95d5fba binary), enveloped |
| 2 | timing zero-floor 0.25 | …timing-…-c8caf6c9….json | **registered** (292a83e, producer block) |
| 3 | timing diversity 0.05 | …timing-…-aec576ff436b.json | legacy: preserved from checkpoints |
| 4 | velocity zero-floor 0.25 | …velocity-…-02fc49ba….json | **registered** (rerun under the fixed binary; aggregates match the archived log exactly) |
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
per-knob report survives in the run log, and the rerun under the
fixed binary registered cleanly, reproducing the lost aggregates
exactly (see the table).

sha256 of preserved files:
- landscape-timing-2026-09-02-aec576ff436b.json:
  0ed83a8fc32fb0f697bac9ff591e7ba385e684ce7a0e7abec03eff5d570e1854
- landscape-velocity-2026-09-02-ce203c2a0199.json:
  9f10dd4c3d1400f7f21789fae10cd2d85b43009e233a44986ac10bdd736c84c9

## The verdict the five runs converge on

**Forcing all timing rules on is costly; forcing all velocity rules
on is nearly free.** (A collective claim: the zero-floor condition
floors every zero simultaneously, so it cannot apportion the cost to
individual rules — the diversity run already shows cadence_depth and
sus_lean can activate cheaply. Per-rule verdicts require the
one-at-a-time ablations registered separately as the floor-only
series.)

- Timing zero-floor 0.25: forcing every rule to participate costs
  train 0.6304 → 0.5767 and held-out 0.5801 → ~0.533 — a collapse,
  and the constrained elite fragments across wide value ranges. The
  all-on timing model is harmful; which zeros carry that cost is the
  floor-only series' question.
- Velocity zero-floor 0.25: the same constraint costs almost nothing
  — train 0.3997 → 0.3927, held-out 0.4111–0.4113 → 0.4091–0.4117
  (overlapping). The all-on velocity model is essentially free.
- Timing diversity 0.05: near-optimal solutions exist with MORE
  active rules — cadence_depth 0.04 and sus_lean 0.02 turn on in
  every elite final at ~0.004 raw train cost, and parts of the elite
  reach held-out 0.5868, ABOVE the unconstrained optimum's 0.5801.
  One split, no uncertainty analysis — but it flags that the
  unconstrained train optimum is not obviously the held-out optimum.
- Velocity diversity 0.05: turns on vel_beat 1.5, dialogue_yield 1,
  seq_echo 1 at ~0.004 train and ~0.0006 held-out — again nearly
  free.

Together with the unconstrained runs: forced participation is
expensive at beat level and nearly free at note level; the velocity
elite spreads across near-flat discrete directions on this lattice
(grouped by the prespecified 0.005 slack — see the statistical
follow-up in the velocity envelope before reading "the data cannot
distinguish them" into that). The perceptual
condition has its targets: the two timing basins (expression 0.6 +
mid_drift vs 0.8 + open_push), and the velocity class's free
dimensions (sus_soft 2 vs 4, harm_charge 0 vs 0.15).

## Statistical follow-up: the velocity free dimensions (2026-09-02)

Paired bootstrap (10k samples, clustered by piece) over the 94
held-out performances, comparing backbone configs differing in one
free dimension, at the prefit level:

- sus_soft 2 vs 4:      mean diff +0.00001, 95% CI [-0.00035, +0.00035]
- harm_charge 0 vs .15: mean diff +0.00003, 95% CI [-0.00031, +0.00037]

The near-flat directions are statistically flat on this (development
— see below) held-out set: intervals an order of magnitude tighter
than the timing basins' and symmetric about zero. As operational
language: the human velocity data, as this program can measure it,
carries no preference along either dimension.

## Book II's status (as of these runs)

Held-out scores have now been inspected across five conditions x 20
starts, used in a basin bootstrap, and consulted when naming
perceptual targets. **Book II is therefore validation/development
data, not a confirmatory test set.** Any publishable final estimate
needs nested validation inside Book I for selection steps, and a
genuinely untouched corpus for confirmation — the natural candidate
is the transcription campaign's 22 orphan pieces, which have no human
data in any analysis to date. Until then, every held-out number in
this program is a development number.
