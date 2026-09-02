# Envelope: landscape-velocity-2026-09-02-5bf7cb3a31cd (LEGACY FORMAT)

This artifact was minted by the pre-registration binary and is
preserved as a legacy raw record, NOT as a newly registered manifest:
its filename carries the old 12-character checkpoint fingerprint, it
has no producer block, and it predates the atomic-write and
build/runtime cross-check regime.

## Recovered provenance

- File SHA-256:
  c8b7f842b4123274b2971a6c9c4784bf1547b39170c425d8b1e4f1899a884b90
- Its `digests.binary` is byte-identical to that of
  landscape-timing-2026-09-02-d3a8a0a9ecbf — the same executable,
  whose producer is recorded in that experiment's notes: **built at
  commit 95d5fba, clean tree**, ghc-9.8.4, linux/x86_64.
- The config it evaluated is NOT the timing run's: its
  `digests.prefit_config` corresponds to the c4dd5ac global refit
  (expression 0.6 et al.), applied between the two runs. Binary from
  95d5fba, experiment against the c4dd5ac config state — both facts
  recoverable from the digests, neither stamped by the minting path,
  which is why this stays legacy.

## The result it records (unconstrained condition, velocity family)

20/20 starts within slack (train 0.3995–0.3997, test 0.4111–0.4113):
**the elite is a class, not a point** — the first observed
equivalence class in this program. Shared backbone: vel_highloud 0.8,
dis_vel 5, subject_vel 10, dialogue_vel 2; all metrical accents
(vel_bar/halfbar/beat/arch) and mel_charge, dialogue_yield, seq_echo
at zero. Free dimensions inside the elite: **sus_soft ∈ {2, 4}** and
**harm_charge ∈ {0, 0.15}**, with held-out spread 0.0002 — the human
velocity data does not distinguish these allocations.

Note the tension with the committed config: dis_vel = 0 and
sus_soft = 0 were fitted at note level under expression 1.0
(2026-09-01); this landscape, evaluated against the c4dd5ac baseline
(expression 0.6), turns both ON. The families interact — reduced
agogic/emphasis intensity leaves velocity room for the dissonance
rules. Any velocity global refit should wait for the condition runs
and be taken as deliberately as the timing one was.
