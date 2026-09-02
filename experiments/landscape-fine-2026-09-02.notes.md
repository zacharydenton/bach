# The fine lattice: two zeros were resolution artifacts

20-start unconstrained run on the extended lattice
(landscape-timing-2026-09-02-520fbb29….json, registered; arch_group,
cadence_depth and novelty_brake gained sub-grid steps at the values
the floor-only ablations pointed to).

| | coarse lattice | fine lattice |
|---|---|---|
| best train | 0.6304 | **0.6343** |
| held-out   | 0.5801 | 0.5833–0.5860 |
| elite      | 17/20  | **20/20** |

- **novelty_brake 0.005 in all 20 finals** — its coarse zero existed
  only because 0.02 was the smallest expressible step.
- **cadence_depth stays 0 in all 20** even with 0.01/0.02 available:
  its zero is real, not resolution.
- boundary_ease, open_push, subject_push, sus_lean: zeros hold.
- Two directions of spread, bootstrapped (10k, piece-clustered, dev
  data as ever; inputs in the run log and regenerable via
  otb bootstrap):
  - expression 0.6 vs 0.4: point −0.0002, CI [−0.0021, +0.0017] —
    genuinely flat, a new free direction.
  - arch_group 0 vs 0.0025: point −0.0028, **CI [−0.0049, −0.0007]**,
    P(diff≤0) 0.995 — the program's FIRST zero-excluding interval:
    held-out prefers the whisper of group arch the coarse lattice
    could not express.

Deployed accordingly (novelty_brake 0.005, arch_group 0.0025; piece
fits re-shrunk, 428 keys): tempo grand mean 0.6564 → 0.6592.
