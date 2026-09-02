# Bootstrap analyses of 2026-09-02: inputs, method, outputs

Two paired-bootstrap analyses referenced by the experiment notes
(timing basins A/B; velocity free dimensions sus_soft and
harm_charge). This directory preserves the exact inputs and records
the method; the analysis itself was performed with a scratch script
(python3 stdlib `random`), and is being replaced by a repo-native
`otb bootstrap` subcommand so future runs regenerate from HEAD.

## Method (as run)

- Observations: per-performance r rows from `otb eval` (`--velocity`
  for the velocity comparisons), config = the paired .toml files
  here (prefit-level: PIECE-FIT lines and banner stripped from
  config/default.toml at the then-HEAD state, plus the appended
  overrides visible at each file's end).
- Pairing: rows keyed by (piece, performer); held-out set = pieces
  with the `wtc2` prefix (Book II — development data, see the
  conditions notes); n = 94 pairs in each comparison.
- Statistic: mean of paired differences (A - B).
- Interval: percentile bootstrap, 10,000 resamples, seed
  `random.Random(1)`, `rng.choices` with replacement; two variants —
  resampling performances directly, and resampling PIECES (clusters)
  with all their performances (the reported clustered CIs).
- Reported: mean difference, 2.5%/97.5% percentiles, and (for the
  basins) P(diff <= 0) as the bootstrap fraction.

## Corrections after review (2026-09-02, same day)

1. ESTIMAND: the velocity comparisons below were first computed as the
   mean of per-performance differences, but the velocity landscape
   optimizes the MEAN OF PER-PIECE MEDIANS. Recomputed under the
   landscape's own statistic (piece-resampled, 10k, seed 1):
   - sus_soft 2 vs 4:      point +0.000030, CI [-0.000332, +0.000385]
   - harm_charge 0 vs .15: point +0.000185, CI [-0.000311, +0.000703]
   These supersede the per-performance velocity numbers for any claim
   about the landscape objective; the substantive conclusion (flat)
   is unchanged. (The timing basin comparison needed no correction:
   the timing objective IS the flat mean over performances.)

2. CROSSED DEPENDENCE: the 94 held-out rows carry only 48 performer
   IDs (39 span multiple pieces, 85 rows), so observations are
   crossed by piece and pianist while the original intervals cluster
   only by piece. One-way performer-clustered intervals (mean-diff
   statistic, 10k, seed 1):
   - basin A vs B:         [-0.00508, +0.00599]
   - sus_soft 2 vs 4:      [-0.00017, +0.00019]
   - harm_charge 0 vs .15: [-0.00023, +0.00032]
   These are ONE-WAY SENSITIVITY ANALYSES, not bounds: with crossed
   piece and pianist effects, the wider of two one-way intervals is
   not an upper bound on a two-way interval. (Note also the two
   clusterings here use different statistics — the performer-
   clustered velocity intervals predate the estimand correction and
   use the per-performance mean — so their widths are not directly
   comparable.) Paper-grade intervals over crossed random effects
   need a two-way cluster bootstrap, or a declaration of which
   population — pieces or pianists — is treated as fixed.

## Results (as recorded in the notes)

- basin A vs B (timing, tempo r): mean +0.0006,
  95% CI per-performance [-0.0054, +0.0061],
  clustered by piece [-0.0091, +0.0089], P(diff<=0) 0.41-0.42.
- sus_soft 2 vs 4 (velocity r): mean +0.00001,
  clustered CI [-0.00035, +0.00035].
- harm_charge 0 vs 0.15 (velocity r): mean +0.00003,
  clustered CI [-0.00031, +0.00037].

## Input digests (sha256)

- basinA.toml: c893bd01b0202da219f69e042300e2640c03c0a8c57311f9e4fc50bb56aab65a
- basinB.toml: 57d1f27a5cd1f21c941f334576b4bcd52c6578eee6f7808d8eea394ddb70d756
- eval-basinA.tsv: 04d603ae5ca89be4529733a52dd3d78c2cd298782c4dc63d11ed1bb98a867c1b
- eval-basinB.tsv: 1ce2c2b503b0941926ebc9b5e1a2b779e8930ecdb832e34fe685335f691e747c
- v-harm0.toml: 1c3f6554193e1c51264dbe62e7718ea7c3fb08b43e6dd84729b92b4a3ea43f27
- v-harm15.toml: 90bb17342e2bb5a58a663789ee860180aac4264aef7219cd778f060d5b55919d
- v-sus2.toml: 1c3f6554193e1c51264dbe62e7718ea7c3fb08b43e6dd84729b92b4a3ea43f27
- v-sus4.toml: e42f4c67fb3f1d827eb5271fbb66b780452b2391393f38c2b6dd02d007e48f32
- veval-harm0.tsv: e0e656be1ba09e334915c503ec81f198bbe47ee1a205ab1c9d625177a1146dd2
- veval-harm15.tsv: fd49bfc90905ae911c012c03d2120c28a3b8ec7d60f994c3d38b810276d0d3d4
- veval-sus2.tsv: e0e656be1ba09e334915c503ec81f198bbe47ee1a205ab1c9d625177a1146dd2
- veval-sus4.tsv: 12e2b83d4c9179990218c9c37c35a58a4f47750c1cf964212f9ebbed68ffdf9c
