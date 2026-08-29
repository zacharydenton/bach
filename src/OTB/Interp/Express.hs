-- | The expressive layer: eight literature rules under two master knobs.
--
-- The knob architecture is Director Musices' own (Sundberg/Friberg): each
-- rule keeps its quantity; families scale under global multipliers.
--
--   * exExpression — interpretive depth: dissonance emphasis (C.P.E. Bach
--     1753: dissonances louder, consonances softer; KTH harmonic charge),
--     nested tempo/dynamic arches (Todd 1992), agogic leans.
--   * exEnsemble — humans-playing-together: melody lead (Palmer 1996:
--     the principal voice anticipates by 20–30 ms), chord roll spread,
--     seeded micro-jitter (KTH/Juslin noise rules — seeded by piece name,
--     so determinism is untouched).
--
-- Style *decisions* are per-piece, not knob-scaled: notes inégales
-- (Quantz XI; Dolmetsch vs Neumann — deliberately opt-in) and overholding
-- (finger pedal; Czerny holds whole bars of BWV 846).
--
-- License: GPL-2.0-or-later.
module OTB.Interp.Express
  ( ExpressParams (..)
  , defaultExpressParams
  , inegalLane
  , overholdLane
  , chargesForLane
  , leanGate
  , seededJitter
  , seedOf
  ) where

import Data.Bits (shiftR, xor)
import Data.Char (ord)
import Data.Ratio (approxRational)
import OTB.Score (ScoreNote (..))
import OTB.Units (WholeNotes (..))

data ExpressParams = ExpressParams
  { exExpression :: !Double -- ^ master: interpretive depth family
  , exEnsemble :: !Double -- ^ master: asynchrony family
  , exDisVel :: !Double -- ^ velocity units per unit dissonance charge
  , exDisLean :: !Double -- ^ gate boost per unit charge (agogic lean)
  , exArchPiece :: !Double -- ^ Todd arch depth, whole-piece level
  , exArchGroup :: !Double -- ^ Todd arch depth, bar-group level
  , exArchBars :: !Int -- ^ bars per group at the inner level
  , exLeadMs :: !Double -- ^ melody (highest voice) anticipation
  , exRollMs :: !Double -- ^ final-chord spread per note, bass upward
  , exJitterMs :: !Double -- ^ onset jitter, 1 sigma-ish
  , exJitterVel :: !Double -- ^ velocity jitter
  , exInegal :: !Double -- ^ per piece: 0 = equal; 0.33 ~ Quantz's long-short
  , exOverhold :: !Double -- ^ per piece: 0..1 of the gap to the next onset
  }
  deriving (Show, Eq)

defaultExpressParams :: ExpressParams
defaultExpressParams = ExpressParams
  { exExpression = 1.0
  , exEnsemble = 1.0
  , exDisVel = 10
  , exDisLean = 0.10
  , exArchPiece = 0.03
  , exArchGroup = 0.02
  , exArchBars = 4
  , exLeadMs = 20
  , exRollMs = 12
  , exJitterMs = 3
  , exJitterVel = 2
  , exInegal = 0
  , exOverhold = 0
  }

-- | Notes inégales (Quantz XI): conjunct equal-duration pairs on the
-- subdivision become long–short. Applied to raw lanes so everything
-- downstream (ornaments, articulation, dynamics) sees the swung grid.
inegalLane :: Double -> [ScoreNote] -> [ScoreNote]
inegalLane r ns
  | r <= 0 = ns
  | otherwise = go ns
  where
    go (a : b : rest)
      | snDur a == snDur b
      , snDur a <= 1 / 8
      , snOnset a + snDur a == snOnset b
      , abs (snPitch a - snPitch b) <= 2 -- conjunct only: Quantz's rule
      , onBeatPair a =
          let d = snDur a
              shift = WholeNotes (toRational r) * d / 2
           in a {snDur = d + shift}
                : b {snOnset = snOnset b + shift, snDur = d - shift}
                : go rest
      | otherwise = a : go (b : rest)
    go xs = xs
    -- the long half must start the pair: even multiple of the value
    onBeatPair a =
      let WholeNotes o = snOnset a
          WholeNotes d = snDur a
       in d > 0 && even (floor (o / d) :: Integer)

-- | Overholding / finger pedal: extend each note by a fraction of the gap
-- to its lane successor (1 = legato to the next onset, Czerny-style for
-- arpeggiated textures). Runs on raw lanes, before articulation.
overholdLane :: Double -> [ScoreNote] -> [ScoreNote]
overholdLane o ns
  | o <= 0 = ns
  | otherwise = zipWith hold ns (map Just (drop 1 ns) <> [Nothing])
  where
    hold n (Just nx)
      | gap > 0 = n {snDur = snDur n + WholeNotes (toRational o) * gap}
      where
        gap = snOnset nx - (snOnset n + snDur n)
    hold n _ = n

-- | C.P.E. Bach's rule, computed from what actually sounds: for each note
-- of a lane, the dissonance charge in [0,1] against every other sounding
-- note at its onset. Minor 2nd/major 7th charge 1, major 2nd/minor 7th
-- 0.7, tritone 0.6, perfect 4th against the bass 0.3; consonances 0.
chargesForLane :: [(WholeNotes, WholeNotes, Int)] -> [ScoreNote] -> [Double]
chargesForLane others = map charge
  where
    charge n =
      let sounding =
            [ p | (o, d, p) <- others
            , o <= snOnset n, snOnset n < o + d, p /= snPitch n ]
          lowest = if null sounding then snPitch n else minimum sounding
          ivCharge p =
            case abs (snPitch n - p) `mod` 12 of
              1 -> 1.0; 11 -> 1.0
              2 -> 0.7; 10 -> 0.7
              6 -> 0.6
              5 | p == lowest || snPitch n == lowest -> 0.3
              _ -> 0.0
       in case sounding of
            [] -> 0
            ps -> maximum (map ivCharge ps)

-- | Agogic lean: dissonant notes get held toward legato. approxRational,
-- not toRational: raw Double rationals carry 2^52-ish denominators that
-- blow up downstream arithmetic (see Units.toTicks).
leanGate :: ExpressParams -> Double -> Rational -> Rational
leanGate ex ch gate =
  min 1 (gate + approxRational (exExpression ex * exDisLean ex * ch) 1e-6)

-- | Deterministic per-note jitter from a seed and note index — an xorshift
-- over (seed, i), mapped to [-1, 1]. Same piece name, same performance.
seededJitter :: Int -> Int -> Double
seededJitter seed i =
  let z0 = fromIntegral (seed * 2654435761 + i * 40503) :: Word
      z1 = z0 `xor` (z0 `shiftR` 13)
      z2 = z1 * 1274126177
      z3 = z2 `xor` (z2 `shiftR` 16)
   in fromIntegral (z3 `mod` 20001) / 10000 - 1

-- | A stable small seed from a piece name.
seedOf :: String -> Int
seedOf = foldl (\h c -> h * 131 + ord c) 7
