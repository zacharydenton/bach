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
  , uphillLane
  , doubleDurLane
  , chargesForLane
  , leanGate
  , seededJitter
  , seededJitter1f
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
  , exMelCharge :: !Double -- ^ vel per unit melodic charge (Friberg 1991)
  , exHarmCharge :: !Double -- ^ vel per unit harmonic charge
  , exSubjectVel :: !Double -- ^ vel boost across fugue subject entries
  , exDurContrast :: !Double -- ^ KTH duration contrast: short-note gate cut
  , exLeapPause :: !Double -- ^ KTH leap articulation: micropause before leaps
  , exLeapDur :: !Double -- ^ KTH leap tone duration: hold the arrival
  , exUphill :: !Double -- ^ KTH faster uphill: rushing ascending runs
  , exDoubleDur :: !Double -- ^ KTH double duration: soften 2:1 contrast
  , exDialogueVel :: !Double
    -- ^ presence for the voice that takes the floor in cross-voice
    -- imitation (Harnoncourt's Klangrede: polyphony is dialogue;
    -- Czerny marks emphasis at every entry, not only the subject's)
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
  , exMelCharge = 0.4
  , exHarmCharge = 0.3
  , exSubjectVel = 5
  , exDurContrast = 0.12
  , exLeapPause = 0.06
  , exLeapDur = 0.05
  , exUphill = 0.03
  , exDoubleDur = 0.07
  , exDialogueVel = 4
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

-- | KTH "faster uphill" (Friberg, Bresin & Sundberg 2006): within an
-- ascending stepwise run of three or more equal values, each interior
-- note gives k of its duration to the run's final note — the run rushes
-- slightly toward its top. Lane total duration is preserved.
uphillLane :: Double -> [ScoreNote] -> [ScoreNote]
uphillLane k0 ns
  | k <= 0 = ns
  | otherwise = concatMap reshape (runs ns)
  where
    -- clamp the EFFECTIVE quantity: expression scales this rule's k
    -- upstream, so per-knob config validation cannot bound the product
    k = min 0.4 k0
    runs xs = case xs of
      [] -> []
      (a : _) ->
        let (r, more) = span' a xs
         in r : runs more
    span' _ [] = ([], [])
    span' _ (x : xs) = go [x] xs
      where
        go acc (y : ys)
          | up (last acc) y = go (acc <> [y]) ys
        go acc rest' = (acc, rest')
    up a b =
      let iv = snPitch b - snPitch a
       in iv > 0 && iv <= 2
            && snDur a == snDur b
            && snOnset a + snDur a == snOnset b
    reshape r
      | length r < 3 = r
      | otherwise =
          let d = WholeNotes (toRational k) * snDur (head r)
              interior = length r - 1
              stolen = d * fromIntegral interior
              shifted =
                [ n { snOnset = snOnset n - d * fromIntegral i'
                    , snDur = snDur n - d }
                | (i', n) <- zip [(0 :: Int) ..] (init r) ]
              lastN = last r
           in shifted
                <> [ lastN { snOnset = snOnset lastN - stolen
                           , snDur = snDur lastN + stolen } ]

-- | KTH "double duration" (Friberg, Bresin & Sundberg 2006): in an
-- adjacent 2:1 pair the contrast is softened — the short note takes k
-- of its length from the long one. Runs on raw lanes like inégales.
doubleDurLane :: Double -> [ScoreNote] -> [ScoreNote]
doubleDurLane k0 ns
  | k <= 0 = ns
  | otherwise = go ns
  where
    k = min 0.45 k0 -- past that the pair inverts
    go (a : b : rest)
      | snDur a == 2 * snDur b
      , snOnset a + snDur a == snOnset b =
          let d = WholeNotes (toRational k) * snDur b
           in a {snDur = snDur a - d}
                : b {snOnset = snOnset b - d, snDur = snDur b + d}
                : go rest
      | otherwise = a : go (b : rest)
    go xs = xs

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

-- | 1\/f-flavoured timing noise (Gilden, Thornton & Mallon 1995: human
-- timing residuals are fractal, not white): three octaves of the seeded
-- noise summed, the slower components shared across neighbouring notes.
seededJitter1f :: Int -> Int -> Int -> Double
seededJitter1f seed lane i =
  -- the index must arrive UNSTRIDED: the div-4/div-16 octaves only
  -- share slow components when consecutive notes have consecutive i.
  -- Lane identity enters as a seed salt instead.
  ( seededJitter s i
      + 0.7 * seededJitter (s + 101) (i `div` 4)
      + 0.5 * seededJitter (s + 707) (i `div` 16) )
    / 2.2
  where
    s = seed + lane * 7919

-- | A stable small seed from a piece name.
seedOf :: String -> Int
seedOf = foldl (\h c -> h * 131 + ord c) 7
