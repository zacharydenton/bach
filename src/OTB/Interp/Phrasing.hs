-- | Breath placement from the literature, not from guesswork.
--
-- Sources, in order of weight:
--
--   * Quantz, /Versuch/ (1752), on where a player may breathe: at rests,
--     after long notes, at leaps — never splitting stepwise figures.
--   * The KTH rule system (Frydén, Sundberg, Friberg; Director Musices):
--     the **Punctuation rule** "automatically locates small tone groups
--     and marks them with lengthening of last note and a micropause."
--   * Cambouropoulos's LBDM: boundary salience from local change —
--     gaps, duration contrast, pitch leaps — as a weighted sum.
--   * Harnoncourt, /Musik als Klangrede/: in polyphony the texture
--     breathes **per voice**; global time stays steadier. Hence
--     micropauses here are per-lane gate cuts, and the tempo map is not
--     touched by phrasing.
--
-- A boundary is scored after every note from three normalised features:
-- the written rest gap that follows (Quantz: rests), the note's length
-- relative to its predecessors (Quantz: after long notes), and the leap
-- to the next note (Quantz: at leaps). Where the weighted sum clears the
-- threshold, the note ends early by ppBreath of its sounding length —
-- the KTH micropause, stolen from the note that precedes the boundary.
--
-- License: GPL-2.0-or-later.
module OTB.Interp.Phrasing
  ( PhraseParams (..)
  , defaultPhraseParams
  , breatheLane
  , breatheLane'
  , boundaryStrengths
  ) where

import OTB.Explain (Why, why)
import OTB.Score (ScoreNote (..))
import OTB.Units (WholeNotes (..))

data PhraseParams = PhraseParams
  { ppThreshold :: !Double -- ^ boundary strength needed for a breath
  , ppBreath :: !Rational -- ^ fraction of sounding length given to silence
  , ppWGap :: !Double -- ^ weight: written rest after the note
  , ppWDur :: !Double -- ^ weight: long note after shorter ones
  , ppWLeap :: !Double -- ^ weight: leap to the next note
  , ppWCadence :: !Double -- ^ bonus at harmonic cadences (V-I arrivals)
  }
  deriving (Show, Eq)

defaultPhraseParams :: PhraseParams
defaultPhraseParams = PhraseParams
  { ppThreshold = 1.0
  , ppBreath = 1 / 4
  , ppWGap = 1.2 -- a written rest is already a breath: strongest signal
  , ppWDur = 0.7
  , ppWLeap = 0.5
  , ppWCadence = 0.8 -- a V-I clause ending is a boundary even when the
                     -- surface is smooth (Quantz on musical commas)
  }

-- | Weighted boundary strength *after* each note of a chronological lane.
boundaryStrengths :: PhraseParams -> [ScoreNote] -> [Double]
boundaryStrengths pp ns = zipWith3 strength ns nexts prevs
  where
    nexts = map Just (drop 1 ns) <> [Nothing]
    prevs = Nothing : map Just ns
    strength n next prev =
      ppWGap pp * gapF + ppWDur pp * durF + ppWLeap pp * leapF
      where
        gapF = case next of
          Just nx ->
            let WholeNotes g = snOnset nx - (snOnset n + snDur n)
                WholeNotes d = snDur n
             in if g > 0 then realToFrac (g / max d (1 / 64)) else 0
          Nothing -> 0
        durF = case prev of
          Just pv
            | snDur n > snDur pv ->
                let WholeNotes r = snDur n / max (snDur pv) (WholeNotes (1 / 64))
                 in realToFrac r - 1
          _ -> 0
        leapF = case next of
          Just nx -> fromIntegral (abs (snPitch nx - snPitch n)) / 12
          Nothing -> 0

-- | Apply Punctuation to an articulated lane: where a boundary clears the
-- threshold, the note before it gives ppBreath of its sounding length to
-- silence. The lane's final note is left alone — the final rit owns
-- endings.
breatheLane :: PhraseParams -> [ScoreNote] -> [(ScoreNote, Rational)] -> [(ScoreNote, Rational)]
breatheLane pp raw = map (\(n, g, _) -> (n, g)) . breatheLane' pp (map (const 0) raw) raw

-- | The same rule with provenance — the annotator's view. The Maybe Why is
-- Nothing on notes that do not breathe. The bonus list (one per note)
-- carries strengths the lane surface cannot see — cadence arrivals from
-- the harmony model.
breatheLane' :: PhraseParams -> [Double] -> [ScoreNote] -> [(ScoreNote, Rational)]
             -> [(ScoreNote, Rational, Maybe Why)]
breatheLane' pp bonus raw arts =
  zipWith3 apply arts strengths lastFlags
  where
    strengths = zipWith (+) (boundaryStrengths pp raw) (bonus <> repeat 0)
    lastFlags = map (const False) (drop 1 arts) <> [True]
    apply (sn, gate) s isLast
      | not isLast, s >= ppThreshold pp =
          ( sn, gate * (1 - ppBreath pp)
          , Just (why "breath"
              ("micropause: gate x" <> show (fromRational (1 - ppBreath pp) :: Double)
                 <> ", boundary strength " <> show (fromIntegral (round (s*100)) / 100 :: Double))
              "Quantz XI; KTH Punctuation") )
      | otherwise = (sn, gate, Nothing)
