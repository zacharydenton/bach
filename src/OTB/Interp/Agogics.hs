-- | Agogics: tempo as a curve, not a constant.
--
-- The SMF tempo track is *generated output* — the conductor lane. What
-- exists so far, deliberately conservative:
--
--   * **final ritardando** — over the last apRitSpan whole notes, tempo
--     eases linearly to apRitFloor of base, discretised at apTempoStep so
--     the slide is smooth. Kills the sewing-machine ending.
--   * **fermata hold** — a note carrying the @;@ mark sounds
--     apFermataHold times its notated length. In the WTC corpus fermatas
--     sit on final chords; a mid-piece fermata would overlap its
--     successors rather than pushing them (a global pause needs the tempo
--     map, and earns it when repertoire demands one).
--
-- Phrase-boundary breaths and cadence detection are where listening
-- drives the parameters; the machinery lands with them in a later pass.
--
-- License: GPL-2.0-or-later.
module OTB.Interp.Agogics
  ( AgogicParams (..)
  , defaultAgogicParams
  , tempoMap
  , fermataFactor
  ) where

import OTB.Kern.Token (Mark (..))
import OTB.Score (ScoreNote (..))
import OTB.Units (Bpm (..), WholeNotes (..))

data AgogicParams = AgogicParams
  { agRitSpan :: !WholeNotes -- ^ length of the closing ritardando
  , agRitFloor :: !Double -- ^ tempo multiplier reached at the final note
  , agTempoStep :: !WholeNotes -- ^ granularity of the discretised curve
  , agFermataHold :: !Rational -- ^ duration multiplier under a fermata
  }
  deriving (Show, Eq)

defaultAgogicParams :: AgogicParams
defaultAgogicParams = AgogicParams
  { agRitSpan = 1 -- one whole note ≈ the final bar in 4/4
  , agRitFloor = 0.6
  , agTempoStep = 1 / 8
  , agFermataHold = 7 / 4
  }

-- | The piece's tempo curve: base tempo from zero, then stepped easing
-- across the closing span. Pairs are (onset, tempo-from-here).
tempoMap :: AgogicParams -> Bpm -> WholeNotes -> [(WholeNotes, Bpm)]
tempoMap ag (Bpm base) end
  | agRitSpan ag <= 0 || end <= agRitSpan ag = [(0, Bpm base)]
  | otherwise = (0, Bpm base) : steps
  where
    start = end - agRitSpan ag
    stepList = takeWhile (< end) (iterate (+ agTempoStep ag) start)
    steps =
      [ (t, Bpm (base * factor))
      | t <- stepList
      , let WholeNotes progress = (t - start) / agRitSpan ag
            factor = 1 + (agRitFloor ag - 1) * realToFrac progress
      ]

-- | Duration multiplier for a note's marks.
fermataFactor :: AgogicParams -> ScoreNote -> Rational
fermataFactor ag sn
  | Fermata `elem` snMarks sn = agFermataHold ag
  | otherwise = 1
