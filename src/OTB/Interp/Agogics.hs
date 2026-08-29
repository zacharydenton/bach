-- | Agogics: tempo as a curve, not a constant.
--
-- The SMF tempo track is *generated output* — the conductor lane. What
-- exists so far, deliberately conservative:
--
--   * **final ritardando** — over the last agRitSpan whole notes, tempo
--     follows the Friberg–Sundberg (1999) curve: modelled on stopping
--     runners, constant braking power, v(x) = (1 + (w^q − 1)·x)^(1/q)
--     with q = agRitCurve (2 = the runners' value) and w = agRitFloor.
--     Steeper at the end than a line, which is what ears rated highest
--     in their listening panels. Kills the sewing-machine ending.
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
  , agRitCurve :: !Double -- ^ q in the Friberg–Sundberg model; 2 = runners
  , agTempoStep :: !WholeNotes -- ^ granularity of the discretised curve
  , agFermataHold :: !Rational -- ^ duration multiplier under a fermata
  }
  deriving (Show, Eq)

defaultAgogicParams :: AgogicParams
defaultAgogicParams = AgogicParams
  { agRitSpan = 1 -- one whole note ≈ the final bar in 4/4
  , agRitFloor = 0.6
  , agRitCurve = 2
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
            x = realToFrac progress :: Double
            w = agRitFloor ag
            q = max 1 (agRitCurve ag)
            factor = (1 + (w ** q - 1) * x) ** (1 / q)
      ]

-- | Duration multiplier for a note's marks.
fermataFactor :: AgogicParams -> ScoreNote -> Rational
fermataFactor ag sn
  | Fermata `elem` snMarks sn = agFermataHold ag
  | otherwise = 1
