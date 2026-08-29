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

-- | The piece's tempo curve. Three multiplicative layers on the base:
-- Todd (1992) arches at whole-piece and bar-group level (tempo and
-- loudness co-vary: faster toward centres, easing at boundaries), then
-- the Friberg–Sundberg closing rit. Pairs are (onset, tempo-from-here);
-- the whole curve is discretised at agTempoStep.
--
-- archPiece/archGroup are fractional depths (0.03 = ±3%); groupBars
-- converts through the meter's bar length (whole notes per bar).
tempoMap :: AgogicParams -> Double -> Double -> WholeNotes -> Bpm -> WholeNotes
         -> [(WholeNotes, Bpm)]
tempoMap ag archPiece archGroup groupLen (Bpm base) end
  | end <= 0 = [(0, Bpm base)]
  | otherwise = thin [(t, Bpm (base * factor t)) | t <- gridPts]
  where
    gridPts = takeWhile (< end) (iterate (+ agTempoStep ag) 0)
    factor t = arch archPiece 0 end t * groupArch t * ritF t
    arch depth a b t
      | depth <= 0 || b <= a = 1
      | otherwise =
          let x = realToFrac ((t - a) / (b - a)) :: Double
           in 1 + depth * (4 * x * (1 - x) * 2 - 1) -- ±depth, peak centre
    groupArch t
      | archGroup <= 0 || groupLen <= 0 = 1
      | otherwise =
          let WholeNotes tw = t
              WholeNotes gw = groupLen
              a = fromRational (gw * fromIntegral (floor (tw / gw) :: Integer))
           in arch archGroup (WholeNotes (toRational a))
                (WholeNotes (toRational a) + groupLen) t
    ritF t
      | agRitSpan ag <= 0 || end <= agRitSpan ag || t < start = 1
      | otherwise =
          let WholeNotes progress = (t - start) / agRitSpan ag
              x = min 1 (realToFrac progress) :: Double
              w = agRitFloor ag
              q = max 1 (agRitCurve ag)
           in (1 + (w ** q - 1) * x) ** (1 / q)
    start = end - agRitSpan ag
    -- collapse runs of equal tempo so flat stretches stay one event
    thin ((t1, b1) : (t2, b2) : rest)
      | b1 == b2 = thin ((t1, b1) : rest)
      | otherwise = (t1, b1) : thin ((t2, b2) : rest)
    thin xs = xs

-- | Duration multiplier for a note's marks.
fermataFactor :: AgogicParams -> ScoreNote -> Rational
fermataFactor ag sn
  | Fermata `elem` snMarks sn = agFermataHold ag
  | otherwise = 1
