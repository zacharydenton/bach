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
  , agCadenceDepth :: !Double -- ^ slowing into a cadence arrival
  , agCadenceSpan :: !WholeNotes -- ^ how far before an easing it begins
  , agOpenPush :: !Double
    -- ^ settling in: tempo starts this fraction above base and decays
    -- to base over agOpenSpan. DISCOVERED, not cited: residual mining
    -- vs 166 ASAP performances (2026-08-31) — humans open their first
    -- two bars +5%% above their own mean, t = 10.6 clustered.
  , agOpenSpan :: !WholeNotes
  , agBoundaryEase :: !Double
    -- ^ phrase-final lengthening at strong breath boundaries, per unit
    -- of excess boundary strength. DISCOVERED: the miner's deep
    -- unexplained holds all sit at breath boundaries — humans make
    -- strong boundaries TEMPO events. This deliberately overrules the
    -- Harnoncourt principle the Phrasing module cites (per-voice
    -- breath, global time untouched): on the evidence, players do both.
  , agSubjectPush :: !Double
    -- ^ forward motion while a fugue subject sounds. DISCOVERED:
    -- +1.3%% marginal / +0.9%% joint, t = 3.6 clustered.
  }
  deriving (Show, Eq)

defaultAgogicParams :: AgogicParams
defaultAgogicParams = AgogicParams
  { agRitSpan = 1 -- one whole note ≈ the final bar in 4/4
  , agRitFloor = 0.6
  , agRitCurve = 2
  , agTempoStep = 1 / 8
  , agFermataHold = 7 / 4
  , agCadenceDepth = 0.04
  , agCadenceSpan = 3 / 4
  , agOpenPush = 0.05
  , agOpenSpan = 2
  , agBoundaryEase = 0.05
  , agSubjectPush = 0.015
  }

-- | The piece's tempo curve. Multiplicative layers on the base: one
-- Todd (1992) arch per node of the grouping hierarchy (tempo and
-- loudness co-vary: faster toward centres, easing at boundaries),
-- phrase-final slowing into each cadence arrival, then the
-- Friberg–Sundberg closing rit. Pairs are (onset, tempo-from-here);
-- the whole curve is discretised at agTempoStep.
--
-- Arches come in as (start, end, fractional depth) — the caller owns
-- the hierarchy (piece level, recursive grouping tree, bar groups);
-- 0.03 = ±3%. Easings are (arrival onset, depth) pairs — cadences and
-- strong breath boundaries alike: over agCadenceSpan before each,
-- tempo eases by up to the depth, recovering at the arrival (the next
-- phrase starts a tempo — phrase-final lengthening, not a global
-- rit). Subject spans get agSubjectPush of forward motion; the
-- opening gets agOpenPush, decaying over agOpenSpan.
tempoMap :: AgogicParams -> [(WholeNotes, WholeNotes, Double)]
         -> [(WholeNotes, Double)] -> [(WholeNotes, WholeNotes)]
         -> Bpm -> WholeNotes -> [(WholeNotes, Bpm)]
tempoMap ag arches easings subjSpans (Bpm base) end
  | end <= 0 || agTempoStep ag <= 0 = [(0, Bpm base)]
  | otherwise = thin [(t, Bpm (base * factor t)) | t <- gridPts]
  where
    gridPts = takeWhile (< end) (iterate (+ agTempoStep ag) 0)
    -- the total factor is floored: however hostile the (validated-per-
    -- knob but unbounded-in-product) configuration, tempo stays positive
    factor t = max 0.1 (product [arch d a b t | (a, b, d) <- arches]
                          * easeF t * openF t * subjF t * ritF t)
    arch depth0 a b t
      | depth <= 0 || b <= a || t < a || t > b = 1
      | otherwise =
          let x = realToFrac ((t - a) / (b - a)) :: Double
           in 1 + depth * (4 * x * (1 - x) * 2 - 1) -- ±depth, peak centre
      where
        -- effective depth is expression x arch_*: clamp the PRODUCT
        -- here, where it exists (per-knob validation cannot bound it)
        depth = min 0.9 depth0
    easeF t
      | agCadenceSpan ag <= 0 = 1
      | otherwise =
          product
            [ 1 - min 0.9 depth * ramp
            | (c, depth) <- easings
            , depth > 0
            , t >= c - agCadenceSpan ag, t < c
            , let WholeNotes pr = (t - (c - agCadenceSpan ag))
                                    / agCadenceSpan ag
                  ramp = realToFrac pr :: Double ]
    openF t
      | agOpenPush ag <= 0 || agOpenSpan ag <= 0 || t >= agOpenSpan ag = 1
      | otherwise =
          let WholeNotes x = t / agOpenSpan ag
           in 1 + min 0.5 (agOpenPush ag) * (1 - realToFrac x)
    subjF t
      | agSubjectPush ag <= 0 = 1
      | any (\(s0, s1) -> t >= s0 && t < s1) subjSpans =
          1 + min 0.5 (agSubjectPush ag)
      | otherwise = 1
    ritF t
      | agRitSpan ag <= 0 || end <= agRitSpan ag || t < start = 1
      | otherwise =
          let WholeNotes progress = (t - start) / agRitSpan ag
              x = min 1 (realToFrac progress) :: Double
              w = max 0.05 (agRitFloor ag)
              q = max 1 (agRitCurve ag)
           in (1 + (w ** q - 1) * x) ** (1 / q)
    start = end - agRitSpan ag
    -- collapse runs of equal tempo so flat stretches stay one event
    thin ((t1, b1) : (t2, b2) : rest)
      | b1 == b2 = thin ((t1, b1) : rest)
      | otherwise = (t1, b1) : thin ((t2, b2) : rest)
    thin xs = xs

-- | Duration multiplier for a note's marks. A fermata holds the tie
-- segment that carries it — on a tied close, only the close — so the
-- factor is the held duration over the notated one.
fermataFactor :: AgogicParams -> ScoreNote -> Rational
fermataFactor ag sn
  | Fermata `notElem` snMarks sn = 1
  | snDur sn <= 0 = agFermataHold ag
  | otherwise =
      let held = sum [ d * (if Fermata `elem` ms then agFermataHold ag else 1)
                     | (WholeNotes d, ms) <- snSegs sn ]
          WholeNotes total = snDur sn
       in held / total
