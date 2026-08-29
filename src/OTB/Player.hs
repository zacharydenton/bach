-- | Score -> Performance, via Euterpea's Music algebra.
--
-- Each voice's notes are rebuilt into monophonic *lanes* (the spine paths
-- they came from), articulation is resolved per lane where line context
-- exists, and each lane becomes a @Music (pitch, gate)@ line whose
-- traversal yields the performed events.
--
-- **The lane, not the voice, is the monophonic unit — so the lane gets
-- the MIDI channel.** Pitch bend is per channel, and temperament needs a
-- distinct bend per sounding note; a voice whose sub-spines hold a chord
-- would smear one bend across it. Channel 9 (GM percussion) is skipped
-- for the audition path's sake; more than 15 simultaneous lanes is a
-- cardinality error, reported, not truncated.
--
-- License: GPL-2.0-or-later.
module OTB.Player
  ( PerfNote (..)
  , Performance (..)
  , Interp (..)
  , defaultInterp
  , perform
  ) where

import Data.List (findIndex, nub, sort, sortOn)
import Data.Ratio (approxRational)
import EuterpeaLite.Music (Music (..), Primitive (..), note, rest)
import OTB.Config (ArtParams, defaultArtParams)
import OTB.Interp.Agogics
  (AgogicParams, defaultAgogicParams, fermataFactor, tempoMap)
import OTB.Interp.Articulation (articulateLane)
import OTB.Interp.Dynamics (DynParams, defaultDynParams, dynamicsLane)
import OTB.Interp.Express
import OTB.Interp.Ornament
  (OrnamentParams, defaultOrnamentParams, realizeLane)
import OTB.Interp.Phrasing
  (PhraseParams (..), boundaryStrengths, breatheLane, defaultPhraseParams)
import OTB.Score
import OTB.Tuning (TuningTable, bendValue, offsetFor, werckmeister3)
import OTB.Units (Bpm (..), WholeNotes (..))

-- | Everything the Player needs beyond the score: the interpretation.
data Interp = Interp
  { iArt :: !ArtParams
  , iAgogics :: !AgogicParams
  , iPhrasing :: !PhraseParams
  , iOrnaments :: !OrnamentParams
  , iDynamics :: !DynParams
  , iExpress :: !ExpressParams
  , iPiece :: !String -- ^ seeds the deterministic jitter
  , iTuning :: !TuningTable
  , iBendRange :: !Double
  }

defaultInterp :: Interp
defaultInterp =
  Interp defaultArtParams defaultAgogicParams defaultPhraseParams
    defaultOrnamentParams defaultDynParams defaultExpressParams ""
    werckmeister3 2

data PerfNote = PerfNote
  { pnOnset :: !WholeNotes
  , pnDur :: !WholeNotes -- ^ sounding (post-articulation) duration
  , pnPitch :: !Int
  , pnVel :: !Int
  , pnBend :: !Int -- ^ 14-bit, emitted on the note's channel before it sounds
  , pnChannel :: !Int
  }
  deriving (Show)

data Performance = Performance
  { perfTempoMap :: [(WholeNotes, Bpm)] -- ^ the conductor lane, generated
  , perfTracks :: [[PerfNote]] -- ^ one track per voice, onset-sorted
  }
  deriving (Show)

-- | One voice's notes into monophonic lanes. The parser recorded which
-- spine path each note came from, so a note goes to *its own* lane — a
-- line keeps its identity across rests instead of being re-guessed. Only
-- a chord token makes a path polyphonic; its extra notes go to *overflow*
-- lanes (reused when free, created when not) — never to another spine's
-- lane, which would hand a chord tone to a line it was not part of.
lanes :: [ScoreNote] -> [[ScoreNote]]
lanes ns =
  map (reverse . snd) (foldl place seed (sortOn snOnset ns))
  where
    seed = [(Just l, []) | l <- sort (nub (map snLane ns))]
    place ls sn
      | Just i <- findIndex (\(l, xs) -> l == Just (snLane sn) && free xs) ls
          = push i
      | Just i <- findIndex (\(l, xs) -> l == Nothing && free xs) ls = push i
      | otherwise = ls <> [(Nothing, [sn])]
      where
        free xs = case xs of
          (prev : _) -> snOnset prev + snDur prev <= snOnset sn
          [] -> True
        push i = [ if j == i then (l, sn : xs) else (l, xs)
                 | (j, (l, xs)) <- zip [0 :: Int ..] ls ]

-- | An articulated lane as a Music line: rests for the gaps, payload
-- (pitch, gate). Durations are already whole notes — Euterpea's Dur —
-- so no time conversion happens here.
laneMusic :: [(ScoreNote, Rational)] -> Music (Int, Rational)
laneMusic = go 0
  where
    go _ [] = rest 0
    go t ((sn, gate) : rest') =
      let gap = realToFrac (snOnset sn - t)
          n = note (realToFrac (snDur sn)) (snPitch sn, gate)
       in rest gap :+: n :+: go (snOnset sn + snDur sn) rest'

-- | Traverse a Music value back to timed events. Pure, total.
musicEvents :: Rational -> Music (Int, Rational) -> [(Rational, Rational, (Int, Rational))]
musicEvents t m = case m of
  Prim (Note d p) -> [(t, d, p)]
  Prim (Rest _) -> []
  a :+: b -> musicEvents t a <> musicEvents (t + durOf a) b
  a :=: b -> musicEvents t a <> musicEvents t b
  Modify _ inner -> musicEvents t inner

durOf :: Music a -> Rational
durOf = \case
  Prim (Note d _) -> d
  Prim (Rest d) -> d
  a :+: b -> durOf a + durOf b
  a :=: b -> max (durOf a) (durOf b)
  Modify _ inner -> durOf inner

-- | Interpretation per voice; channel per lane; bend per note from the
-- tuning table; tempo curve (Todd arches + closing rit) over the whole.
--
-- Order per lane: raw -> inégales -> overhold -> ornaments -> dissonance
-- charges (against everything that actually sounds) -> articulation ->
-- breaths -> agogic lean -> dynamics (+charge). Assembly then applies the
-- asynchrony family: melody lead, final-chord roll, seeded jitter.
perform :: Interp -> Score -> Either String Performance
perform (Interp ap ag pp orn dyn ex piece table bendRange)
        (Score tempo voices _ _ meter) =
  let prepLane =
        realizeLane orn tempo
          . overholdLane (exOverhold ex)
          . inegalLane (exInegal ex)
      voiceLanes = [(v, map prepLane (lanes (vNotes v))) | v <- voices]
      totalLanes = sum (map (length . snd) voiceLanes)
      allSounding =
        [ (snOnset n, snDur n, snPitch n)
        | (_, ls) <- voiceLanes, l <- ls, n <- l ]
      end = maximum (0 : [o + d | (o, d, _) <- allSounding])
      -- group arches span arch_bars bars of whichever meter is in force
      groups = case meter of
        [] -> [(0, fromIntegral (exArchBars ex))]
        ms -> [ (t, WholeNotes (fromIntegral n / fromIntegral d)
                      * fromIntegral (exArchBars ex))
              | (t, (n, d)) <- ms ]
      exN = exExpression ex
      tmap =
        tempoMap ag (exN * exArchPiece ex) (exN * exArchGroup ex)
          groups tempo end
      -- highest mean pitch is the melody, which leads (Palmer 1996)
      meanPitch v =
        let ps = map snPitch (vNotes v)
         in if null ps then 0 else sum ps `div` length ps
      melodyVi =
        fst (last (sortOn snd
          [(vi, meanPitch v) | (vi, (v, _)) <- zip [0 :: Int ..] voiceLanes]))
      -- ms rules are converted at the *local* tempo, so a 20 ms lead
      -- stays 20 ms inside the closing ritardando instead of stretching
      -- with it.
      bpmAt t = case takeWhile ((<= t) . fst) tmap of
        [] -> tempo
        xs -> snd (last xs)
      msToWnAt t ms =
        let Bpm bpmD = bpmAt t
         in WholeNotes (approxRational (ms / 1000 * bpmD / 240) 1e-6)
      -- the final chord is whatever was *notated* at the last onset —
      -- decided from the score, before ornaments subdivide it and before
      -- lead/jitter perturb it; each event carries its source note so a
      -- trilled final chord tone rolls as one note
      finalOnset = maximum (0 : [snOnset n | v <- voices, n <- vNotes v])
      finalTag sn
        | fst (snSource sn) == finalOnset = Just (snd (snSource sn))
        | otherwise = Nothing
      seed = seedOf piece
      clampV = max 1 . min 127
      usableChannels = [ch | ch <- [0 .. 15], ch /= 9] -- 9 = GM percussion

      voiceTrack (chans, acc) (vi, (_, ls)) =
        let (mine, rest') = splitAt (length ls) chans
            evs = concat
              [ [ ( finalTag sn
                  , PerfNote (max 0 (fromRational t - lead + jit))
                      (fromRational (d * gate' * fermataFactor ag sn))
                      p v' bend ch )
                | (i, ((sn, gate), charge, (t, d, (p, _)))) <-
                    zip [0 :: Int ..]
                      (zip3 arts charges (musicEvents 0 (laneMusic arts)))
                , let gate' = leanGate ex charge gate
                      lead = if vi == melodyVi
                               then msToWnAt (fromRational t)
                                      (exEnsemble ex * exLeadMs ex)
                               else 0
                      jit = msToWnAt (fromRational t)
                              (exEnsemble ex * exJitterMs ex
                                 * seededJitter seed (i * 13 + ch * 7 + 1))
                      v' = clampV (vels !! min i (length vels - 1)
                             + round (exN * exDisVel ex * charge
                                 + exEnsemble ex * exJitterVel ex
                                     * seededJitter seed (i * 31 + ch * 3)))
                      bend = bendValue bendRange (offsetFor table p)
                ]
              | (ch, l') <- zip mine ls
              , let bounds =
                      map (>= ppThreshold pp) (boundaryStrengths pp l')
                    arts = breatheLane pp l' (articulateLane ap l')
                    vels = dynamicsLane dyn meter bounds l'
                    charges = chargesForLane allSounding l'
              ]
         in (rest', acc <> [sortOn (pnOnset . snd) evs])

      -- final chord rolled bass-upward (universal keyboard practice).
      -- Membership was decided from notated onsets, so jitter and lead
      -- cannot break the chord into singletons.
      rollFinal tracks
        | exEnsemble ex * exRollMs ex <= 0 = map (map snd) tracks
        | otherwise =
            let finals = sort (nub [ p | tr <- tracks, (Just p, _) <- tr ])
                rankOf p = length (takeWhile (< p) finals)
                rollStep = msToWnAt finalOnset (exEnsemble ex * exRollMs ex)
                shift (tag, n) = case tag of
                  Just p ->
                    let dt = rollStep * fromIntegral (rankOf p)
                     in n { pnOnset = pnOnset n + dt
                          , pnDur = max (pnDur n - dt)
                                        (msToWnAt finalOnset 30) }
                  Nothing -> n
             in map (map shift) tracks
   in if totalLanes > length usableChannels
        then Left ("score needs " <> show totalLanes
                     <> " monophonic lanes; only "
                     <> show (length usableChannels)
                     <> " MIDI channels available")
        else Right . Performance tmap . map enforceMono . rollFinal . snd $
               foldl voiceTrack (usableChannels, []) (zip [0 ..] voiceLanes)
  where
    -- The IR's contract: a channel is monophonic — no note may overlap its
    -- channel successor. Jitter, leans, overhold and legato gates can each
    -- push an off past the next on (which then releases the WRONG note:
    -- an audible dropout, invisible to transport counters). Clamp here,
    -- at the source, so every consumer inherits the guarantee; a 1.5 ms
    -- gap keeps off strictly before on even after downstream rounding.
    enforceMono track =
      let byCh = sortOn (\n -> (pnChannel n, pnOnset n)) track
          clamp n (Just nx)
            | pnChannel nx == pnChannel n
            , pnOnset n + pnDur n > gapBefore nx =
                n {pnDur = max (WholeNotes (1 / 512))
                             (gapBefore nx - pnOnset n)}
          clamp n _ = n
          gapBefore nx = pnOnset nx - WholeNotes (1 / 512)
       in sortOn pnOnset
            (zipWith clamp byCh (map Just (drop 1 byCh) <> [Nothing]))
