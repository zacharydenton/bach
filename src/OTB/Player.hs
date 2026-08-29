-- | Score -> Performance, via Euterpea's Music algebra.
--
-- Each voice's notes are rebuilt into monophonic *lanes* (the spine paths
-- they came from). Each lane is interpreted into a @Music@ line whose
-- payload is the fully decided note ('Ev'); a voice is its lanes in
-- parallel (@:=:@), traversed once into timed events; assembly then
-- applies the ensemble family across voices.
--
-- **The lane, not the voice, is the monophonic unit — so the lane gets
-- the MIDI channel.** Pitch bend is per channel, and temperament needs a
-- distinct bend per sounding note; a voice whose sub-spines hold a chord
-- would smear one bend across it. Channel 9 (GM percussion) is skipped
-- for the audition path's sake; more than 15 simultaneous lanes is a
-- cardinality error, reported, not truncated.
--
-- The pipeline, in the order the code is laid out:
--
--   1. 'lanes'         — voice notes into spine lanes
--   2. 'prepareLane'   — raw -> inégales -> overhold -> ornaments
--   3. 'interpretLane' — dissonance charges -> articulation -> breaths
--                        -> agogic lean -> dynamics, into a Music line
--   4. 'voiceEvents'   — lanes in parallel, one traversal
--   5. 'assemble'      — melody lead, seeded jitter, final-chord roll,
--                        and the monophonic-channel invariant
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
import EuterpeaLite.Music (Music (..), Primitive (..), dur, note, rest)
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

-- | The payload of a Music line: a note with everything the lane stages
-- have decided, before the ensemble family perturbs it in assembly.
data Ev = Ev
  { evPitch :: !Int
  , evGate :: !Rational -- ^ post-articulation, post-lean fraction of duration
  , evVel :: !Int -- ^ pre-jitter velocity
  , evHold :: !Rational -- ^ fermata factor
  , evChannel :: !Int
  , evIndex :: !Int -- ^ position within its lane (jitter seed)
  , evFinal :: !(Maybe Int) -- ^ notated pitch, when part of the final chord
  }

-- ---------------------------------------------------------------------
-- 1. Lanes

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

-- ---------------------------------------------------------------------
-- 2. Preparation: what the lane *is* before any rule reads it

-- | raw -> inégales -> overhold -> ornaments. Everything downstream sees
-- the swung, held, subdivided grid.
prepareLane :: Interp -> Bpm -> [ScoreNote] -> [ScoreNote]
prepareLane ip tempo =
  realizeLane (iOrnaments ip) tempo
    . overholdLane (exOverhold ex)
    . inegalLane (exInegal ex)
  where
    ex = iExpress ip

-- ---------------------------------------------------------------------
-- 3. Interpretation: one prepared lane into a Music line

-- | Charges (against everything that sounds) -> articulation -> breaths
-- -> agogic lean -> dynamics (+charge). The result is a Music line: rests
-- for the gaps, an 'Ev' per note. Durations are already whole notes —
-- Euterpea's Dur — so no time conversion happens here.
interpretLane
  :: Interp
  -> [(WholeNotes, WholeNotes, Int)] -- ^ every sounding note in the piece
  -> [(WholeNotes, (Int, Int))] -- ^ meter map
  -> (ScoreNote -> Maybe Int) -- ^ final-chord membership
  -> Int -- ^ channel
  -> [ScoreNote]
  -> Music Ev
interpretLane ip sounding meter finalTag ch l =
  go 0 (zip3 [0 ..] arts (zip charges vels))
  where
    ex = iExpress ip
    pp = iPhrasing ip
    bounds = map (>= ppThreshold pp) (boundaryStrengths pp l)
    arts = breatheLane pp l (articulateLane (iArt ip) l)
    vels = dynamicsLane (iDynamics ip) meter bounds l
    charges = chargesForLane sounding l
    clampV = max 1 . min 127
    go _ [] = rest 0
    go t ((i, (sn, gate), (charge, vel)) : more) =
      let ev = Ev
            { evPitch = snPitch sn
            , evGate = leanGate ex charge gate
            , evVel = clampV (vel + round (exExpression ex * exDisVel ex * charge))
            , evHold = fermataFactor (iAgogics ip) sn
            , evChannel = ch
            , evIndex = i
            , evFinal = finalTag sn
            }
          gap = realToFrac (snOnset sn - t)
       in rest gap :+: note (realToFrac (snDur sn)) ev
            :+: go (snOnset sn + snDur sn) more

-- ---------------------------------------------------------------------
-- 4. Traversal: a voice is its lanes in parallel

-- | Lanes side by side (@:=:@), then one traversal back to timed events.
-- Pure, total.
voiceEvents :: [Music Ev] -> [(Rational, Rational, Ev)]
voiceEvents = musicEvents 0 . foldr (:=:) (rest 0)

musicEvents :: Rational -> Music a -> [(Rational, Rational, a)]
musicEvents t m = case m of
  Prim (Note d p) -> [(t, d, p)]
  Prim (Rest _) -> []
  a :+: b -> musicEvents t a <> musicEvents (t + dur a) b
  a :=: b -> musicEvents t a <> musicEvents t b
  Modify _ inner -> musicEvents t inner

-- ---------------------------------------------------------------------
-- 5. Assembly: the ensemble family, across voices

-- | Interpretation per voice; channel per lane; bend per note from the
-- tuning table; tempo curve (Todd arches + closing rit) over the whole.
perform :: Interp -> Score -> Either String Performance
perform ip (Score tempo voices _ _ meter _) =
  if totalLanes > length usableChannels
    then Left ("score needs " <> show totalLanes
                 <> " monophonic lanes; only "
                 <> show (length usableChannels)
                 <> " MIDI channels available")
    else Right (Performance tmap
                  (assemble ip tempo tmap finalOnset melodyVi tracks))
  where
    ex = iExpress ip
    ag = iAgogics ip
    usableChannels = [ch | ch <- [0 .. 15], ch /= 9] -- 9 = GM percussion

    voiceLanes = [(v, map (prepareLane ip tempo) (lanes (vNotes v))) | v <- voices]
    totalLanes = sum (map (length . snd) voiceLanes)

    -- "sounding" is what actually sounds after preparation: overheld
    -- (finger-pedalled) tones are deliberately included, so a note is
    -- charged against the pedal haze the ear still hears, not against
    -- the notated skeleton
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
    tmap = tempoMap ag (exN * exArchPiece ex) (exN * exArchGroup ex)
             groups tempo end

    -- highest mean pitch is the melody, which leads (Palmer 1996)
    meanPitch v =
      let ps = map snPitch (vNotes v)
       in if null ps then 0 else sum ps `div` length ps
    melodyVi =
      fst (last (sortOn snd
        [(vi, meanPitch v) | (vi, (v, _)) <- zip [0 :: Int ..] voiceLanes]))

    -- the final chord is whatever was *notated* at the last onset —
    -- decided from the score, before ornaments subdivide it and before
    -- lead/jitter perturb it; each event carries its source note so a
    -- trilled final chord tone rolls as one note
    finalOnset = maximum (0 : [snOnset n | v <- voices, n <- vNotes v])
    finalTag sn
      | fst (snSource sn) == finalOnset = Just (snd (snSource sn))
      | otherwise = Nothing

    -- channels dealt to lanes in voice order
    tracks = snd (foldl deal (usableChannels, []) voiceLanes)
    deal (chans, acc) (_, ls) =
      let (mine, more) = splitAt (length ls) chans
       in ( more
          , acc <> [voiceEvents
                      (zipWith (interpretLane ip allSounding meter finalTag)
                         mine ls)] )

-- | Melody lead, seeded jitter, bend lookup, final-chord roll, mono.
assemble
  :: Interp
  -> Bpm -- ^ base tempo
  -> [(WholeNotes, Bpm)]
  -> WholeNotes -- ^ final chord's notated onset
  -> Int -- ^ index of the melody voice
  -> [[(Rational, Rational, Ev)]]
  -> [[PerfNote]]
assemble ip tempo tmap finalOnset melodyVi =
  map enforceMono . rollFinal . zipWith perturb [0 ..]
  where
    ex = iExpress ip
    seed = seedOf (iPiece ip)
    clampV = max 1 . min 127

    -- ms rules are converted at the *local* tempo, so a 20 ms lead
    -- stays 20 ms inside the closing ritardando instead of stretching
    -- with it.
    bpmAt t = case takeWhile ((<= t) . fst) tmap of
      [] -> tempo
      xs -> snd (last xs)
    msToWnAt t ms =
      let Bpm bpmD = bpmAt t
       in WholeNotes (approxRational (ms / 1000 * bpmD / 240) 1e-6)

    perturb vi = map one
      where
        one (t, d, ev) =
          let ch = evChannel ev
              i = evIndex ev
              lead = if vi == melodyVi
                       then msToWnAt (fromRational t) (exEnsemble ex * exLeadMs ex)
                       else 0
              jit = msToWnAt (fromRational t)
                      (exEnsemble ex * exJitterMs ex
                         * seededJitter seed (i * 13 + ch * 7 + 1))
              vel = clampV (evVel ev
                      + round (exEnsemble ex * exJitterVel ex
                                 * seededJitter seed (i * 31 + ch * 3)))
              bend = bendValue (iBendRange ip) (offsetFor (iTuning ip) (evPitch ev))
           in ( evFinal ev
              , PerfNote (max 0 (fromRational t - lead + jit))
                  (fromRational (d * evGate ev * evHold ev))
                  (evPitch ev) vel bend ch )

    -- final chord rolled bass-upward (universal keyboard practice).
    -- Membership was decided from notated onsets, so jitter and lead
    -- cannot break the chord into singletons.
    rollFinal trs
      | exEnsemble ex * exRollMs ex <= 0 = map (map snd) trs
      | otherwise =
          let finals = sort (nub [ p | tr <- trs, (Just p, _) <- tr ])
              rankOf p = length (takeWhile (< p) finals)
              rollStep = msToWnAt finalOnset (exEnsemble ex * exRollMs ex)
              shift (tag, n) = case tag of
                Just p ->
                  let dt = rollStep * fromIntegral (rankOf p)
                   in n { pnOnset = pnOnset n + dt
                        , pnDur = max (pnDur n - dt)
                                      (msToWnAt finalOnset 30) }
                Nothing -> n
           in map (map shift) trs

    -- The IR's contract: a channel is monophonic — no note may overlap its
    -- channel successor. Jitter, leans, overhold and legato gates can each
    -- push an off past the next on (which then releases the WRONG note:
    -- an audible dropout, invisible to transport counters). Clamp here,
    -- at the source, so every consumer inherits the guarantee; a 1.5 ms
    -- gap *at the local tempo* keeps off strictly before on even after
    -- downstream rounding. When two onsets sit closer than that, the
    -- first note keeps half the distance rather than vanishing.
    enforceMono track =
      let byCh = sortOn (\n -> (pnChannel n, pnOnset n)) track
          clamp n (Just nx)
            | pnChannel nx == pnChannel n =
                let available = max 0 (pnOnset nx - pnOnset n)
                    safeGap = min (msToWnAt (pnOnset nx) 1.5) (available / 2)
                 in n {pnDur = max 0 (min (pnDur n) (available - safeGap))}
          clamp n _ = n
       in sortOn pnOnset
            (zipWith clamp byCh (map Just (drop 1 byCh) <> [Nothing]))
