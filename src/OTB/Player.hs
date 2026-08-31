-- | Score -> Performance, through the annotation Player.
--
-- The Player is now thin: it builds lanes, prepares them (inégales,
-- overhold — the grid reshapers), hands each to 'annotateLane' (every
-- rule speaks Euterpea 'PhraseAttribute's) and 'interpret' (the one
-- annotated->performed path, which also realises ornament annotations),
-- traverses voices, and applies the ensemble family in 'assemble'.
-- The tempo map is *derived* from a conductor Music line
-- ('annotateConductor' / 'deriveTempoMap') — tempo has no other authority.
--
-- The lane, not the voice, is the monophonic unit — so the lane gets the
-- MIDI channel (bend is per channel; temperament needs a bend per note).
-- Channel 9 (GM percussion) is skipped; >15 lanes is a reported error.
--
-- License: GPL-2.0-or-later.
module OTB.Player
  ( PerfNote (..)
  , Performance (..)
  , Structure (..)
  , Interp (..)
  , defaultInterp
  , perform
  , analyzeStructure
  , lanes
  , prepareLane
  ) where

import Data.List (findIndex, nub, sort, sortOn)
import Data.Ratio (approxRational)
import EuterpeaLite.Music (Control (..), Music (..), Primitive (..))
import OTB.Analysis.Grouping (groupSpans)
import OTB.Analysis.Harmony (Harmony (..), analyzeHarmony)
import OTB.Analysis.Imitation (Imitation (..), findImitation)
import OTB.Analysis.Parallelism (Sequences (..), findSequences)
import OTB.Analysis.Subject (subjectEntries)
import OTB.Annotate
import OTB.Explain (Why, why)
import OTB.Interp.Phrasing (boundaryStrengths)
import OTB.Interp
import OTB.Interp.Agogics (AgogicParams (..), tempoMap)
import OTB.Interp.Express
import OTB.Interp.Ornament ()
import OTB.Score
import OTB.Tuning (adaptiveCents, bendValue, offsetFor)
import OTB.Units (Cents (..))
import OTB.Units (Bpm (..), WholeNotes (..))

data PerfNote = PerfNote
  { pnOnset :: !WholeNotes
  , pnDur :: !WholeNotes -- ^ sounding (post-articulation) duration
  , pnPitch :: !Int
  , pnVel :: !Int
  , pnBend :: !Int -- ^ 14-bit, emitted on the note's channel before it sounds
  , pnChannel :: !Int
  , pnIndex :: !Int -- ^ position within its lane; explain key with channel
  , pnSrcOn :: !WholeNotes
    -- ^ notated score onset — bar selection must use this, not 'pnOnset',
    -- which melody lead and jitter have already moved
  , pnCharge :: !Double
    -- ^ dissonance charge; velocity-blind hardware converts it to agogics
  }
  deriving (Show)

data Performance = Performance
  { perfTempoMap :: [(WholeNotes, Bpm)] -- ^ derived from the conductor line
  , perfTracks :: [[PerfNote]] -- ^ one track per voice, onset-sorted
  , perfWhys :: [((Int, Int), [Why])] -- ^ (channel, index) -> provenance; lazy
  , perfCadences :: [WholeNotes] -- ^ V-I arrivals (harmony model), notated
  }

-- ---------------------------------------------------------------------
-- Lanes (parser-recorded spine identity; chord extras to overflow lanes)

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

-- | Grid reshapers only; ornaments are annotations now and realise in
-- 'interpret'. Order: the inegales grid first (a style decision the
-- other rules should see), then the KTH micro-timing pair (double
-- duration softens 2:1, faster uphill rushes ascending runs), finger
-- pedal last.
prepareLane :: Interp -> [ScoreNote] -> [ScoreNote]
prepareLane ip =
  overholdLane (exOverhold ex)
    . uphillLane (exExpression ex * exUphill ex)
    . doubleDurLane (exExpression ex * exDoubleDur ex)
    . inegalLane (exInegal ex)
  where
    ex = iExpress ip

-- | Timed events from a Music line, honouring @Modify (Tempo r)@
-- (times and durations scale by 1/r) — Euterpea's own semantics.
musicEvents :: Rational -> Rational -> Music a -> [(Rational, Rational, a)]
musicEvents f t m = case m of
  Prim (Note d p) -> [(t, d / f, p)]
  Prim (Rest _) -> []
  a :+: b -> musicEvents f t a <> musicEvents f (t + durAt f a) b
  a :=: b -> musicEvents f t a <> musicEvents f t b
  Modify (Tempo r) inner -> musicEvents (f * r) t inner
  Modify _ inner -> musicEvents f t inner
  where
    durAt f' x = ndurOf x / f'
    ndurOf x = case x of
      Prim (Note d _) -> d
      Prim (Rest d) -> d
      a :+: b -> ndurOf a + ndurOf b
      a :=: b -> max (ndurOf a) (ndurOf b)
      Modify (Tempo r) inner -> ndurOf inner / r
      Modify _ inner -> ndurOf inner

voiceEvents :: [Music Ev] -> [(Rational, Rational, Ev)]
voiceEvents = musicEvents 1 0 . foldr (:=:) (Prim (Rest 0))

-- ---------------------------------------------------------------------
-- structural analysis, shared verbatim between 'perform' and
-- @otb analyze@ — what gets exported for fitting IS what gets played

data Structure = Structure
  { stSounding :: [(WholeNotes, WholeNotes, Int)]
    -- ^ every prepared note (post-reshaper onsets and durations)
  , stEnd :: WholeNotes
  , stBounds :: [(WholeNotes, Double)]
    -- ^ per-lane boundary strengths > 0.3, aggregated across the
    -- texture, computed with the CONFIGURED phrasing on PREPARED lanes
  , stTree :: [(WholeNotes, WholeNotes, Int)]
  }

analyzeStructure :: Interp -> Score -> Structure
analyzeStructure ip score =
  Structure
    { stSounding = sounding
    , stEnd = end
    , stBounds = bounds
    , stTree = groupSpans 3 (2 * barLen0) bounds 0 end
    }
  where
    voiceLanes' =
      [map (prepareLane ip) (lanes (vNotes v)) | v <- scVoices score]
    sounding =
      [ (snOnset n, snDur n, snPitch n)
      | ls <- voiceLanes', l <- ls, n <- l ]
    end = maximum (0 : [o + d | (o, d, _) <- sounding])
    bounds =
      [ (snOnset n + snDur n, str)
      | ls <- voiceLanes', lane <- ls
      , (n, str) <- zip lane (boundaryStrengths (iPhrasing ip) lane)
      , str > 0.3 ]
    barLen0 = case scMeter score of
      ((_, (n, d)) : _) -> WholeNotes (fromIntegral n / fromIntegral d)
      [] -> 1

-- ---------------------------------------------------------------------
-- perform

perform :: Interp -> Score -> Either String Performance
perform ip score@(Score tempo voices _ _ meter _) =
  if totalLanes > length usableChannels
    then Left ("score needs " <> show totalLanes
                 <> " monophonic lanes; only "
                 <> show (length usableChannels)
                 <> " MIDI channels available")
    else Right (assemble ip tempo tmap finalOnset melodyVi
                  (floorAt, floorAnyAt)
                  (hRootAt harm, hStabilityAt harm)
                  (hCadences harm) tracks)
  where
    ex = iExpress ip
    usableChannels = [ch | ch <- [0 .. 15], ch /= 9] -- 9 = GM percussion

    voiceLanes = [(v, map (prepareLane ip) (lanes (vNotes v))) | v <- voices]
    totalLanes = sum (map (length . snd) voiceLanes)

    -- what sounds after preparation: overheld tones charged on purpose;
    -- ornament subnotes NOT — a trill is heard as its parent's pitch for
    -- dissonance purposes (the annotation change, accepted)
    st = analyzeStructure ip score
    allSounding = stSounding st
    end = stEnd st

    exN = exExpression ex

    -- the harmony model and the fugue's subject, analysed once
    harm = analyzeHarmony meter allSounding end
    subjSet = subjectEntries score
    inSubject src = src `elem` subjSet

    -- the conversation: who holds the floor when (Klangrede)
    imit = findImitation score
    floorAt vi t =
      any (\(t0, v, sp) -> v == vi && t0 <= t && t < t0 + sp)
        (imTakes imit)
    floorAnyAt t =
      any (\(t0, _, sp) -> t0 <= t && t < t0 + sp) (imTakes imit)

    -- the grouping tree: from the shared structural analysis
    allBounds = stBounds st
    tree = stTree st

    -- one arch per node: piece level, tree levels decaying by depth,
    -- and the metrical bar groups as the innermost uniform layer
    barGroups = case meter of
      [] -> [(0, end)]
      ms ->
        concat
          [ [ (a, min (min stop end) (a + gl))
            | a <- takeWhile (< stop) (iterate (+ gl) t) ]
          | ((t, (n, d)), stop) <-
              zip ms (map fst (drop 1 ms) <> [end])
          , let gl = WholeNotes (fromIntegral n / fromIntegral d)
                       * fromIntegral (exArchBars ex)
          , gl > 0 ]
    arches =
      (0, end, exN * exArchPiece ex)
        : [ (a, b, exN * exArchGroup ex * 0.8 ^ (depth - 1))
          | (a, b, depth) <- tree ]
        <> [ (a, b, exN * exArchGroup ex * 0.5) | (a, b) <- barGroups ]

    -- easings: cadence arrivals at full depth, plus strong breath
    -- boundaries scaled by excess strength — the miner's discovered
    -- rule that players make strong boundaries tempo events
    strongBounds =
      mergeNearB
        [ (t, min 0.2 (agBoundaryEase (iAgogics ip) * (str - 1.5)))
        | (t, str) <- sortOn fst allBounds, str > 1.5 ]
    mergeNearB ((t1, d1) : (t2, d2) : more)
      | t2 - t1 <= 1 / 8 = mergeNearB ((t1, max d1 d2) : more)
      | otherwise = (t1, d1) : mergeNearB ((t2, d2) : more)
    mergeNearB xs = xs
    easings =
      [(c, agCadenceDepth (iAgogics ip)) | c <- hCadences harm]
        <> strongBounds

    -- the novelty brake: mean first-appearance rate per beat, applied
    -- from the FOLLOWING beat (humans respond, not anticipate)
    noveltySteps
      | agNoveltyBrake (iAgogics ip) <= 0 = []
      | otherwise =
          let nov = sqNovelty (findSequences score)
              beatLen = case meter of
                ((_, (_, d)) : _) ->
                  WholeNotes (1 / fromIntegral d)
                [] -> 1 / 4
              beats = takeWhile (< end) (iterate (+ beatLen) 0)
              rate lo =
                let xs = [v | (o, v) <- nov, lo <= o, o < lo + beatLen]
                 in if null xs then 0 else sum xs / fromIntegral (length xs)
           in [ (b + beatLen,
                 1 - agNoveltyBrake (iAgogics ip) * rate b)
              | b <- beats ]

    -- subject statements as merged spans, for the forward-motion push
    subjSpans = spansOf (sort (map fst subjSet))
    spansOf [] = []
    spansOf (t : ts) = go t t ts
      where
        go s e (x : xs) | x - e <= 1 / 2 = go s x xs
        go s e xs = (s, e + 1 / 4) : spansOf xs

    -- curve -> conductor Music -> derived map: the conductor is the
    -- carrier, deriveTempoMap the single reader
    curve = tempoMap (iAgogics ip) arches easings subjSpans noveltySteps
              tempo end
    conductor = annotateConductor curve tempo end
    tmap = deriveTempoMap tempo conductor

    meanPitch v =
      let ps = map snPitch (vNotes v)
       in if null ps then 0 else sum ps `div` length ps
    melodyVi =
      fst (last (sortOn snd
        [(vi, meanPitch v) | (vi, (v, _)) <- zip [0 :: Int ..] voiceLanes]))

    finalOnset = maximum (0 : [snOnset n | v <- voices, n <- vNotes v])
    finalTag sn
      | fst (snSource sn) == finalOnset = Just (snd (snSource sn))
      | otherwise = Nothing

    ctx vi ch = Ctx { cSounding = allSounding
                    , cMeters = meter
                    , cFinalTag = finalTag
                    , cChannel = ch
                    , cRootAt = hRootAt harm
                    , cChargeAt = hChargeAt harm
                    , cCadences = hCadences harm
                    , cSubject = inSubject
                    , cFloor = floorAt vi }

    tracks = snd (foldl deal (usableChannels, []) (zip [0 ..] voiceLanes))
    deal (chans, acc) (vi, (_, ls)) =
      let (mine, more) = splitAt (length ls) chans
       in ( more
          , acc <> [voiceEvents
                      [ interpret ip tempo ch (annotateLane ip (ctx vi ch) l)
                      | (ch, l) <- zip mine ls ]] )

-- ---------------------------------------------------------------------
-- Assembly: the ensemble family, across voices

assemble
  :: Interp -> Bpm -> [(WholeNotes, Bpm)] -> WholeNotes -> Int
  -> (Int -> WholeNotes -> Bool, WholeNotes -> Bool)
  -> (WholeNotes -> Maybe Int, WholeNotes -> Double) -> [WholeNotes]
  -> [[(Rational, Rational, Ev)]] -> Performance
assemble ip tempo tmap finalOnset melodyVi (floorAt, floorAny)
         (rootAt, stabAt) cads trs =
  Performance tmap
    (map enforceMono (rollFinal perturbed))
    (concatMap (map snd) whysed)
    cads
  where
    ex = iExpress ip
    seed = seedOf (iPiece ip)
    clampV = max 1 . min 127

    bpmAt t = case takeWhile ((<= t) . fst) tmap of
      [] -> tempo
      xs -> snd (last xs)
    msToWnAt t ms =
      let Bpm bpmD = bpmAt t
       in WholeNotes (approxRational (ms / 1000 * bpmD / 240) 1e-6)

    whysed = zipWith perturb [0 ..] trs
    perturbed = map (map fst) whysed

    perturb vi = map one
      where
        one (t, d, ev) =
          let ch = evChannel ev
              i = evIndex ev
              leadMs = exEnsemble ex * exLeadMs ex
              isMelody = vi == melodyVi
              -- Klangrede: whoever takes the floor leads — and the
              -- standing melody YIELDS while any voice holds it, or a
              -- two-voice exchange would shift both parts identically
              -- and produce no asynchrony at all
              hasFloor = floorAt vi (evSrcOn ev)
              melodyLeads = isMelody && not (floorAny (evSrcOn ev))
              lead = if hasFloor || melodyLeads
                       then msToWnAt (fromRational t) leadMs else 0
              jms = exEnsemble ex * exJitterMs ex
                      * seededJitter1f seed (ch * 2) i
              jit = msToWnAt (fromRational t) jms
              jv = exEnsemble ex * exJitterVel ex
                     * seededJitter1f seed (ch * 2 + 1) i
              vel = clampV (evVel ev + round jv)
              werck = offsetFor (iTuning ip) (evPitch ev)
              off
                | iAdaptive ip
                , Just r <- rootAt (evSrcOn ev) =
                    adaptiveCents (iTuning ip) r
                      (stabAt (evSrcOn ev)) (evPitch ev)
                | otherwise = werck
              bend = bendValue (iBendRange ip) off
              adaptDelta = let Cents a = off; Cents w = werck in a - w
              ws = evWhy ev
                <> [ why "adaptive-tuning"
                       (showD adaptDelta
                          <> " cents toward just (root settled)")
                       "Duffin; 5-limit just intonation over Werckmeister"
                   | iAdaptive ip, abs adaptDelta > 0.5 ]
                <> [ why "melody-lead"
                       ("-" <> show (round leadMs :: Int) <> " ms (leads)")
                       "Palmer 1996; Rasch 1979 (asynchrony aids voice streaming)"
                   | melodyLeads, leadMs > 0 ]
                <> [ why "dialogue-lead"
                       ("-" <> show (round leadMs :: Int)
                          <> " ms (has the floor)")
                       "Harnoncourt, Musik als Klangrede"
                   | hasFloor, not isMelody, leadMs > 0 ]
                <> [ why "jitter"
                       (showMs jms <> " ms, " <> showD jv <> " vel (seeded 1/f)")
                       "KTH noise rules; Gilden 1995" | exEnsemble ex > 0 ]
           in ( ( evFinal ev
                , PerfNote (max 0 (fromRational t - lead + jit))
                    (fromRational (d * evGate ev * evHold ev))
                    (evPitch ev) vel bend ch i (evSrcOn ev) (evCharge ev) )
              , ((ch, i), ws) )
        showMs v = showD v
        showD v =
          let r = fromIntegral (round (v * 10) :: Int) / 10 :: Double
           in (if r >= 0 then "+" else "") <> show r

    rollFinal trs'
      | exEnsemble ex * exRollMs ex <= 0 = map (map snd) trs'
      | otherwise =
          let finals = sort (nub [ p | tr <- trs', (Just p, _) <- tr ])
              rankOf p = length (takeWhile (< p) finals)
              rollStep = msToWnAt finalOnset (exEnsemble ex * exRollMs ex)
              shift (tag, n) = case tag of
                Just p ->
                  let dt = rollStep * fromIntegral (rankOf p)
                   in n { pnOnset = pnOnset n + dt
                        , pnDur = max (pnDur n - dt)
                                      (msToWnAt finalOnset 30) }
                Nothing -> n
           in map (map shift) trs'

    -- the IR's monophonic-channel contract (see the mono-invariant story
    -- in the git history): clamp offs before their channel successor with
    -- a 1.5 ms local-tempo gap; closer than that, keep half the distance
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
