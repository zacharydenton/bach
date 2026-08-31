-- | The annotation Player: rules speak Euterpea, one interpreter listens.
--
-- Every interpretive decision is attached to the score as Euterpea's own
-- vocabulary — @Modify (Phrase [...])@ around each note, with
-- 'Articulation' (Staccato\/Legato gates, Breath, Fermata), 'Dynamic'
-- (Loudness, Accent = dissonance charge), and 'Ornament' (Trill, Mordent,
-- InvMordent, Turn, Arpeggio for final-chord membership). The *annotated
-- score* is an artifact: the interpretation written in the language the
-- literature's Players were designed for (HSoM ch. 8–9).
--
-- 'interpret' is the single 'Annotated' -> Music Ev path: it folds the
-- attribute environment into 'Ev' values and expands ornament annotations
-- into subnotes ('realizeNote'). Provenance ('Why') rides in the payload;
-- laziness keeps it free until @otb explain@ forces it.
--
-- The conductor is a Music line too: tempo lives as @Modify (Tempo f)@
-- segments and the emitted tempo map is *derived* by 'deriveTempoMap' —
-- there is no other tempo authority.
--
-- License: GPL-2.0-or-later.
module OTB.Annotate
  ( APayload
  , Annotated -- abstract: 'annotateLane' builds, 'interpret' consumes
  , Ctx (..)
  , Ev (..)
  , annotateLane
  , interpret
  , annotateConductor
  , deriveTempoMap
  , annEvents
  ) where

import Data.Maybe (isJust, mapMaybe)
import EuterpeaLite.Music
import OTB.Explain (Why (..), why)
import OTB.Interp
import OTB.Interp.Agogics (fermataFactor)
import OTB.Interp.Articulation (articulateLane')
import OTB.Interp.Dynamics (dynamicsLane')
import OTB.Interp.Express
import OTB.Interp.Ornament (realizeNote)
import OTB.Analysis.Harmony (melodicCharge)
import OTB.Interp.Phrasing (PhraseParams (..), boundaryStrengths, breatheLane')
import OTB.Kern.Token qualified as K
import OTB.Score
import OTB.Units (Bpm (..), WholeNotes (..))

-- | What a note carries through the annotated tree: the score note itself
-- (marks, segments, source identity intact) and the reasons so far.
type APayload = (ScoreNote, [Why])

-- | Stage phantom: annotated but not yet performed. 'interpret' is the
-- only exit; feeding an un-annotated Music to the performer, or
-- annotating twice, does not typecheck.
newtype Annotated = Annotated {unAnnotated :: Music APayload}

-- | Per-piece context the annotators need beyond the lane itself.
data Ctx = Ctx
  { cSounding :: [(WholeNotes, WholeNotes, Int)] -- ^ all sounding notes
  , cMeters :: [(WholeNotes, (Int, Int))] -- ^ meter map
  , cFinalTag :: ScoreNote -> Maybe Int -- ^ final-chord membership
  , cChannel :: Int
  , cRootAt :: WholeNotes -> Maybe Int -- ^ chord root pc (harmony model)
  , cChargeAt :: WholeNotes -> Double -- ^ harmonic charge at a position
  , cCadences :: [WholeNotes] -- ^ V-I arrival onsets
  , cSubject :: (WholeNotes, Int) -> Bool -- ^ fugue subject membership
  , cFloor :: WholeNotes -> Bool
    -- ^ does THIS voice hold the floor (cross-voice imitation) at the
    -- notated onset — the dialogue rule's question
  }

-- | The interpreter's output payload, plus provenance.
data Ev = Ev
  { evPitch :: !Int
  , evGate :: !Rational
  , evVel :: !Int
  , evHold :: !Rational
  , evChannel :: !Int
  , evIndex :: !Int
  , evSrcOn :: !WholeNotes -- ^ notated onset (explain's bar selector key)
  , evCharge :: !Double -- ^ dissonance charge — velocity-blind targets
                        -- convert it to agogics downstream
  , evFinal :: !(Maybe Int)
  , evWhy :: [Why] -- ^ lazy on purpose: forced only by explain
  }

-- ---------------------------------------------------------------------
-- Annotation

-- | A prepared lane (graces, inégales and overhold applied; ornaments
-- NOT yet realised) into an annotated Music line: each note wrapped in
-- @Modify (Phrase attrs)@ carrying every decision the lane rules made.
-- The first argument carries per-note whys from the preparation stage
-- (Player.prepareLaneW) so the reshapers' decisions reach the explain
-- output alongside everything decided here.
annotateLane :: Interp -> Ctx -> [[Why]] -> [ScoreNote] -> Annotated
annotateLane ip ctx preWs l = Annotated (go 0 decided)
  where
    ex = iExpress ip
    arts = articulateLane' (iArt ip) l
    -- cadence arrivals from the harmony model bolster the boundary
    -- strength a lane's surface alone would compute
    cadBonus =
      [ if any (\c -> abs' (snOnset n + snDur n - c) <= 1 / 8)
             (cCadences ctx)
          then ppWCadence (iPhrasing ip) else 0
      | n <- l ]
    abs' (WholeNotes r) = WholeNotes (abs r)
    breaths = breatheLane' (iPhrasing ip) cadBonus l
                [(n, g) | (n, g, _) <- arts]
    charges = chargesForLane (cSounding ctx) l
    bounds = map (>= ppThreshold (iPhrasing ip))
               (zipWith (+) (boundaryStrengths (iPhrasing ip) l) cadBonus)
    vels = dynamicsLane' (iDynamics ip) (cMeters ctx) bounds l

    -- KTH leap articulation / leap tone duration (Friberg, Bresin &
    -- Sundberg 2006): micropause before a large leap, a fuller hold on
    -- its arrival
    nextIv = [ fmap (\nx -> abs (snPitch nx - snPitch n)) mnx
             | (n, mnx) <- zip l (map Just (drop 1 l) <> [Nothing]) ]
    prevIv = Nothing : init nextIv
    -- KTH duration contrast: short notes against the lane's median get
    -- crisper (velocity side lives in Dynamics' hierarchy already)
    medianDur = case map snDur l of
      [] -> 1
      ds -> let srt = sortDurs ds in srt !! (length srt `div` 2)
    sortDurs = foldr ins []
      where ins x (y : ys) | x > y = y : ins x ys
            ins x ys = x : ys

    -- preWs padded: it is positionally aligned but the pad keeps this
    -- total if a caller supplies fewer (the other lists bound the zip)
    decided =
      zipWith7 build (preWs <> repeat []) arts breaths charges vels
        nextIv prevIv
    zipWith7 f (p : ps) (a : as) (b : bs) (c : cs) (d : ds) (e : es) (g : gs) =
      f p a b c d e g : zipWith7 f ps as bs cs ds es gs
    zipWith7 _ _ _ _ _ _ _ _ = []

    build preW (_, _, artWhy) (sn, gateBreathed, mBreath) charge (v, velWhys)
          mNextIv mPrevIv =
      (sn, attrs, whys)
      where
        leapCut = case mNextIv of
          Just iv | iv >= 7 ->
            exExpression ex * exLeapPause ex
              * min 1 (fromIntegral (iv - 6) / 12)
          _ -> 0
        leapHold = case mPrevIv of
          Just iv | iv >= 7 -> exExpression ex * exLeapDur ex
          _ -> 0
        dcCut =
          let WholeNotes dr = snDur sn
              WholeNotes mr = medianDur
              ratio = fromRational (dr / max mr (1 / 64)) :: Double
           in if ratio < 1
                then exExpression ex * exDurContrast ex * (1 - ratio)
                else 0
        gateK = max (1 / 20) $ min (23 / 20) $
          gateBreathed * approx ((1 - leapCut) * (1 - dcCut))
            + approx leapHold
        approx x = toRational (fromIntegral (round (x * 1e6) :: Int)) / 1e6
        gate = leanGate ex charge gateK

        -- harmony model: melodic + harmonic charge (Friberg 1991), and
        -- the fugue subject speaks up (Czerny's practice)
        mcBump = case cRootAt ctx (snOnset sn) of
          Just r -> round (exExpression ex * exMelCharge ex
                             * melodicCharge (snPitch sn) r)
          Nothing -> 0
        hcBump = round (exExpression ex * exHarmCharge ex
                          * cChargeAt ctx (snOnset sn))
        sBump = if cSubject ctx (snSource sn)
                  then round (exExpression ex * exSubjectVel ex)
                  else 0 :: Int
        dBump = if cFloor ctx (fst (snSource sn))
                  then round (exEnsemble ex * exDialogueVel ex)
                  else 0 :: Int
        vOut = v + mcBump + hcBump + sBump + dBump
        attrs = concat
          [ [Art (if gate < 1 then Staccato gate else Legato gate)]
          , [Art Breath | isJust mBreath]
          , [Art Fermata | K.Fermata `elem` snMarks sn]
          , [Dyn (Loudness (fromIntegral vOut))]
          , [Dyn (Accent (charge1000 charge)) | charge > 0]
          , mapMaybe ornAttr (snMarks sn)
          , [Orn Arpeggio | isJust (cFinalTag ctx sn)]
          ]
        whys = concat
          [ preW -- preparation-stage decisions happened first
          , [artWhy]
          , maybe [] pure mBreath
          , [ why "dissonance"
                ("+" <> show (bump :: Int) <> " vel, agogic lean; charge "
                   <> show (fromRational (charge1000 charge) :: Double))
                "CPE Bach 1753; KTH harmonic charge"
            | charge > 0 ]
          , velWhys
          , [ why "leap-pause"
                ("gate x" <> showD2 (1 - leapCut) <> " before the leap")
                "KTH leap articulation (Friberg/Bresin/Sundberg 2006)"
            | leapCut > 0.005 ]
          , [ why "leap-arrival"
                ("gate +" <> showD2 leapHold <> " on the arrival")
                "KTH leap tone duration" | leapHold > 0.005 ]
          , [ why "duration-contrast"
                ("gate x" <> showD2 (1 - dcCut) <> " (short vs lane median)")
                "KTH duration contrast" | dcCut > 0.005 ]
          , [ why "melodic-charge"
                ("+" <> show mcBump <> " vel")
                "Friberg 1991; DM melodic charge" | mcBump > 0 ]
          , [ why "harmonic-charge"
                ("+" <> show hcBump <> " vel")
                "Sundberg; DM harmonic charge" | hcBump > 0 ]
          , [ why "subject-entry"
                ("+" <> show sBump <> " vel across the statement")
                "fugal practice; Czerny's WTC edition" | sBump > 0 ]
          , [ why "dialogue"
                ("+" <> show dBump <> " vel — takes the floor")
                "Harnoncourt, Musik als Klangrede; Czerny's WTC edition"
            | dBump > 0 ]
          , [ why "ornament" (ornName m) "Bach's Explication; CPE Bach 1753"
            | m <- snMarks sn, isJust (ornAttr m) ]
          , [ why "final-chord" "member: rolled bass-upward in assembly"
                "keyboard practice; Palmer 1996"
            | isJust (cFinalTag ctx sn) ]
          ]
        bump = round (exExpression ex * exDisVel ex * charge)
        showD2 x = show (fromIntegral (round (x * 100) :: Int) / 100 :: Double)

    charge1000 :: Double -> Rational
    charge1000 c = fromIntegral (round (c * 1000) :: Int) / 1000

    ornAttr m = case m of
      K.Trill _ -> Just (Orn Trill)
      K.Mordent _ -> Just (Orn Mordent)
      K.InvMordent _ -> Just (Orn InvMordent)
      K.Turn -> Just (Orn Turn)
      K.InvTurn -> Just (Orn TrilledTurn)
      _ -> Nothing
    ornName m = case m of
      K.Trill i -> "trill (aux +" <> show i <> ")"
      K.Mordent i -> "mordent (aux -" <> show i <> ")"
      K.InvMordent i -> "inverted mordent (aux +" <> show i <> ")"
      K.Turn -> "turn"
      K.InvTurn -> "inverted turn"
      _ -> "?"

    go _ [] = rest 0
    go t ((sn, attrs, ws) : more) =
      let gap = realToFrac (snOnset sn - t)
          n = Modify (Phrase attrs) (note (realToFrac (snDur sn)) (sn, ws))
       in rest gap :+: n :+: go (snOnset sn + snDur sn) more

-- ---------------------------------------------------------------------
-- Interpretation

-- | Fold the attribute environment into 'Ev' values, expanding ornament
-- annotations into subnotes. The only 'Annotated' exit.
interpret :: Interp -> Bpm -> Int -> Annotated -> Music Ev
interpret ip tempo ch (Annotated m0) = snd (go [] 0 m0)
  where
    go env i m = case m of
      Modify (Phrase attrs) inner -> go (attrs <> env) i inner
      Modify c inner ->
        let (i', m') = go env i inner in (i', Modify c m')
      Prim (Rest d) -> (i, Prim (Rest d))
      Prim (Note d (sn, ws)) -> noteOut env i d sn ws
      a :+: b ->
        let (i1, a') = go env i a
            (i2, b') = go env i1 b
         in (i2, a' :+: b')
      a :=: b ->
        let (i1, a') = go env i a
            (i2, b') = go env i1 b
         in (i2, a' :=: b')

    noteOut env i d sn ws
      | isOrnamented env =
          let subs = realizeNote (iOrnaments ip) tempo sn
              k = length subs
              evs =
                [ mkEv env (i + j) s ws
                    (if j == k - 1 then gateOf env else 1)
                    (if j == k - 1 then holdOf env sn else 1)
                | (j, s) <- zip [0 ..] subs ]
              -- subnote durations sum to d by realizeNote's contract
              subLine =
                foldr (:+:) (rest 0)
                  (zipWith (\s ev -> note (realToFrac (snDur s)) ev) subs evs)
           in (i + k, subLine)
      | otherwise =
          (i + 1, note d (mkEv env i sn ws (gateOf env) (holdOf env sn)))

    isOrnamented env = or [True | Orn o <- env, o `elem` ornKinds]
    ornKinds = [Trill, Mordent, InvMordent, Turn, TrilledTurn]

    gateOf env = case [r | Art a <- env, Just r <- [gateR a]] of
      (r : _) -> r
      [] -> 1
    gateR a = case a of
      Staccato r -> Just r
      Legato r -> Just r
      _ -> Nothing

    holdOf env sn
      | or [True | Art Fermata <- env] = fermataFactor (iAgogics ip) sn
      | otherwise = 1

    mkEv env i sn ws gate hold = Ev
      { evPitch = snPitch sn
      , evGate = gate
      , evVel = clampV (loud + bump)
      , evHold = hold
      , evChannel = ch
      , evIndex = i
      , evSrcOn = fst (snSource sn)
      , evCharge = case [c | Dyn (Accent c) <- env] of
          (c : _) -> fromRational c
          [] -> 0
      , evFinal = if or [True | Orn Arpeggio <- env]
                    then Just (snd (snSource sn)) else Nothing
      , evWhy = ws
      }
      where
        loud = case [round v | Dyn (Loudness v) <- env] of
          (v : _) -> v
          [] -> 84
        bump = case [c | Dyn (Accent c) <- env] of
          (c : _) -> round (exExpression ex * exDisVel ex * fromRational c)
          [] -> 0
        ex = iExpress ip
        clampV = max 1 . min 127

-- ---------------------------------------------------------------------
-- The conductor: tempo as a Music line, the map derived from it

-- | Tempo curve segments as @Modify (Tempo f)@ over rests — the conductor
-- part. Euterpea semantics: @Tempo f@ plays its scope f times faster.
annotateConductor :: [(WholeNotes, Bpm)] -> Bpm -> WholeNotes -> Music APayload
annotateConductor tmap (Bpm base) end = line' (zip tmap nexts)
  where
    nexts = map (Just . fst) (drop 1 tmap) <> [Nothing]
    line' [] = rest 0
    line' (((t, Bpm b), next) : more) =
      let stop = maybe end id next
          d = realToFrac (max 0 (stop - t))
       in Modify (Tempo (toRational (b / base))) (rest d) :+: line' more

-- | Read the tempo map back out of a conductor line — the single reader
-- of tempo annotations. Positions are *notated* score time (advanced by
-- 'ndur', which unlike Euterpea's 'dur' ignores Tempo scaling: a Tempo
-- modifier maps positions to seconds later, it does not move them).
deriveTempoMap :: Bpm -> Music APayload -> [(WholeNotes, Bpm)]
deriveTempoMap (Bpm base) = ensure . thin . go 1 0
  where
    go f t m = case m of
      Modify (Tempo r) inner -> go (f * fromRational r) t inner
      Modify _ inner -> go f t inner
      -- zero-length rests carry no tempo span; in particular the
      -- conductor line's structural trailing @rest 0@ sits OUTSIDE any
      -- Tempo modifier and would otherwise emit (end, base) — a
      -- base-tempo reset that cancels the closing ritardando while the
      -- final fermata is still sounding
      Prim (Rest d) -> [(WholeNotes t, Bpm (base * f)) | d > 0]
      Prim (Note _ _) -> []
      a :+: b -> go f t a <> go f (t + ndur a) b
      a :=: b -> go f t a <> go f t b
    thin ((t1, b1) : (t2, b2) : more)
      | b1 == b2 = thin ((t1, b1) : more)
      | otherwise = (t1, b1) : thin ((t2, b2) : more)
    thin xs = xs
    ensure [] = [(0, Bpm base)]
    ensure xs = xs

-- | Notated duration: like 'dur' but Tempo-blind.
ndur :: Music a -> Rational
ndur m = case m of
  Prim (Note d _) -> d
  Prim (Rest d) -> d
  a :+: b -> ndur a + ndur b
  a :=: b -> max (ndur a) (ndur b)
  Modify _ inner -> ndur inner

-- ---------------------------------------------------------------------
-- Law-facing traversal

-- | Timed events from an annotated tree, honouring @Tempo@ (scales times
-- and durations) and @Transpose@ (shifts pitches) — the traversal the
-- metamorphic laws exercise.
annEvents :: Music APayload -> [(Rational, Rational, APayload)]
annEvents = go 1 0 0
  where
    go f dt tr m = case m of
      Prim (Note d (sn, ws)) ->
        -- a chromatic shift cannot keep the notation: respelling needs
        -- key context, so a nonzero transpose clears snSpell rather than
        -- carry a contradicting one (the Score.hs invariant)
        let sn' | tr == 0 = sn
                | otherwise =
                    sn {snPitch = snPitch sn + tr, snSpell = Nothing}
         in [(dt, d / f, (sn', ws))]
      Prim (Rest _) -> []
      a :+: b -> go f dt tr a <> go f (dt + dur a / f) tr b
      a :=: b -> go f dt tr a <> go f dt tr b
      Modify (Tempo r) inner -> go (f * r) dt tr inner
      Modify (Transpose n) inner -> go f dt (tr + n) inner
      Modify _ inner -> go f dt tr inner
