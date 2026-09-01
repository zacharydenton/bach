-- | The ASAP note bridge, in-process: join a performed .match file's
-- snote rows to the compiler's own notes on the score key
-- (notated onset ticks, notated pitch).
--
-- This is a faithful port of the Python reference (tools/note_align.py
-- at its deterministic revision): four passes — exact affine join,
-- segmented re-anchoring, pitch-anchored fuzzy, written-out ornament —
-- with the same greedy orders, the same tie-breaks (the reference was
-- determinized first: sorted candidate iteration, key-ordered ornament
-- groups), and Double-mediated tick rounding so results agree with the
-- JSON-fed reference bit-for-bit. The retirement gate: row-for-row
-- equality against the Python on every performance of the corpus.
--
-- License: GPL-2.0-or-later.
module OTB.Bridge
  ( MatchPerf (..)
  , MatchRow (..)
  , IRRep (..)
  , SubNote (..)
  , BridgeRow (..)
  , Pass (..)
  , Counters (..)
  , wnTicks
  , parseMatch
  , loadIrNotes
  , bridge
  ) where

import Data.Char (isDigit)
import Data.List (foldl', minimumBy, sort, sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import Data.Text (Text)
import Data.Text qualified as T
import OTB.Explain (Why (..))
import OTB.Player (PerfNote (..), Performance (..))
import OTB.Units (Seconds (..), WholeNotes (..), secondsAt)

-- | Ticks per whole note: the universal join-key unit.
wnTicks :: Int
wnTicks = 1920

-- ---------------------------------------------------------------------
-- .match parsing

data MatchRow = MatchRow
  { mrKey :: !Int -- ^ notated onset in ticks (den-normalised)
  , mrXmlId :: !Text
  , mrPitch :: !Int
  , mrOnS :: !Double
  , mrOffS :: !Double
  , mrVel :: !Int
  , mrVoice :: !Text
  , mrTags :: !Text
  , mrGrace :: !Bool
  }

data MatchPerf = MatchPerf
  { mpDen :: !Int
  , mpRows :: [MatchRow]
  , mpDeletions :: !Int
  , mpInsertions :: !Int
  , mpPitchMismatch :: !Int
  , mpExtraTimesigs :: !Int
  , mpGraceSnotes :: !Int
  }

pcOf :: Char -> Maybe Int
pcOf c = lookup c
  [('C', 0), ('D', 2), ('E', 4), ('F', 5), ('G', 7), ('A', 9), ('B', 11)]

accOf :: Text -> Int
accOf a = case a of
  "n" -> 0; "#" -> 1; "b" -> -1; "x" -> 2; "bb" -> -2; "" -> 0; _ -> 0

-- Data.Text.Read replacement without importing it qualified everywhere:
decimal' :: Text -> Maybe (Int, Text)
decimal' t =
  let (ds, rest) = T.span isDigit t
   in if T.null ds then Nothing
      else Just (foldl' (\a c -> a * 10 + fromEnum c - 48) 0
                   (T.unpack ds), rest)

readInt :: Text -> Maybe Int
readInt t0 =
  let (neg, t) = case T.uncons t0 of
        Just ('-', r) -> (True, r)
        _ -> (False, t0)
   in case decimal' t of
        Just (v, rest) | T.null rest -> Just (if neg then -v else v)
        _ -> Nothing

readDouble :: Text -> Maybe Double
readDouble t = case reads (T.unpack t) of
  [(x, "")] -> Just x
  _ -> Nothing

-- | Parse one snote(...)-... line into its twelve regex groups.
parseSnoteLine :: Text -> Maybe (Text, Text, Text, Text, Double, Double,
                                 Text, Text)
parseSnoteLine l0 = do
  l1 <- T.stripPrefix "snote(" l0
  let (xmlId, r1) = T.breakOn "," l1
  r2 <- T.stripPrefix ",[" r1
  (letter, r3) <- T.uncons r2
  r4 <- T.stripPrefix "," r3
  let (acc, r5) = T.breakOn "]" r4
  r6 <- T.stripPrefix "]," r5
  let (octv, r7) = T.breakOn "," r6
  r8 <- T.stripPrefix "," r7
  -- meas:beat, offset, duration — three comma fields we do not use
  let dropField t = T.drop 1 (T.dropWhile (/= ',') t)
      r9 = dropField (dropField (dropField r8))
      (onB, r10) = T.breakOn "," r9
  r11 <- T.stripPrefix "," r10
  let (offB, r12) = T.breakOn "," r11
  r13 <- T.stripPrefix ",[" r12
  let (tags, r14) = T.breakOn "]" r13
  rhs <- T.stripPrefix "])-" r14
  ob <- readDouble onB
  fb <- readDouble offB
  pure (xmlId, T.singleton letter, acc, octv, ob, fb, tags, rhs)

parseNoteRhs :: Text -> Maybe (Int, Int, Int, Int)
parseNoteRhs r0 = do
  r1 <- T.stripPrefix "note(" r0
  let fields = T.splitOn "," r1
  case fields of
    (_nid : p : onT : offT : vel : _) ->
      (,,,) <$> readInt p <*> readInt onT <*> readInt offT
            <*> readInt (T.takeWhile isDigit vel)
    _ -> Nothing

-- | Parse a whole .match file. Mirrors note_align.parse_match.
parseMatch :: Text -> MatchPerf
parseMatch src = finish (foldl' step st0 (T.lines src))
  where
    st0 = (480 :: Int, 500000 :: Int, [] :: [(Int, Int)],
           MatchPerf 4 [] 0 0 0 0 0,
           [] :: [(Text, Int, Int, Int, Int, Text, Text, Double, Bool)])
    step (units, rate, tsigs, mp, rows) l
      | Just r <- T.stripPrefix "info(midiClockUnits," l
      , Just v <- readInt (T.takeWhile isDigit r) =
          (v, rate, tsigs, mp, rows)
      | Just r <- T.stripPrefix "info(midiClockRate," l
      , Just v <- readInt (T.takeWhile isDigit r) =
          (units, v, tsigs, mp, rows)
      | Just r <- T.stripPrefix "scoreprop(timeSignature," l
      , (numT, r2) <- T.breakOn "/" r
      , Just n <- readInt numT
      , Just d <- readInt (T.takeWhile isDigit (T.drop 1 r2)) =
          (units, rate, tsigs <> [(n, d)], mp, rows)
      | "insertion-note(" `T.isPrefixOf` l =
          (units, rate, tsigs, mp {mpInsertions = mpInsertions mp + 1},
           rows)
      | Just (xmlId, letter, acc, octv, ob, fb, tags, rhs)
          <- parseSnoteLine l =
          if "deletion" `T.isPrefixOf` rhs
            then (units, rate, tsigs,
                  mp {mpDeletions = mpDeletions mp + 1}, rows)
            else case parseNoteRhs rhs of
              Nothing -> (units, rate, tsigs,
                          mp {mpDeletions = mpDeletions mp + 1}, rows)
              Just (p, onT, offT, vel) ->
                let mismatch = case (,) <$> (pcOf =<< fmap fst
                                              (T.uncons (T.toUpper letter)))
                                        <*> readInt octv of
                      Just (pc, o) -> pc + accOf acc + 12 * (o + 1) /= p
                      Nothing -> True
                    grace = ob == fb
                    bits = if T.null tags then [] else T.splitOn "," tags
                    voice = if length bits >= 2
                              then T.intercalate "," (take 2 bits)
                              else tags
                    mp' = mp
                      { mpPitchMismatch =
                          mpPitchMismatch mp + (if mismatch then 1 else 0)
                      , mpGraceSnotes =
                          mpGraceSnotes mp + (if grace then 1 else 0)
                      }
                 in (units, rate, tsigs, mp',
                     rows <> [(xmlId, p, onT, offT, vel, voice, tags,
                               ob, grace)])
      | otherwise = (units, rate, tsigs, mp, rows)
    finish (units, rate, tsigs, mp, rows) =
      let den = case tsigs of ((_, d) : _) -> d; [] -> 4
          spt = fromIntegral rate / 1e6 / fromIntegral units :: Double
       in mp
            { mpDen = den
            , mpExtraTimesigs = max 0 (length tsigs - 1)
            , mpRows =
                [ MatchRow
                    (round (ob / fromIntegral den
                              * fromIntegral wnTicks))
                    xmlId p
                    (fromIntegral onT * spt) (fromIntegral offT * spt)
                    vel voice tags gr
                | (xmlId, p, onT, offT, vel, voice, tags, ob, gr) <- rows ]
            }

-- ---------------------------------------------------------------------
-- the IR side, in-process (mirrors load_ir_notes over the JSON —
-- all continuous values pass through Double first, like the JSON did)

data SubNote = SubNote
  { sbPitch :: !Int
  , sbVel :: !Int
  , sbOnS :: !Double
  , sbDurS :: !Double
  , sbOnWn :: !Double
  , sbDurWn :: !Double
  }

data IRRep = IRRep
  { irVel :: !Int
  , irOnS :: !Double
  , irDurS :: !Double
  , irOnWn :: !Double
  , irDurWn :: !Double
  , irCh :: !Int
  , irRules :: Map Text Text
  , irGatePct :: Maybe Int
  , irGateLabel :: Maybe Text
  , irIsFinal :: !Bool
  , irIsOrn :: !Bool
  , irGroup :: Maybe [SubNote]
  }

-- | Group a Performance's notes by (srcWn ticks, srcPitch); one
-- representative per channel (earliest keystroke), carrying the whole
-- realisation when the group is an ornament.
loadIrNotes :: Performance -> Map (Int, Int) [IRRep]
loadIrNotes (Performance tmap tracks whys _cads _end) =
  Map.mapWithKey reps grouped
  where
    whyMap = Map.fromList whys
    sub pn =
      let Seconds onS = secondsAt tmap (pnOnset pn)
          Seconds offS = secondsAt tmap (pnOnset pn + pnDur pn)
          WholeNotes onW = pnOnset pn
          WholeNotes durW = pnDur pn
       in ( pn
          , SubNote (pnPitch pn) (pnVel pn) onS (offS - onS)
              (fromRational onW) (fromRational durW) )
    keyOf pn =
      let WholeNotes w = pnSrcOn pn
       in ( round (fromRational w * fromIntegral wnTicks :: Double)
          , pnSrcPitch pn )
    grouped = foldl'
      (\m pn -> Map.insertWith (flip (<>)) (keyOf pn) [sub pn] m)
      Map.empty
      (concat tracks)
    maxSrc = maximum (0 : map fst (Map.keys grouped))
    reps (kt, _) ns = map mk (uniq (sort (map (pnChannel . fst) ns)))
      where
        mk ch =
          let mine = sortOn (sbOnS . snd)
                       [x | x <- ns, pnChannel (fst x) == ch]
              (pn0, s0) = head mine
              ws = Map.findWithDefault []
                     (pnChannel pn0, pnIndex pn0) whyMap
              gate = firstGate ws
              rules = Map.fromList
                [ (T.pack (whyRule w), T.pack (whyDelta w)) | w <- ws ]
           in IRRep (sbVel s0) (sbOnS s0) (sbDurS s0) (sbOnWn s0)
                (sbDurWn s0) ch rules (fst <$> gate) (snd <$> gate)
                (kt == maxSrc)
                (length mine > 1)
                (if length mine > 1
                   then Just (map snd mine) else Nothing)

uniq :: Eq a => [a] -> [a]
uniq [] = []
uniq (x : xs) = x : uniq (dropWhile (== x) xs)

-- | "gate 78% (détaché)" out of the articulation why's delta.
firstGate :: [Why] -> Maybe (Int, Text)
firstGate ws = case mapMaybe g ws of
  (x : _) -> Just x
  [] -> Nothing
  where
    g w = do
      r <- T.stripPrefix "gate " (T.pack (whyDelta w))
      let (numT, rest) = T.span isDigit r
      n <- readInt numT
      r2 <- T.stripPrefix "% (" rest
      let (lbl, r3) = T.breakOn ")" r2
      if T.null r3 then Nothing else Just (n, lbl)

-- ---------------------------------------------------------------------
-- rows and passes

data Pass = PassExact | PassSeg | PassFuzzy | PassOrn
  deriving (Eq, Show)

data BridgeRow = BridgeRow
  { brWn :: !Double
  , brPitch :: !Int
  , brXmlId :: !Text
  , brVoice :: !Text
  , brPass :: !Pass
  , brHumanVel :: !Int
  , brHumanOnS :: !Double
  , brHumanOffS :: !Double
  , brOtbVel :: !Int
  , brOtbOnS :: !Double
  , brOtbDurS :: !Double
  , brOtbOnWn :: Maybe Double
  , brOtbDurWn :: Maybe Double
  , brCh :: Maybe Int
  , brRules :: Map Text Text
  , brIsFinal :: !Bool
  , brGrace :: !Bool
  , brGatePct :: Maybe Int
  , brGateLabel :: Maybe Text
  , brIsOrn :: !Bool
  }

data Counters = Counters
  { cMatched, cExact, cSeg, cFuzzy, cOrn, cSegRuns :: !Int
  , cUnmatched, cDeleted, cInsertions, cPitchMismatch :: !Int
  , cGraceSnotes, cExtraTimesigs :: !Int
  , cOffsetWn :: !Double
  , cScale :: (Int, Int)
  , cTotalSnotes :: !Int
  }

scales :: [(Int, Int)]
scales = [(1, 1), (2, 3), (3, 2), (1, 2), (2, 1), (1, 3), (3, 1),
          (3, 4), (4, 3)]

affine :: [(Int, Int)] -> [(Int, Int)] -> (Int, Int, Int)
affine irKeys matchKeys = go (0 :: Int, 1, 1, 0) scales
  where
    irSet = Map.fromList [(k, ()) | k <- irKeys]
    firstIr = Map.fromListWith min [(p, t) | (t, p) <- irKeys]
    go (_, n, d, o) [] = (n, d, o)
    go best@(bs, _, _, _) ((num, den) : rest) =
      let scaled = Map.fromListWith (flip (<>))
            [ (p, [t * num `div` den])
            | (t, p) <- matchKeys, (t * num) `mod` den == 0 ]
          firstM = Map.map minimum scaled
          diffs = [ fi - fm
                  | (p, fi) <- Map.toList firstIr
                  , Just fm <- [Map.lookup p firstM] ]
          best' =
            if null diffs then best
            else
              let counts = Map.fromListWith (+) [(x, 1 :: Int) | x <- diffs]
                  mode = fst (minimumBy
                          (comparing (\(v, c) -> (negate c, v)))
                          (Map.toList counts))
                  offs = if mode == 0 then [0] else [0, mode]
                  try b off =
                    let got = length
                          [ () | (p, ts) <- Map.toList scaled, t <- ts
                          , Map.member (t + off, p) irSet ]
                     in if got > (\(g, _, _, _) -> g) b
                          then (got, num, den, off) else b
               in foldl' try best offs
       in go (if (\(g, _, _, _) -> g) best' > bs then best' else best)
             rest

-- (the modes: python used max(sorted(set), key=count) — smallest value
-- among count ties; mirrored above by comparing (negate count, value))

-- pool operations: Map key -> [IRRep], pop-front
takeRep :: (Int, Int) -> Map (Int, Int) [IRRep]
        -> (Maybe IRRep, Map (Int, Int) [IRRep])
takeRep k pool = case Map.lookup k pool of
  Just (r : rs) -> (Just r, Map.insert k rs pool)
  _ -> (Nothing, pool)

mkRow :: (Int, Int) -> MatchRow -> IRRep -> Pass -> BridgeRow
mkRow (kt, _) mr rep how = BridgeRow
  { brWn = fromIntegral kt / fromIntegral wnTicks
  , brPitch = mrPitch mr
  , brXmlId = mrXmlId mr
  , brVoice = mrVoice mr
  , brPass = how
  , brHumanVel = mrVel mr
  , brHumanOnS = mrOnS mr
  , brHumanOffS = mrOffS mr
  , brOtbVel = irVel rep
  , brOtbOnS = irOnS rep
  , brOtbDurS = irDurS rep
  , brOtbOnWn = Just (irOnWn rep)
  , brOtbDurWn = Just (irDurWn rep)
  , brCh = Just (irCh rep)
  , brRules = irRules rep
  , brIsFinal = irIsFinal rep
  , brGrace = mrGrace mr
  , brGatePct = irGatePct rep
  , brGateLabel = irGateLabel rep
  , brIsOrn = irIsOrn rep
  }

-- | The four-pass join. Mirrors note_align.bridge.
bridge :: Map (Int, Int) [IRRep] -> MatchPerf -> ([BridgeRow], Counters)
bridge irNotes mp = (rowsAll, counters)
  where
    (num, den, offset) = affine
      (Map.keys irNotes)
      (uniq (sort [(mrKey r, mrPitch r) | r <- mpRows mp]))

    xform t = round (fromIntegral t * fromIntegral num
                       / fromIntegral den :: Double) + offset

    exactStep (pool, rows, misses) mr =
      let kt = xform (mrKey mr)
          key = (kt, mrPitch mr)
       in case takeRep key pool of
            (Just rep, pool') ->
              (pool', rows <> [mkRow key mr rep PassExact], misses)
            (Nothing, _) ->
              (pool, rows, misses <> [(kt, mr)])
    (pool1, rows1, misses1) =
      foldl' exactStep (irNotes, [], []) (mpRows mp)
    nExact = length rows1

    (pool2, rows2, misses2, nSegRuns) = segmented pool1 misses1 rows1
    (pool3, rows3, misses3) = fuzzy pool2 misses2 rows2
    (rows4, misses4) = ornament irNotes misses3 rows3
    rowsAll = rows4
    _ = pool3

    counters = Counters
      { cMatched = length rowsAll
      , cExact = nExact
      , cSeg = length [() | r <- rowsAll, brPass r == PassSeg]
      , cFuzzy = length [() | r <- rowsAll, brPass r == PassFuzzy]
      , cOrn = length [() | r <- rowsAll, brPass r == PassOrn]
      , cSegRuns = nSegRuns
      , cUnmatched = length misses4
      , cDeleted = mpDeletions mp
      , cInsertions = mpInsertions mp
      , cPitchMismatch = mpPitchMismatch mp
      , cGraceSnotes = mpGraceSnotes mp
      , cExtraTimesigs = mpExtraTimesigs mp
      , cOffsetWn = fromIntegral offset / fromIntegral wnTicks
      , cScale = (num, den)
      , cTotalSnotes = length (mpRows mp) + mpDeletions mp
      }

    segmented pool misses rows = go pool misses rows 0 []
      where
        go p [] rs runs still = (p, rs, reverse still, runs)
        go p ms@(headMiss : _) rs runs still =
          let (t0, mr0) = headMiss
              p0 = mrPitch mr0
              cands = sort (uniq (sort
                [ it - t0
                | ((it, ip), reps) <- Map.toList p
                , ip == p0, not (null reps)
                , abs (it - t0) <= 4 * wnTicks ]))
              probe = take 12 ms
              (bestOff, bestScore) = foldl'
                (\(bo, bsc) off ->
                   let sc = length
                         [ () | (t, mr) <- probe
                         , maybe False (not . null)
                             (Map.lookup (t + off, mrPitch mr) p) ]
                    in if sc > bsc then (Just off, sc) else (bo, bsc))
                (Nothing, 0) cands
              threshold = max 3 ((3 * length probe) `div` 4)
           in case bestOff of
                Just off | bestScore >= threshold ->
                  consume p ms rs (runs + 1) still off 0
                _ -> go p (drop 1 ms) rs runs (headMiss : still)
        consume p [] rs runs still _ _ = (p, rs, reverse still, runs)
        consume p ms@((t, mr) : rest) rs runs still off fails
          | fails >= 4 = go p ms rs runs still
          | otherwise =
              case takeRep (t + off, mrPitch mr) p of
                (Just rep, p') ->
                  consume p' rest
                    (rs <> [mkRow (t + off, mrPitch mr) mr rep PassSeg])
                    runs still off 0
                (Nothing, _) ->
                  consume p rest rs runs ((t, mr) : still) off (fails + 1)

    fuzzy pool misses rows = go pool (sortOn fst misses) rows []
      where
        window = wnTicks `div` 16
        go p [] rs still = (p, rs, reverse still)
        go p ((t, mr) : rest) rs still =
          let near = sort
                [ (abs (it - t), it)
                | ((it, ip), reps) <- Map.toList p
                , ip == mrPitch mr, not (null reps)
                , abs (it - t) <= window ]
           in case near of
                ((_, it) : _) ->
                  case takeRep (it, mrPitch mr) p of
                    (Just rep, p') ->
                      go p' rest
                        (rs <> [mkRow (it, mrPitch mr) mr rep PassFuzzy])
                        still
                    (Nothing, _) -> go p rest rs ((t, mr) : still)
                [] -> go p rest rs ((t, mr) : still)

    ornament notes misses rows = walk (sortOn fst misses) rows [] []
      where
        -- groups indexed for identity: (group ordinal, start, end, subs)
        groups = zip [0 :: Int ..]
          [ (t, end, subs)
          | ((t, _p), reps) <- Map.toAscList notes
          , rep <- reps
          , Just subs <- [irGroup rep]
          , let end = maximum
                  [ round ((sbOnWn n + sbDurWn n)
                             * fromIntegral wnTicks :: Double)
                  | n <- subs ]
          ]
        walk [] rs _used still = (rs, reverse still)
        walk ((t, mr) : rest) rs used still =
          let p = mrPitch mr
              best = foldl'
                (\b (gid, (s0, e0, subs)) ->
                   if s0 - 60 <= t && t <= e0 + 60
                     then foldl'
                       (\b' (si, n) ->
                          if (gid, si) `elem` used || sbPitch n /= p
                            then b'
                            else
                              let d = abs (round (sbOnWn n
                                        * fromIntegral wnTicks
                                          :: Double) - t)
                               in case b' of
                                    Just (bd, _, _, _) | bd <= d -> b'
                                    _ -> Just (d, gid, si, n))
                       b (zip [0 :: Int ..] subs)
                     else b)
                Nothing groups
           in case best of
                Just (d, gid, si, n) | d <= wnTicks `div` 4 ->
                  let rep = IRRep (sbVel n) (sbOnS n) (sbDurS n)
                              0 0 0 Map.empty Nothing Nothing False True
                              Nothing
                      row = (mkRow (t, p) mr rep PassOrn)
                        { brOtbOnWn = Nothing, brOtbDurWn = Nothing
                        , brCh = Nothing, brIsFinal = False }
                   in walk rest (rs <> [row]) ((gid, si) : used) still
                _ -> walk rest rs used ((t, mr) : still)
