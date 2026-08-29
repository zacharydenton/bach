-- | Drives lexed records through the spine machine into a 'Score'.
--
-- Tie resolution strategy: an open tie is held aside keyed by
-- (voice, pitch) and grows by each continuation's duration; only on
-- @]@ does it land in the note list. Kern ties stay within a voice in
-- this corpus; a tie left open at end of file is surfaced as an error.
--
-- License: GPL-2.0-or-later.
module OTB.Kern.Parser
  ( parseKern
  , ParseError
  ) where

import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as TR
import OTB.Kern.Lexer (lexRecord)
import OTB.Kern.Spine
import OTB.Kern.Token
import OTB.Score
import OTB.Units (Bpm (..), WholeNotes)

type ParseError = String

data PSt = PSt
  { psSpines :: SpineState
  , psTempo :: Maybe Bpm
  , psMeter :: [(WholeNotes, (Int, Int))] -- ^ reverse order
  , psDrifts :: !Int
  , psGrace :: !Int -- ^ grace tokens skipped (see 'noteTok')
  , psDone :: Map Int [ScoreNote] -- ^ per voice, reverse order
  , psTies :: Map (Int, Int) [ScoreNote]
    -- ^ open ties by (voice, pitch) — a FIFO, because two sub-spines of one
    -- voice can hold overlapping ties on the same pitch (wtc2p02 has
    -- simultaneous C5 ties in two lanes); close pops the earliest open
  }

parseKern :: Bpm -> Text -> Either ParseError Score
parseKern defaultTempo src = do
  let records =
        [ r
        | (n, line) <- zip [1 ..] (T.lines src)
        , not (T.null (T.strip line))
        , let r = lexRecord n line
        , not (isCommentRecord r)
        ]
  (header, body) <- case records of
    (h@(Record _ fs) : rest)
      | any isExclusive fs -> Right (h, rest)
    _ -> Left "no **kern exclusive interpretation record found"
  st0 <- initFromHeader header
  final <- foldM step (PSt st0 Nothing [] 0 0 Map.empty Map.empty) body
  -- flush unclosed ties as sounding notes: they *were* heard for their
  -- accumulated duration (usually an enharmonic respelling at the close, or
  -- an editorial quirk); the count is surfaced, not fatal
  let leftovers =
        [ (v, sn) | ((v, _), sns) <- Map.toList (psTies final), sn <- sns ]
      flushed =
        foldl
          (\m (v, sn) -> Map.insertWith (<>) v [sn] m)
          (psDone final)
          leftovers
      voices =
        [ Voice i (sortOn snOnset (reverse ns))
        | (i, ns) <- Map.toAscList flushed
        , not (null ns)
        ]
  Right
    (Score (fromMaybe defaultTempo (psTempo final)) voices
       (length leftovers) (psDrifts final) (reverse (psMeter final))
       (psGrace final))
  where
    foldM f z = foldl (\acc x -> acc >>= \s -> f s x) (Right z)

isCommentRecord :: Record -> Bool
isCommentRecord (Record _ fs) = all (\case FComment -> True; _ -> False) fs

isExclusive :: Field -> Bool
isExclusive (FInterp IKernStart) = True
isExclusive (FInterp (IOtherExclusive _)) = True
isExclusive _ = False

initFromHeader :: Record -> Either ParseError SpineState
initFromHeader (Record n fs) = do
  is <- traverse interpOf fs
  Right (initState is)
  where
    interpOf (FInterp i) = Right i
    interpOf f = Left ("line " <> show n <> ": non-interpretation in header: " <> show f)

step :: PSt -> Record -> Either ParseError PSt
step st (Record n fs)
  | all isBarOrNull fs = Right st -- barline record: no time semantics here
  | any isInterp fs = do
      is <- traverse asInterp fs
      let tempo' = firstTempo is
      changed <-
        mapLeft (("line " <> show n <> ": ") <>) (applyInterps is (psSpines st))
      Right st
        { psSpines = maybe (psSpines st) fst changed
        , psDrifts = psDrifts st + maybe 0 snd changed
        , psTempo = case psTempo st of Nothing -> tempo'; t -> t
        , psMeter = case firstMeter is of
            Nothing -> psMeter st
            Just m -> (now, m) : dropWhile ((== now) . fst) (psMeter st)
        }
  | otherwise = do
      pairs <-
        mapLeft (("line " <> show n <> ": ") <>) (dataPaths (psSpines st) fs)
      Right (foldl dataField st {psSpines = map fst (advance pairs)} pairs)
  where
    isInterp (FInterp _) = True
    isInterp _ = False
    isBarOrNull f = case f of FBar _ -> True; FNull -> True; FComment -> True; _ -> False
    asInterp (FInterp i) = Right i
    asInterp f = Left ("line " <> show n <> ": mixed interpretation record: " <> show f)
    -- an interpretation record sits at one instant; all live kern paths
    -- agree on it (the corpus sweep would surface a drift as a merge error)
    now = case [pClock p | p <- psSpines st, pKern p] of
      (c : _) -> c
      [] -> 0
    firstTempo is = case [b | ITempo b <- is] of (b : _) -> Just b; [] -> Nothing
    firstMeter is = case [m | IMeter txt <- is, Just m <- [readMeter txt]] of
      (m : _) -> Just m; [] -> Nothing
    readMeter txt = case T.splitOn "/" txt of
      [a, b]
        | Right (n, _) <- decimalT a
        , Right (d, _) <- decimalT b
        , n > 0, d > 0 -> Just (n, d)
      _ -> Nothing
    decimalT = TR.decimal
    -- advance each path's clock by its field's (first) note duration
    advance = map (\(p, f) -> (advancePath p f, f))
    advancePath p (FData (t : _)) = p {pClock = pClock p + ntDur t}
    advancePath p _ = p

-- | Fold one (path, field) into the note state. Uses the path's clock
-- *before* advancement, so it is applied to the un-advanced pairs — the
-- caller advances clocks separately via 'advance'.
dataField :: PSt -> (Path, Field) -> PSt
dataField st (p, FData toks)
  | pKern p = foldl (noteTok (pVoice p) (pLane p) (pClock p)) st toks
dataField st _ = st

noteTok :: Int -> Int -> WholeNotes -> PSt -> NoteTok -> PSt
-- grace notes (zero duration): not realised yet — dropped, but counted
noteTok _ _ _ st (NoteTok d _ _ _) | d <= 0 = st {psGrace = psGrace st + 1}
noteTok _ _ _ st (NoteTok _ Nothing _ _) = st -- rest: clock already advanced
noteTok voice lane onset st (NoteTok d (Just pit) tie marks) =
  case tie of
    TieNone -> emit fresh
    TieOpen ->
      st {psTies = Map.insertWith (flip (<>)) key [fresh] (psTies st)}
    -- an ornament on a continuation/close cannot be "unioned" into the
    -- note: a trill that starts mid-way through a held pitch is a fresh
    -- attack from the upper auxiliary. So the held part lands as it is and
    -- the ornamented token starts a new note (which may itself tie on).
    TieContinue
      | ornamented ->
          restruck {psTies = Map.insertWith (flip (<>)) key [fresh] (psTies restruck)}
      | otherwise -> extendEarliest
    TieClose
      | ornamented -> emitIn restruck fresh
      | otherwise ->
          case popOpen st of
            (Just open, st') -> emitIn st' (extend open)
            -- no open on this key: a stray close. The corpus really contains
            -- these — e.g. wtc2p02 opens [8cc whose continuation is a chord
            -- subtoken without the closing ] — so keep the sound and move on;
            -- the opposite half (opens never closed) is flushed at EOF and
            -- counted in scTieLeftovers.
            (Nothing, _) -> emit fresh
  where
    key = (voice, pit)
    fresh = ScoreNote onset d pit marks lane [(d, marks)] (onset, pit)
    ornamented = any isOrnamentMark marks
    -- a continuation/close token adds a segment with *its own* marks; the
    -- union in snMarks is for marks that describe the whole note (slurs,
    -- accents) — segment-local ones are read from snSegs (fermata) or
    -- split off here (ornaments)
    extend open =
      open { snDur = snDur open + d
           , snMarks = snMarks open <> filter (`notElem` snMarks open) marks
           , snSegs = snSegs open <> [(d, marks)] }
    emitIn s sn = s {psDone = Map.insertWith (<>) voice [sn] (psDone s)}
    emit = emitIn st
    -- pop the earliest open tie on this key
    popOpen s = case Map.lookup key (psTies s) of
      Just (open : rest') ->
        ( Just open
        , s {psTies = if null rest' then Map.delete key (psTies s)
                      else Map.insert key rest' (psTies s)} )
      _ -> (Nothing, s)
    -- the held note so far, closed early because this token restrikes
    restruck = case popOpen st of
      (Just open, s) -> emitIn s open
      (Nothing, s) -> s
    extendEarliest = case Map.lookup key (psTies st) of
      Just (open : rest') ->
        st {psTies = Map.insert key (extend open : rest') (psTies st)}
      _ -> st

isOrnamentMark :: Mark -> Bool
isOrnamentMark m = case m of
  Trill _ -> True; Mordent _ -> True; InvMordent _ -> True
  Turn -> True; InvTurn -> True
  _ -> False

mapLeft :: (a -> a') -> Either a b -> Either a' b
mapLeft f = either (Left . f) Right
