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
import OTB.Kern.Lexer (lexRecord)
import OTB.Kern.Spine
import OTB.Kern.Token
import OTB.Score
import OTB.Units (Bpm (..), WholeNotes)

type ParseError = String

data PSt = PSt
  { psSpines :: SpineState
  , psTempo :: Maybe Bpm
  , psDone :: Map Int [ScoreNote] -- ^ per voice, reverse order
  , psTies :: Map (Int, Int) ScoreNote -- ^ open ties by (voice, pitch)
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
  final <- foldM step (PSt st0 Nothing Map.empty Map.empty) body
  case Map.keys (psTies final) of
    [] -> pure ()
    ks -> Left ("unclosed ties at EOF: " <> show ks)
  let voices =
        [ Voice i (sortOn snOnset (reverse ns))
        | (i, ns) <- Map.toAscList (psDone final)
        , not (null ns)
        ]
  Right (Score (fromMaybe defaultTempo (psTempo final)) voices)
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
        { psSpines = fromMaybe (psSpines st) changed
        , psTempo = case psTempo st of Nothing -> tempo'; t -> t
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
    firstTempo is = case [b | ITempo b <- is] of (b : _) -> Just b; [] -> Nothing
    -- advance each path's clock by its field's (first) note duration
    advance = map (\(p, f) -> (advancePath p f, f))
    advancePath p (FData (t : _)) = p {pClock = pClock p + ntDur t}
    advancePath p _ = p

-- | Fold one (path, field) into the note state. Uses the path's clock
-- *before* advancement, so it is applied to the un-advanced pairs — the
-- caller advances clocks separately via 'advance'.
dataField :: PSt -> (Path, Field) -> PSt
dataField st (p, FData toks)
  | pKern p = foldl (noteTok (pVoice p) (pClock p)) st toks
dataField st _ = st

noteTok :: Int -> WholeNotes -> PSt -> NoteTok -> PSt
noteTok _ _ st (NoteTok d _ _ _) | d <= 0 = st -- grace notes: skipped for now
noteTok _ _ st (NoteTok _ Nothing _ _) = st -- rest: clock already advanced
noteTok voice onset st (NoteTok d (Just pit) tie marks) =
  case tie of
    TieNone -> emit (ScoreNote onset d pit marks)
    TieOpen -> st {psTies = Map.insert key (ScoreNote onset d pit marks) (psTies st)}
    TieContinue -> extend
    TieClose ->
      case Map.lookup key (psTies st) of
        Nothing -> emit (ScoreNote onset d pit marks) -- stray close: keep the sound
        Just open ->
          (emit open {snDur = snDur open + d}) {psTies = Map.delete key (psTies st)}
  where
    key = (voice, pit)
    emit sn = st {psDone = Map.insertWith (<>) voice [sn] (psDone st)}
    extend = case Map.lookup key (psTies st) of
      Nothing -> st
      Just open -> st {psTies = Map.insert key open {snDur = snDur open + d} (psTies st)}

mapLeft :: (a -> a') -> Either a b -> Either a' b
mapLeft f = either (Left . f) Right
