-- | Drives lexed records through the spine machine into a 'Score'.
--
-- Tie resolution strategy: an open tie is held aside keyed by
-- (voice, staff position) and grows by each continuation's duration; only
-- on @]@ does it land in the note list. Kern ties stay within a voice in
-- this corpus; a tie left open at end of file is surfaced as an error.
--
-- License: GPL-2.0-or-later.
module OTB.Kern.Parser
  ( parseKern
  , ParseError
  ) where

import Data.List (nub, sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as TR
import OTB.Kern.Lexer (lexRecord)
import OTB.Kern.Spine
import OTB.Kern.Token
import OTB.Pitch (spDegree)
import OTB.Score
import OTB.Units (Bpm (..), WholeNotes (..))

type ParseError = String

data PSt = PSt
  { psSpines :: SpineState
  , psTempo :: Maybe Bpm
  , psMeter :: [(WholeNotes, (Int, Int))] -- ^ reverse order
  , psDrifts :: !Int
  , psGrace :: !Int -- ^ grace tokens skipped (see 'noteTok')
  , psRestHolds :: [(WholeNotes, WholeNotes)]
    -- ^ fermatas on rests, (onset, dur), reverse order — realised by the
    -- Player as tempo-map holds
  , psFirstBar :: Maybe WholeNotes
    -- ^ clock at the first barline record: data before it is an
    -- anacrusis (six WTC pieces open with one), and the bar grid must
    -- anchor at the first FULL bar
  , psDone :: Map Int [ScoreNote] -- ^ per voice, reverse order
  , psTies :: Map (Int, Int) [ScoreNote]
    -- ^ open ties by (voice, staff position) — spelled degree, so a
    -- continuation that drops the accidental still finds its open — a
    -- FIFO, because two sub-spines of one voice can hold overlapping ties
    -- on the same position (wtc2p02 has simultaneous C5 ties in two
    -- lanes); close pops the earliest open
  }

parseKern :: Bpm -> Text -> Either ParseError Score
parseKern defaultTempo src = do
  let records =
        [ r
        | (n, line0) <- zip [1 ..] (T.lines src)
        -- CRLF input: T.lines keeps the \r, which would corrupt the
        -- last field of every record (".\r" is not a null token)
        , let line = T.dropWhileEnd (== '\r') line0
        , not (T.null (T.strip line))
        , let r = lexRecord n line
        , not (isCommentRecord r)
        ]
  (header, body) <- case records of
    (h@(Record _ fs) : rest)
      | any isExclusive fs -> Right (h, rest)
    _ -> Left "no **kern exclusive interpretation record found"
  st0 <- initFromHeader header
  final <-
    foldM (step defaultTempo)
      (PSt st0 Nothing [] 0 0 [] Nothing Map.empty Map.empty) body
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
      -- anchor the bar grid at the first FULL bar: data before the
      -- first barline record is an anacrusis, so the first meter
      -- entry's onset moves to the pickup's end — every metrical
      -- consumer (Sloboda accents, bar arches, otb explain --bar)
      -- aligns at once, and anacrusis notes fall before the grid,
      -- which reads as "no downbeat accent" (an upbeat, correctly)
      meter0 = reverse (psMeter final)
      pickup = case (psFirstBar final, meter0) of
        (Just (WholeNotes fb), (_, (n, d)) : _) | n > 0, d > 0 ->
          let bl = fromIntegral n / fromIntegral d
           in WholeNotes (fb - bl * fromIntegral (floor (fb / bl) :: Integer))
        _ -> 0
      meter' = case meter0 of
        ((0, m) : more) | pickup > 0 -> (pickup, m) : more
        _ -> meter0
  Right
    (Score (fromMaybe defaultTempo (psTempo final)) voices
       (length leftovers) (psDrifts final) meter'
       (psGrace final) (reverse (psRestHolds final))
       (psTempo final /= Nothing))
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

step :: Bpm -> PSt -> Record -> Either ParseError PSt
step dflt st (Record n fs)
  -- barline record: no time semantics, but the FIRST one's clock marks
  -- where the bar grid begins (see the anacrusis note in parseKern)
  | all isBarOrNull fs =
      Right st {psFirstBar = case psFirstBar st of
                  Nothing | any isBar fs -> Just now
                  fb -> fb}
  | any isInterp fs = do
      is <- traverse asInterp fs
      -- the Score holds ONE tempo, so (a) a record's tempo fields must
      -- agree with each other, and (b) once music has sounded the
      -- EFFECTIVE tempo (declared, or the fallback) may not change —
      -- which also catches a late FIRST *MM that would retroactively
      -- re-tempo everything already heard. (None in the corpus — these
      -- are the loud guards for other kern input.)
      tempo' <- case nub [b | ITempo b <- is] of
        [] -> Right Nothing
        [t] -> Right (Just t)
        ts -> Left ("line " <> show n
                      <> ": conflicting *MM in one record: " <> show ts)
      case tempo' of
        Just t' | now > 0, t' /= fromMaybe dflt (psTempo st) ->
          Left ("line " <> show n
                  <> ": mid-piece *MM tempo change is not supported")
        _ -> Right ()
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
    isBar f = case f of FBar _ -> True; _ -> False
    asInterp (FInterp i) = Right i
    asInterp f = Left ("line " <> show n <> ": mixed interpretation record: " <> show f)
    -- an interpretation record sits at one instant; all live kern paths
    -- agree on it (the corpus sweep would surface a drift as a merge error)
    now = case [pClock p | p <- psSpines st, pKern p] of
      (c : _) -> c
      [] -> 0
    firstMeter is = case [m | IMeter txt <- is, Just m <- [readMeter txt]] of
      (m : _) -> Just m; [] -> Nothing
    -- full consumption required: *M3+2/8 (an additive meter) must not
    -- quietly read as 3/8
    readMeter txt = case T.splitOn "/" txt of
      [a, b]
        | Right (nu, ra) <- decimalT a, T.null ra
        , Right (d, rb) <- decimalT b, T.null rb
        , nu > 0, d > 0 -> Just (nu, d)
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
-- grace notes (zero duration): retained as zero-duration notes carrying
-- the Grace mark — realisation (on the beat, stealing from the main
-- note) is interpretation and lives in the Player. A pitchless
-- zero-duration token has nothing to realise; that loss is still counted.
noteTok voice lane onset st (NoteTok d _ mpit mspell _ marks)
  | d <= 0 = case mpit of
      Just pit
        | Grace `elem` marks ->
            let g = ScoreNote onset 0 pit marks lane [(0, marks)] (onset, pit)
                      mspell 0
             in st {psDone = Map.insertWith (<>) voice [g] (psDone st)}
      _ -> st {psGrace = psGrace st + 1}
-- rest: clock already advanced. A fermata on a rest has no note to
-- stretch — its (onset, dur) span is recorded for the Player, which
-- realises the hold through the tempo map
noteTok _ _ onset st (NoteTok d _ Nothing _ _ marks)
  | Fermata `elem` marks = st {psRestHolds = (onset, d) : psRestHolds st}
  | otherwise = st
noteTok voice lane onset st (NoteTok d dots (Just pit) mspell tie marks) =
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
    -- ties are grouped by STAFF POSITION (letter + octave): kern law
    -- respells the accidental on every token, and the corpus occasionally
    -- drops it on a continuation or close ([2a- … 2a_) — under MIDI
    -- keying such a tie broke into a stray close plus an EOF leftover.
    -- Within the group, 'pick' still prefers the exact chromatic match.
    -- The MIDI fallback only serves tokens without spelling, which the
    -- lexer never produces for pitched notes.
    key = (voice, maybe pit spDegree mspell)
    fresh =
      ScoreNote onset d pit marks lane [(d, marks)] (onset, pit) mspell dots
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
    -- Which open does this token continue/close? The earliest exact
    -- chromatic match (the old FIFO — wtc2p02's overlapping same-pitch
    -- ties depend on it); failing that, an open at the same staff
    -- position whose accumulated end lands exactly on this token's onset
    -- — a real tie is temporally adjacent, so this is the dropped-
    -- accidental lapse and nothing else. Adjacency is what keeps a stray
    -- @]@ on a passing tone (wtc1p07:150's 32an]) from swallowing an
    -- A-flat tie open since another beat.
    pick opens =
      case break ((== pit) . snPitch) opens of
        (before, o : after) -> Just (o, before <> after)
        _ -> case break (\o -> snOnset o + snDur o == onset) opens of
          (before, o : after) -> Just (o, before <> after)
          _ -> Nothing
    popOpen s = case Map.lookup key (psTies s) >>= pick of
      Just (open, rest') ->
        ( Just open
        , s {psTies = if null rest' then Map.delete key (psTies s)
                      else Map.insert key rest' (psTies s)} )
      _ -> (Nothing, s)
    -- the held note so far, closed early because this token restrikes
    restruck = case popOpen st of
      (Just open, s) -> emitIn s open
      (Nothing, s) -> s
    extendEarliest = case Map.lookup key (psTies st) >>= pick of
      Just (open, rest') ->
        st {psTies = Map.insert key (extend open : rest') (psTies st)}
      -- no open for this token: a stray continuation. The corpus ties
      -- across top-level spines a few times (e.g. wtc1p04:240's 2.cc#_
      -- continues a [4cc# opened in the neighbouring spine), which the
      -- per-voice key cannot match — keep the sound, exactly like the
      -- stray close below; the abandoned open still flushes at EOF into
      -- scTieLeftovers.
      _ -> emit fresh

isOrnamentMark :: Mark -> Bool
isOrnamentMark m = case m of
  Trill _ -> True; Mordent _ -> True; InvMordent _ -> True
  Turn _ _ -> True; InvTurn _ _ -> True
  _ -> False

mapLeft :: (a -> a') -> Either a b -> Either a' b
mapLeft f = either (Left . f) Right
