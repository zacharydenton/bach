-- | Beat-level tempo evaluation against human performances, in-process.
--
-- The Python research rigs (asap_eval, piece_fit, knob_landscape) spent
-- most of their wall time on orchestration: an @otb compile@ subprocess
-- per candidate per piece, a multi-megabyte JSON round trip, and
-- GIL-bound row crunching. The actual work — compile a piece, integrate
-- its tempo map at the annotated beat positions, correlate local tempo
-- with each human's — is a few hundred milliseconds of Haskell. This
-- module does exactly that, so @otb eval@ can score a whole corpus in
-- one process with pieces in parallel.
--
-- The annotation formats are ASAP's own (also emitted by the MAESTRO
-- aligner's mirror tree): @midi_score_annotations.txt@ — deadpan
-- constant-tempo beat times, three tab columns, @db,N/D@ downbeat/meter
-- tags — and @<perf>_annotations.txt@ — the same rows with real
-- seconds. Beat positions in whole notes are recovered exactly as the
-- Python reference does (asap_eval.score_beat_positions): score
-- position is proportional to the first time column, with the
-- whole-notes-per-second factor taken as the median over full
-- downbeat-to-downbeat bars measured against their stated meter — the
-- one mapping that survives pickups and partial final bars.
--
-- License: GPL-2.0-or-later.
module OTB.Eval
  ( PerfR (..)
  , scoreBeatPositions
  , perfBeatTimes
  , localTempo
  , pearson
  , loadDirAnnotations
  , scorePerformances
  ) where

import Data.List (sort)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import OTB.Units (Bpm (..), Seconds (..), WholeNotes (..), secondsAt)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (takeFileName, (</>))

-- | One human performance's agreement with our tempo curve.
data PerfR = PerfR
  { prPerformer :: String
  , prR :: Double
  , prBeats :: Int
  }

-- ---------------------------------------------------------------------
-- annotation parsing (the ASAP 3-column format)

data AnnRow = AnnRow
  { arTime :: !Double
  , arDownbeat :: !Bool
  , arMeter :: Maybe (Int, Int)
  }

parseAnnotations :: Text -> [AnnRow]
parseAnnotations = go Nothing . T.lines
  where
    go _ [] = []
    go meter (l : ls) = case T.splitOn "\t" l of
      (t : _ : tag : _) | Just x <- readD t ->
        let bits = T.splitOn "," tag
            m = case bits of
              (_ : mm : _) | Just nd <- readMeter mm -> Just nd
              _ -> meter
            db = take 1 bits == ["db"]
         in AnnRow x db m : go m ls
      _ -> go meter ls
    readMeter mm = case T.splitOn "/" mm of
      [n, d] | Just a <- readI n, Just b <- readI d -> Just (a, b)
      _ -> Nothing

readD :: Text -> Maybe Double
readD t = case reads (T.unpack (T.strip t)) of
  [(x, "")] -> Just x
  _ -> Nothing

readI :: Text -> Maybe Int
readI t = case reads (T.unpack (T.strip t)) of
  [(x, "")] -> Just x
  _ -> Nothing

-- | Whole-note position of every annotated beat (see module header).
scoreBeatPositions :: Text -> [WholeNotes]
scoreBeatPositions src =
  let rows = parseAnnotations src
      dbs = [(arTime r, m) | r <- rows, arDownbeat r
                           , Just m <- [arMeter r]]
      factors =
        [ (fromIntegral n / fromIntegral d) / (t2 - t1)
        | ((t1, (n, d)), (t2, _)) <- zip dbs (drop 1 dbs)
        , t2 > t1 ]
      fallback =
        let ts = map arTime rows
            gaps = sort [b - a | (a, b) <- zip ts (drop 1 ts), b > a]
         in if null gaps then [0.5]
            else [0.25 / (gaps !! (length gaps `div` 2))]
      fs = sort (if null factors then fallback else factors)
      wnPerS = fs !! (length fs `div` 2)
   in [WholeNotes (toRational (arTime r * wnPerS)) | r <- rows]

-- | Real seconds of every annotated beat of a performance.
perfBeatTimes :: Text -> [Double]
perfBeatTimes = map arTime . parseAnnotations

-- | Inverse inter-beat intervals; degenerate intervals drop the pair
-- downstream (mirrors the Python NaN convention via Maybe).
localTempo :: [Double] -> [Maybe Double]
localTempo ts =
  [ if ibi > 1e-4 then Just (1 / ibi) else Nothing
  | (a, b) <- zip ts (drop 1 ts), let ibi = b - a ]

pearson :: [Double] -> [Double] -> Maybe Double
pearson xs ys
  | n < 3 = Nothing
  | den <= 0 = Nothing
  | otherwise = Just (num / den)
  where
    n = length xs
    mx = sum xs / fromIntegral n
    my = sum ys / fromIntegral n
    num = sum (zipWith (\x y -> (x - mx) * (y - my)) xs ys)
    den = sqrt (sum [(x - mx) ^ (2 :: Int) | x <- xs])
        * sqrt (sum [(y - my) ^ (2 :: Int) | y <- ys])

-- ---------------------------------------------------------------------

-- | IO half: one source dir's score-annotation text plus every
-- performance's (name, annotation text). Nothing when the dir has no
-- score grid. Each dir carries its OWN grid — ASAP's notated beats and
-- the aligner's uniform quarters must never mix.
loadDirAnnotations :: FilePath -> IO (Maybe (Text, [(String, Text)]))
loadDirAnnotations dir = do
  ok <- doesDirectoryExist dir
  if not ok then pure Nothing else do
    let sann = dir </> "midi_score_annotations.txt"
    hasS <- doesFileExist sann
    if not hasS then pure Nothing else do
      scoreT <- TIO.readFile sann
      files <- sort <$> listDirectory dir
      let anns = [ f | f <- files
                 , "_annotations.txt" `T.isSuffixOf` T.pack f
                 , not ("midi_score" `T.isPrefixOf` T.pack f) ]
      perfs <- mapM (\f -> do
                       t <- TIO.readFile (dir </> f)
                       let suffix = "_annotations.txt" :: String
                           name = take (length f - length suffix)
                                    (takeFileName f)
                       pure (name, t))
                    anns
      pure (Just (scoreT, perfs))

-- | Pure half: score a tempo map against one dir's annotations —
-- everything after the file reads, so a corpus can fan out in parMap.
scorePerformances
  :: [(WholeNotes, Bpm)] -> (Text, [(String, Text)]) -> [PerfR]
scorePerformances tmap (scoreT, perfs) =
  let positions = scoreBeatPositions scoreT
      ours = localTempo
        [ s | w <- positions, let Seconds s = secondsAt tmap w ]
   in [ PerfR name r (length pairs)
      | (name, t) <- perfs
      , let human = localTempo (perfBeatTimes t)
            n = min (length ours) (length human)
            pairs = [ (o, h)
                    | (Just o, Just h)
                        <- zip (take n ours) (take n human) ]
            r = maybe (0 / 0) id
                  (pearson (map fst pairs) (map snd pairs)) ]
