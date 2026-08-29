-- | otb — the One-Take Bach interpretation compiler.
--
--   otb compile SCORE.krn -o out.mid [--emit-json ..] [--emit-scl ..]
--   otb explain SCORE.krn [--bar N | --ch C --note I]
--
-- (bare @otb SCORE.krn ...@ still works: compile is the default command.)
-- The piece name for per-piece config overrides is the input basename
-- (wtc1p01.krn -> [piece.wtc1p01]).
--
-- License: GPL-2.0-or-later.
module Main (main) where

import Control.Monad (when)
import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import OTB.Config
  ( agogicsFor, artParamsFor, dynamicsFor, expressFor, emptyConfig, loadConfig
  , ornamentsFor, phrasingFor, pieceTempo, tuningBendRange )
import OTB.Emit.Json (renderJson)
import OTB.Emit.Midi (writeSmf)
import OTB.Explain (renderWhys)
import OTB.Instrument (hardwareTracks)
import OTB.Interp.Agogics (defaultAgogicParams)
import OTB.Interp.Dynamics (defaultDynParams)
import OTB.Interp.Express (defaultExpressParams)
import OTB.Interp.Ornament (defaultOrnamentParams)
import OTB.Interp.Phrasing (defaultPhraseParams)
import OTB.Kern.Parser (parseKern)
import OTB.Player (Interp (..), PerfNote (..), Performance (..), perform)
import OTB.Score (Score (..), Voice (..))
import OTB.Tuning (TuningTable, equalTable, parseScl, renderScl, werckmeister3)
import OTB.Units (Bpm (..), WholeNotes (..))
import Options.Applicative
import System.Directory (doesFileExist)
import System.Exit (die)
import System.FilePath (takeBaseName)

data Common = Common
  { cInput :: FilePath
  , cConfig :: FilePath
  , cTempo :: Double
  , cTemperament :: String
  }

data Cmd
  = Compile Common FilePath (Maybe FilePath) (Maybe FilePath) String
  | Explain Common (Maybe Int) (Maybe (Int, Int))

common :: Parser Common
common =
  Common
    <$> argument str (metavar "SCORE.krn")
    <*> strOption (long "config" <> metavar "RULES.toml"
          <> value "config/default.toml"
          <> help "interpretation rules + per-piece override log")
    <*> option auto (long "tempo" <> metavar "BPM" <> value 72
          <> help "fallback tempo when the score has no *MM")
    <*> strOption (long "temperament" <> metavar "NAME|FILE.scl"
          <> value "werckmeister3"
          <> help "werckmeister3 (default), equal, or a Scala .scl file")

compileCmd :: Parser Cmd
compileCmd =
  Compile
    <$> common
    <*> strOption (long "output" <> short 'o' <> metavar "OUT.mid"
          <> value "out.mid")
    <*> optional (strOption (long "emit-scl" <> metavar "OUT.scl"
          <> help "also write the temperament as .scl (Surge native tuning)"))
    <*> optional (strOption (long "emit-json" <> metavar "OUT.json"
          <> help "also write PerformanceIR as JSON (the renderer seam)"))
    <*> strOption (long "target" <> metavar "surge|hardware" <> value "surge"
          <> help "hardware remaps lanes onto the rig (A4 x4, Model D, BS2) with capabilities enforced")

explainCmd :: Parser Cmd
explainCmd =
  Explain
    <$> common
    <*> optional (option auto (long "bar" <> metavar "N"
          <> help "explain every note in bar N (1-based)"))
    <*> optional
          ((,) <$> option auto (long "ch" <> metavar "C")
               <*> option auto (long "note" <> metavar "I"))

cmdP :: Parser Cmd
cmdP =
  hsubparser
    ( command "compile"
        (info compileCmd (progDesc "score -> performed MIDI"))
        <> command "explain"
             (info explainCmd
                (progDesc "per-note rule traces with citations"))
    )
    <|> compileCmd -- bare invocation = compile

main :: IO ()
main = do
  c <- execParser (info (cmdP <**> helper)
        (fullDesc <> progDesc "One-Take Bach interpretation compiler"))
  case c of
    Compile com out mscl mjson tgt -> runCompile com out mscl mjson tgt
    Explain com mbar mnote -> runExplain com mbar mnote

load :: Common -> IO (String, Score, Performance, TuningTable, Bool)
load com = do
  when (isNaN (cTempo com) || isInfinite (cTempo com) || cTempo com <= 0)
    (die "--tempo must be a finite number > 0")
  src <- TIO.readFile (cInput com)
  score0 <- either (die . ("parse: " <>)) pure
              (parseKern (Bpm (cTempo com)) src)
  haveCfg <- doesFileExist (cConfig com)
  cfg <- if haveCfg
           then either (die . (("config " <> cConfig com <> ": ") <>)) pure
                  . loadConfig =<< TIO.readFile (cConfig com)
           else pure emptyConfig
  table <- resolveTemperament (cTemperament com)
  let piece = T.pack (takeBaseName (cInput com))
      score = maybe score0 (\bpm -> score0 {scTempo = Bpm bpm})
                (pieceTempo cfg piece)
      interp = Interp
        { iArt = artParamsFor cfg piece
        , iAgogics = agogicsFor cfg piece defaultAgogicParams
        , iPhrasing = phrasingFor cfg piece defaultPhraseParams
        , iOrnaments = ornamentsFor cfg piece defaultOrnamentParams
        , iDynamics = dynamicsFor cfg piece defaultDynParams
        , iExpress = expressFor cfg piece defaultExpressParams
        , iPiece = T.unpack piece
        , iTuning = table
        , iBendRange = tuningBendRange cfg
        }
  p <- either (die . ("perform: " <>)) pure (perform interp score)
  pure (T.unpack piece, score, p, table, haveCfg)

runCompile :: Common -> FilePath -> Maybe FilePath -> Maybe FilePath -> String -> IO ()
runCompile com out mscl mjson tgt = do
  (piece, score, p0, table, haveCfg) <- load com
  (p, hwClips) <- case tgt of
    "hardware" -> either die pure (hardwareTracks p0)
    _ -> pure (p0, 0)
  writeSmf out p
  mapM_ (\sp -> TIO.writeFile sp
           (renderScl (T.pack (cTemperament com)) table)) mscl
  mapM_ (\jp -> writeFile jp (renderJson piece p)) mjson
  let Bpm bpm = scTempo score
  putStrLn $
    "voices " <> show (length (scVoices score))
      <> " | notes " <> show (sum (map (length . vNotes) (scVoices score)))
      <> " | tempo " <> show bpm
      <> " | piece " <> piece
      <> (if haveCfg then "" else " | WARN no config file, defaults only")
      <> (if scTieLeftovers score > 0
            then " | WARN tie-leftovers " <> show (scTieLeftovers score)
            else "")
      <> (if scMergeDrifts score > 0
            then " | WARN merge-drifts " <> show (scMergeDrifts score)
            else "")
      <> (if scGraceDropped score > 0
            then " | WARN grace-notes-dropped " <> show (scGraceDropped score)
            else "")
      <> (if hwClips > 0
            then " | hardware mono-reduction clipped " <> show hwClips
            else "")
      <> " | " <> cTemperament com
      <> " | -> " <> out

runExplain :: Common -> Maybe Int -> Maybe (Int, Int) -> IO ()
runExplain com mbar mnote = do
  (piece, score, p, _, _) <- load com
  let whys = perfWhys p
      notes = sortOn pnOnset (concat (perfTracks p))
      barLen = case scMeter score of
        ((_, (n, d)) : _) -> WholeNotes (fromIntegral n / fromIntegral d)
        [] -> 1
      inBar b n =
        let lo = fromIntegral (b - 1) * barLen
         in lo <= pnOnset n && pnOnset n < lo + barLen
      selected = case (mbar, mnote) of
        (Just b, _) -> filter (inBar b) notes
        (_, Just (ch, i)) ->
          filter (\n -> pnChannel n == ch && pnIndex n == i) notes
        _ -> take 8 notes
  putStrLn ("== " <> piece <> maybe "" ((" bar " <>) . show) mbar <> " ==")
  mapM_ (explainNote whys) selected
  where
    explainNote whys n = do
      let WholeNotes t = pnOnset n
          ws = fromMaybe [] (lookup (pnChannel n, pnIndex n) whys)
      putStrLn ""
      putStrLn $
        "note ch" <> show (pnChannel n) <> " #" <> show (pnIndex n)
          <> "  pitch " <> show (pnPitch n)
          <> "  t=" <> show (fromRational t :: Double) <> "wn"
          <> "  vel " <> show (pnVel n)
          <> "  bend " <> show (pnBend n)
      putStrLn (renderWhys ws)

resolveTemperament :: String -> IO TuningTable
resolveTemperament name = case name of
  "werckmeister3" -> pure werckmeister3
  "equal" -> pure equalTable
  path -> do
    src <- TIO.readFile path
    either (die . (("scl " <> path <> ": ") <>)) pure (parseScl src)
