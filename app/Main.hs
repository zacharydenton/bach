-- | otb — the One-Take Bach interpretation compiler.
--
--   otb corpus/bach-wtc/kern/wtc1p01.krn -o out.mid [--config config/default.toml]
--
-- The piece name for per-piece config overrides is the input basename
-- (wtc1p01.krn -> [piece.wtc1p01]).
--
-- License: GPL-2.0-or-later.
module Main (main) where

import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import OTB.Config
  ( agogicsFor, artParamsFor, dynamicsFor, loadConfig, ornamentsFor
  , phrasingFor, tuningBendRange )
import OTB.Emit.Json (renderJson)
import OTB.Emit.Midi (writeSmf)
import OTB.Interp.Agogics (defaultAgogicParams)
import OTB.Interp.Dynamics (defaultDynParams)
import OTB.Interp.Ornament (defaultOrnamentParams)
import OTB.Interp.Phrasing (defaultPhraseParams)
import OTB.Kern.Parser (parseKern)
import OTB.Player (Interp (..), perform)
import OTB.Score (Score (..), Voice (..))
import OTB.Tuning (TuningTable, equalTable, parseScl, renderScl, werckmeister3)
import OTB.Units (Bpm (..))
import Options.Applicative
import System.Directory (doesFileExist)
import System.Exit (die)
import System.FilePath (takeBaseName)

data Opts = Opts
  { optInput :: FilePath
  , optOutput :: FilePath
  , optConfig :: FilePath
  , optTempo :: Double
  , optTemperament :: String
  , optEmitScl :: Maybe FilePath
  , optEmitJson :: Maybe FilePath
  }

opts :: Parser Opts
opts =
  Opts
    <$> argument str (metavar "SCORE.krn")
    <*> strOption (long "output" <> short 'o' <> metavar "OUT.mid" <> value "out.mid")
    <*> strOption (long "config" <> metavar "RULES.toml" <> value "config/default.toml"
          <> help "interpretation rules + per-piece override log")
    <*> option auto (long "tempo" <> metavar "BPM" <> value 72
          <> help "fallback tempo when the score has no *MM")
    <*> strOption (long "temperament" <> metavar "NAME|FILE.scl"
          <> value "werckmeister3"
          <> help "werckmeister3 (default), equal, or a Scala .scl file")
    <*> optional (strOption (long "emit-scl" <> metavar "OUT.scl"
          <> help "also write the temperament as .scl (for Surge's native microtuning)"))
    <*> optional (strOption (long "emit-json" <> metavar "OUT.json"
          <> help "also write PerformanceIR as JSON (absolute seconds; for surgepy audition and future live players)"))

main :: IO ()
main = do
  o <- execParser (info (opts <**> helper)
        (fullDesc <> progDesc "Compile a **kern score to performed MIDI"))
  src <- TIO.readFile (optInput o)
  score <- either (die . ("parse: " <>)) pure (parseKern (Bpm (optTempo o)) src)
  haveCfg <- doesFileExist (optConfig o)
  cfg <- if haveCfg then loadConfig <$> TIO.readFile (optConfig o)
         else pure (loadConfig "")
  table <- resolveTemperament (optTemperament o)
  let piece = T.pack (takeBaseName (optInput o))
      interp = Interp
        { iArt = artParamsFor cfg piece
        , iAgogics = agogicsFor cfg piece defaultAgogicParams
        , iPhrasing = phrasingFor cfg piece defaultPhraseParams
        , iOrnaments = ornamentsFor cfg piece defaultOrnamentParams
        , iDynamics = dynamicsFor cfg piece defaultDynParams
        , iTuning = table
        , iBendRange = tuningBendRange cfg
        }
  p <- either (die . ("perform: " <>)) pure (perform interp score)
  writeSmf (optOutput o) p
  case optEmitScl o of
    Nothing -> pure ()
    Just sclPath ->
      TIO.writeFile sclPath
        (renderScl (T.pack (optTemperament o)) table)
  case optEmitJson o of
    Nothing -> pure ()
    Just jsonPath -> writeFile jsonPath (renderJson (T.unpack piece) p)
  let Bpm bpm = scTempo score
  putStrLn $
    "voices " <> show (length (scVoices score))
      <> " | notes " <> show (sum (map (length . vNotes) (scVoices score)))
      <> " | tempo " <> show bpm
      <> " | piece " <> T.unpack piece
      <> (if haveCfg then "" else " | WARN no config file, defaults only")
      <> (if scTieLeftovers score > 0
            then " | WARN tie-leftovers " <> show (scTieLeftovers score)
            else "")
      <> " | " <> optTemperament o
      <> " | -> " <> optOutput o

resolveTemperament :: String -> IO TuningTable
resolveTemperament name = case name of
  "werckmeister3" -> pure werckmeister3
  "equal" -> pure equalTable
  path -> do
    src <- TIO.readFile path
    either (die . (("scl " <> path <> ": ") <>)) pure (parseScl src)
