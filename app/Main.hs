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
import OTB.Config (artParamsFor, loadConfig)
import OTB.Emit.Midi (writeSmf)
import OTB.Kern.Parser (parseKern)
import OTB.Player (perform)
import OTB.Score (Score (..), Voice (..))
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

main :: IO ()
main = do
  o <- execParser (info (opts <**> helper)
        (fullDesc <> progDesc "Compile a **kern score to performed MIDI"))
  src <- TIO.readFile (optInput o)
  score <- either (die . ("parse: " <>)) pure (parseKern (Bpm (optTempo o)) src)
  haveCfg <- doesFileExist (optConfig o)
  cfg <- if haveCfg then loadConfig <$> TIO.readFile (optConfig o)
         else pure (loadConfig "")
  let piece = T.pack (takeBaseName (optInput o))
      ap = artParamsFor cfg piece
      p = perform ap score
  writeSmf (optOutput o) p
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
      <> " | -> " <> optOutput o
