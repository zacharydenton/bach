-- | otb — the One-Take Bach interpretation compiler, M0.
--
--   otb compile corpus/bach-wtc/kern/wtc1p01.krn -o out.mid [--gate 0.8]
--
-- License: GPL-2.0-or-later.
module Main (main) where

import Data.Text.IO qualified as TIO
import OTB.Kern.Parser (parseKern)
import OTB.Player (perform)
import OTB.Emit.Midi (writeSmf)
import OTB.Score (Score (..), Voice (..))
import OTB.Units (Bpm (..))
import Options.Applicative
import System.Exit (die)

data Opts = Opts
  { optInput :: FilePath
  , optOutput :: FilePath
  , optGate :: Double
  , optTempo :: Double
  }

opts :: Parser Opts
opts =
  Opts
    <$> argument str (metavar "SCORE.krn")
    <*> strOption (long "output" <> short 'o' <> metavar "OUT.mid" <> value "out.mid")
    <*> option auto (long "gate" <> metavar "FRACTION" <> value 0.8
          <> help "sounding fraction of notated duration (détaché default)")
    <*> option auto (long "tempo" <> metavar "BPM" <> value 72
          <> help "fallback tempo when the score has no *MM")

main :: IO ()
main = do
  o <- execParser (info (opts <**> helper)
        (fullDesc <> progDesc "Compile a **kern score to performed MIDI"))
  src <- TIO.readFile (optInput o)
  score <- either (die . ("parse: " <>)) pure (parseKern (Bpm (optTempo o)) src)
  let p = perform (toRational (optGate o)) score
  writeSmf (optOutput o) p
  let Bpm bpm = scTempo score
  putStrLn $
    "voices " <> show (length (scVoices score))
      <> " | notes " <> show (sum (map (length . vNotes) (scVoices score)))
      <> " | tempo " <> show bpm
      <> (if scTieLeftovers score > 0
            then " | WARN tie-leftovers " <> show (scTieLeftovers score)
            else "")
      <> " | -> " <> optOutput o
