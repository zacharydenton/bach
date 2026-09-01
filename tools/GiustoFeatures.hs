-- | Feature dump for the tempo-giusto calibration (tools/giusto_fit.py).
--
-- One line per corpus piece:
--   piece fast vfast broad num den predictedBpm
-- where the features are exactly those OTB.TempoGiusto consumes and
-- predictedBpm is the SHIPPED formula's output, so the fitter and the
-- verifier read the same notation the compiler does.
--
-- Run from the repo root:  stack runghc tools/GiustoFeatures.hs
--
-- License: GPL-2.0-or-later.
import Control.Monad (forM_)
import Data.List (isSuffixOf, sort)
import qualified Data.Text.IO as TIO
import OTB.Kern.Parser (parseKern)
import OTB.Score
import OTB.TempoGiusto (tempoGiusto)
import OTB.Units (Bpm (..))
import System.Directory (listDirectory)

main :: IO ()
main = do
  fs <- sort . filter (".krn" `isSuffixOf`) <$> listDirectory dir
  forM_ fs $ \f -> do
    src <- TIO.readFile (dir <> "/" <> f)
    case parseKern (Bpm 72) src of
      Left _ -> pure ()
      Right s -> do
        let notes = [n | v <- scVoices s, n <- vNotes v, snDur n > 0]
            total = fromIntegral (length notes) :: Double
            frac p =
              if total == 0 then 0
              else fromIntegral (length (filter p notes)) / total
            (num, den) = case scMeter s of
              ((_, m) : _) -> m
              [] -> (4, 4)
            Bpm g = tempoGiusto s
        putStrLn $ unwords
          [ take (length f - 4) f
          , show (frac ((<= 1 / 16) . snDur))
          , show (frac ((<= 1 / 32) . snDur))
          , show (frac ((>= 1 / 2) . snDur))
          , show num, show den, show g ]
  where
    dir = "corpus/bach-wtc/kern"
