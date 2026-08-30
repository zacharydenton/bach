-- | PerformanceIR as JSON — the seam.
--
-- Everything a renderer needs and nothing it must derive: events carry
-- absolute wall-clock seconds (the tempo map already integrated) *and*
-- score positions in whole notes, so a live player can follow either.
-- Consumers: tools/audition.py (surgepy offline render), and one day a
-- Rust live player beside Vass.
--
-- Hand-rolled emission: the numeric payload needs no escaping; the why
-- strings (rules, deltas, citations) go through a minimal escaper. The
-- whys make the IR self-describing — the patchboard's live subtitles
-- and any future reader get the interpretation's reasons with its
-- notes, which is what an executable edition owes its audience.
--
-- License: GPL-2.0-or-later.
module OTB.Emit.Json
  ( renderJson
  ) where

import Data.List (intercalate)
import Data.Map.Strict qualified as Map
import OTB.Explain (Why (..))
import OTB.Player (PerfNote (..), Performance (..))
import OTB.Units (Bpm (..), Seconds (..), WholeNotes (..), secondsAt)

renderJson :: String -> Performance -> String
renderJson piece (Performance tmap tracks whys) =
  obj
    [ ("piece", show piece)
    , ("tempoMap", arr (map tempoJson tmap))
    , ("tracks", arr (map trackJson tracks))
    ]
  where
    tempoJson (WholeNotes w, Bpm b) =
      let Seconds s = secondsAt tmap (WholeNotes w)
       in obj [("wn", num (fromRational w)), ("onS", num s), ("bpm", num b)]
    trackJson notes = arr (map noteJson notes)
    noteJson pn =
      let Seconds onS = secondsAt tmap (pnOnset pn)
          Seconds offS = secondsAt tmap (pnOnset pn + pnDur pn)
          WholeNotes onW = pnOnset pn
          WholeNotes durW = pnDur pn
       in obj
            [ ("onS", num onS)
            , ("durS", num (offS - onS))
            , ("onWn", num (fromRational onW))
            , ("durWn", num (fromRational durW))
            , ("pitch", show (pnPitch pn))
            , ("vel", show (pnVel pn))
            , ("bend", show (pnBend pn))
            , ("ch", show (pnChannel pn))
            , ("whys", arr (map (str . oneWhy) (whysOf pn)))
            ]
    whyMap = Map.fromList whys
    whysOf pn = Map.findWithDefault [] (pnChannel pn, pnIndex pn) whyMap
    oneWhy w = whyRule w <> ": " <> whyDelta w
      <> (if null (whyCite w) then "" else "  [" <> whyCite w <> "]")
    str t = '"' : concatMap esc t <> "\""
      where
        esc '"' = "\\\""
        esc '\\' = "\\\\"
        esc c | c < ' ' = " " -- control chars cannot occur in whys
              | otherwise = [c]
    obj kvs = "{" <> intercalate "," [show k <> ":" <> v | (k, v) <- kvs] <> "}"
    arr xs = "[" <> intercalate "," xs <> "]"
    num :: Double -> String
    num = show
