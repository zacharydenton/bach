-- | PerformanceIR as JSON — the seam.
--
-- Everything a renderer needs and nothing it must derive: events carry
-- absolute wall-clock seconds (the tempo map already integrated) *and*
-- score positions in whole notes, so a live player can follow either.
-- Consumers: tools/audition.py (surgepy offline render), and one day a
-- Rust live player beside Vass.
--
-- Hand-rolled emission: the payload is purely numeric, so JSON here is
-- string concatenation with no escaping problem — no aeson needed.
--
-- License: GPL-2.0-or-later.
module OTB.Emit.Json
  ( renderJson
  ) where

import Data.List (intercalate)
import OTB.Player (PerfNote (..), Performance (..))
import OTB.Units (Bpm (..), Seconds (..), WholeNotes (..), secondsAt)

renderJson :: String -> Performance -> String
renderJson piece (Performance tmap tracks) =
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
            ]
    obj kvs = "{" <> intercalate "," [show k <> ":" <> v | (k, v) <- kvs] <> "}"
    arr xs = "[" <> intercalate "," xs <> "]"
    num :: Double -> String
    num = show
