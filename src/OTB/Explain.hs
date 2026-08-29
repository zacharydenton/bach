-- | Provenance: every interpretive decision carries its reasons.
--
-- Annotators emit 'Why' values beside their attributes; the interpreter
-- and assembly thread them through to the Performance, where
-- @otb explain@ renders per-note rule traces. Laziness makes this free
-- until asked for: the [Why] lists are built but never forced unless the
-- explain path demands them.
--
-- License: GPL-2.0-or-later.
module OTB.Explain
  ( Why (..)
  , why
  , renderWhys
  ) where

import Data.List (intercalate)

data Why = Why
  { whyRule :: !String -- ^ short rule name, e.g. \"dissonance\"
  , whyDelta :: !String -- ^ what it did, with numbers
  , whyCite :: !String -- ^ the literature behind it
  }
  deriving (Show, Eq)

why :: String -> String -> String -> Why
why = Why

renderWhys :: [Why] -> String
renderWhys ws =
  intercalate "\n"
    [ "  " <> pad (whyRule w) <> whyDelta w
        <> (if null (whyCite w) then "" else "   [" <> whyCite w <> "]")
    | w <- ws
    ]
  where
    pad s = s <> replicate (max 1 (14 - length s)) ' '
