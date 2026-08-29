-- | The spine-path state machine — the only real complexity in kern.
--
-- A kern column may split (@*^@), merge (@*v@ groups) and terminate (@*-@).
-- Each *top-level* spine is a contrapuntal voice; split-out sub-spines stay
-- inside their parent voice (WTC encodes e.g. a held note under moving
-- sixteenths this way from bar 1 of Book I's first prelude).
--
-- Every live path carries its own clock. A split child inherits the parent
-- clock; a merge keeps the maximum (they should agree — a mismatch is a
-- parse bug surfaced by the M1 corpus sweep, not silently absorbed).
--
-- License: GPL-2.0-or-later.
module OTB.Kern.Spine
  ( Path (..)
  , SpineState
  , initState
  , applyInterps
  , dataPaths
  ) where

import OTB.Kern.Token (Interp (..))
import OTB.Units (WholeNotes)

data Path = Path
  { pVoice :: !Int -- ^ index of the top-level spine this path belongs to
  , pKern :: !Bool -- ^ False for **dynam etc: fields consumed, never played
  , pClock :: !WholeNotes -- ^ this path's own elapsed time
  }
  deriving (Show)

type SpineState = [Path]

-- | From the @**kern@ exclusive-interpretation record.
initState :: [Interp] -> SpineState
initState = zipWith mk [0 ..]
  where
    mk i IKernStart = Path i True 0
    mk i _ = Path i False 0

-- | Apply a full record of spine-manipulator interpretations positionally.
-- Returns Nothing when the record contains no manipulators (plain tandem
-- records like @*M4/4@ line up one-to-one and change nothing structural).
applyInterps :: [Interp] -> SpineState -> Either String (Maybe SpineState)
applyInterps is st
  | length is /= length st =
      Left ("interpretation record arity " <> show (length is)
              <> " /= live spines " <> show (length st))
  | not (any manipulator is) = Right Nothing
  | otherwise = Right (Just (go (zip is st)))
  where
    manipulator i = case i of
      ISplit -> True; IMerge -> True; ITerminate -> True; IAdd -> True
      _ -> False
    go [] = []
    go ((ISplit, p) : rest) = p : p : go rest
    go ((IMerge, p) : rest) =
      let (ms, rest') = span ((\case IMerge -> True; _ -> False) . fst) rest
          clk = maximum (pClock p : map (pClock . snd) ms)
       in p {pClock = clk} : go rest'
    go ((ITerminate, _) : rest) = go rest
    go ((IAdd, p) : rest) = p : Path (pVoice p) False (pClock p) : go rest
    go ((_, p) : rest) = p : go rest

-- | Pair data fields with their paths (arity must match).
dataPaths :: SpineState -> [a] -> Either String [(Path, a)]
dataPaths st xs
  | length st == length xs = Right (zip st xs)
  | otherwise =
      Left ("data record arity " <> show (length xs)
              <> " /= live spines " <> show (length st))
