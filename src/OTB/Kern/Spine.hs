-- | The spine-path state machine — the only real complexity in kern.
--
-- A kern column may split (@*^@), merge (@*v@ groups) and terminate (@*-@).
-- Each *top-level* spine is a contrapuntal voice; split-out sub-spines stay
-- inside their parent voice (WTC encodes e.g. a held note under moving
-- sixteenths this way from bar 1 of Book I's first prelude).
--
-- Every live path carries its own clock and a *lane* number within its
-- voice: a split child takes the lowest lane not live in that voice, so a
-- note can say which sub-spine it came from and the Player need not guess.
-- A merge must join paths of one voice. Their clocks should agree; the
-- corpus has a few merges where they do not (a sub-spine short by a rest),
-- so the maximum is kept and the drift is *counted*, surfaced by the parser
-- as a diagnostic rather than absorbed silently. @*+@ is rejected.
--
-- License: GPL-2.0-or-later.
module OTB.Kern.Spine
  ( Path (..)
  , SpineState
  , initState
  , applyInterps
  , dataPaths
  ) where

import Data.List (nub)
import OTB.Kern.Token (Interp (..))
import OTB.Units (WholeNotes)

data Path = Path
  { pVoice :: !Int -- ^ index of the top-level spine this path belongs to
  , pKern :: !Bool -- ^ False for **dynam etc: fields consumed, never played
  , pClock :: !WholeNotes -- ^ this path's own elapsed time
  , pLane :: !Int -- ^ sub-spine slot within the voice; 0 for the trunk
  }
  deriving (Show)

type SpineState = [Path]

-- | From the @**kern@ exclusive-interpretation record.
initState :: [Interp] -> SpineState
initState = zipWith mk [0 ..]
  where
    mk i IKernStart = Path i True 0 0
    mk i _ = Path i False 0 0

-- | Apply a full record of spine-manipulator interpretations positionally.
-- Returns Nothing when the record contains no manipulators (plain tandem
-- records like @*M4/4@ line up one-to-one and change nothing structural);
-- otherwise the new state and the number of merges whose clocks disagreed.
applyInterps :: [Interp] -> SpineState -> Either String (Maybe (SpineState, Int))
applyInterps is st
  | length is /= length st =
      Left ("interpretation record arity " <> show (length is)
              <> " /= live spines " <> show (length st))
  | not (any manipulator is) = Right Nothing
  | otherwise = Just <$> go live (zip is st)
  where
    manipulator i = case i of
      ISplit -> True; IMerge -> True; ITerminate -> True; IAdd -> True
      IExchange -> True
      _ -> False
    live = [(pVoice p, pLane p) | p <- st]
    go _ [] = Right ([], 0)
    go used ((ISplit, p) : rest) =
      let l = case [k | k <- [0 ..], (pVoice p, k) `notElem` used] of
            (k : _) -> k
            [] -> 0 -- unreachable: [0 ..] is infinite
       in cons p . cons p {pLane = l} <$> go ((pVoice p, l) : used) rest
    go used ((IMerge, p) : rest) = do
      let (ms, rest') = span ((\case IMerge -> True; _ -> False) . fst) rest
          group = p : map snd ms
          clks = nub (map pClock group)
          drift = if length clks > 1 then 1 else 0
      case nub (map pVoice group) of
        [_] ->
          (\(ps, n) -> (p {pClock = maximum clks} : ps, n + drift))
            <$> go used rest'
        vs -> Left ("*v merges paths of different voices: " <> show vs)
    go used ((ITerminate, _) : rest) = go used rest
    go _ ((IAdd, _) : _) = Left "*+ (spine add) is not supported"
    -- an active exchange swaps two spines' contents; absorbing it would
    -- assign every later note in both spines to the wrong voice — the
    -- worst silent failure a counterpoint compiler can have
    go _ ((IExchange, _) : _) = Left "*x (spine exchange) is not supported"
    go used ((_, p) : rest) = cons p <$> go used rest
    cons p (ps, n) = (p : ps, n)

-- | Pair data fields with their paths (arity must match).
dataPaths :: SpineState -> [a] -> Either String [(Path, a)]
dataPaths st xs
  | length st == length xs = Right (zip st xs)
  | otherwise =
      Left ("data record arity " <> show (length xs)
              <> " /= live spines " <> show (length st))
