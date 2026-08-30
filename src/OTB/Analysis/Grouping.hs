-- | Hierarchical grouping: the phrase tree the arches nest over.
--
-- Todd (1992) is properly recursive — one arch per node of the grouping
-- hierarchy, summed — where the earlier implementation ran a fixed two
-- levels (piece + bar group). Here the interior structure comes from
-- the music: per-lane boundary strengths (Cambouropoulos's LBDM
-- features, already computed for phrasing) are aggregated across
-- voices, and the piece is split recursively at the strongest interior
-- boundary (GTTM's grouping intuition, computationally in the spirit of
-- Hamanaka's implementations). Each node contributes an arch whose
-- depth decays with tree depth.
--
-- License: GPL-2.0-or-later.
module OTB.Analysis.Grouping
  ( groupSpans
  ) where

import Data.List (maximumBy, sortOn)
import Data.Ord (comparing)
import OTB.Units (WholeNotes (..))

-- | Recursive segmentation: (aggregated boundary position, strength)
-- pairs in, (start, end, depth) spans out — depth 1 children of the
-- whole span, and so on. A span shorter than @minSpan@ or deeper than
-- @maxDepth@ stops splitting; the root span itself is NOT returned
-- (the caller owns the piece-level arch).
groupSpans
  :: Int -- ^ max depth
  -> WholeNotes -- ^ minimum span worth splitting
  -> [(WholeNotes, Double)] -- ^ boundary candidates (position, strength)
  -> WholeNotes -> WholeNotes -- ^ piece start, end
  -> [(WholeNotes, WholeNotes, Int)]
groupSpans maxDepth minSpan bounds start end = go 1 start end
  where
    merged = mergeNear (sortOn fst bounds)
    -- boundaries within a 32nd of each other are one boundary
    mergeNear ((t1, s1) : (t2, s2) : more)
      | t2 - t1 <= 1 / 32 = mergeNear ((t1, s1 + s2) : more)
      | otherwise = (t1, s1) : mergeNear ((t2, s2) : more)
    mergeNear xs = xs

    go depth a b
      | depth > maxDepth || b - a < minSpan * 2 = []
      | otherwise =
          -- interior candidates only, kept away from the edges so a
          -- split never produces a sliver
          case [ (t, s) | (t, s) <- merged
               , t > a + minSpan, t < b - minSpan ] of
            [] -> []
            interior ->
              let (cut, _) = maximumBy (comparing snd) interior
               in (a, cut, depth) : (cut, b, depth)
                    : go (depth + 1) a cut <> go (depth + 1) cut b
