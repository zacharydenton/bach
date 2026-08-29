-- | Counterpoint validators — the mezzo idea, inverted.
--
-- Mezzo makes illegal parallel fifths fail to compile; this project
-- interprets an already-correct corpus, so the rules point the other way:
-- **Bach doesn't write parallel fifths — the parser might.** A high count
-- here means the spine machine or tie resolution mangled voice-leading.
-- Low single digits per fugue are musicology (hidden/allowed cases, voice
-- crossings, our crude sounding-pitch sampling), not bugs.
--
-- License: GPL-2.0-or-later.
module OTB.Analysis.Counterpoint
  ( parallelPerfects
  ) where

import Data.List (sortOn)
import Data.Maybe (mapMaybe)
import OTB.Score
import OTB.Units (WholeNotes)

-- | Count parallel perfect fifths/octaves between every voice pair:
-- successive onset slices where both voices moved in the same direction
-- and the interval class stayed a perfect consonance (0 or 7 mod 12).
parallelPerfects :: Score -> Int
parallelPerfects (Score _ voices _ _ _) =
  sum [pairCount a b | (a, bs) <- zip voices (drop 1 (iterate (drop 1) voices)), b <- bs]

pairCount :: Voice -> Voice -> Int
pairCount va vb =
  length (filter parallel (zip slices (drop 1 slices)))
  where
    onsets =
      sortOn id (map snOnset (vNotes va) <> map snOnset (vNotes vb))
    slices =
      mapMaybe
        (\t -> (,) <$> soundingAt t (vNotes va) <*> soundingAt t (vNotes vb))
        onsets
    parallel ((a1, b1), (a2, b2)) =
      let iv1 = abs (a1 - b1) `mod` 12
          iv2 = abs (a2 - b2) `mod` 12
       in iv1 == iv2
            && (iv1 == 0 || iv1 == 7)
            && a1 /= a2
            && b1 /= b2
            && signum (a2 - a1) == signum (b2 - b1)

-- | The pitch sounding at time t, if any (highest, when a voice's
-- sub-spines make it momentarily polyphonic).
soundingAt :: WholeNotes -> [ScoreNote] -> Maybe Int
soundingAt t ns =
  case [snPitch n | n <- ns, snOnset n <= t, t < snOnset n + snDur n] of
    [] -> Nothing
    ps -> Just (maximum ps)
