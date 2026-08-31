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

import Data.List (maximumBy, sortOn)
import Data.Ord (comparing)
import OTB.Pitch (spDegree)
import OTB.Score
import OTB.Units (WholeNotes)

-- | Count parallel perfect fifths/octaves between every voice pair:
-- successive onset slices where both voices moved in the same direction
-- and the interval stayed the same perfect consonance. When both notes
-- carry spelling, quality comes from letter distance too — a diminished
-- sixth spans seven semitones but is not a fifth; unspelled (generated)
-- scores fall back to semitones alone.
parallelPerfects :: Score -> Int
parallelPerfects (Score _ voices _ _ _ _ _) =
  sum [pairCount a b | (a, bs) <- zip voices (drop 1 (iterate (drop 1) voices)), b <- bs]

pairCount :: Voice -> Voice -> Int
pairCount va vb =
  length (filter parallel (zip slices (drop 1 slices)))
  where
    onsets =
      sortOn id (map snOnset (vNotes va) <> map snOnset (vNotes vb))
    -- Maybe-slices, NOT mapMaybe: dropping silent onsets would make two
    -- fifths separated by a rest in either voice look consecutive. A
    -- Nothing between two sounding slices breaks their adjacency.
    slices =
      map
        (\t -> (,) <$> soundingAt t (vNotes va) <*> soundingAt t (vNotes vb))
        onsets
    -- "same interval" is the CHROMATIC class (spelling may be absent on
    -- one slice but not the other — mixed scores must still pair up);
    -- spelling, where present, is a per-slice quality veto only
    parallel (Just (a1, b1), Just (a2, b2)) =
      let iv1 = ivClass a1 b1
          iv2 = ivClass a2 b2
       in snd iv1 == snd iv2
            && perfect iv1
            && perfect iv2
            && snPitch a1 /= snPitch a2
            && snPitch b1 /= snPitch b2
            && signum (snPitch a2 - snPitch a1)
                 == signum (snPitch b2 - snPitch b1)
    parallel _ = False
    -- (diatonic steps mod 7 | -1 when unspelled, semitones mod 12)
    ivClass x y =
      ( case (snSpell x, snSpell y) of
          (Just sx, Just sy) -> abs (spDegree sx - spDegree sy) `mod` 7
          _ -> -1
      , abs (snPitch x - snPitch y) `mod` 12 )
    perfect (steps, semis) = case steps of
      -1 -> semis == 0 || semis == 7 -- unspelled fallback
      _ -> (steps, semis) == (4, 7) || (steps, semis) == (0, 0)

-- | The note sounding at time t, if any (highest, when a voice's
-- sub-spines make it momentarily polyphonic).
soundingAt :: WholeNotes -> [ScoreNote] -> Maybe ScoreNote
soundingAt t ns =
  case [n | n <- ns, snOnset n <= t, t < snOnset n + snDur n] of
    [] -> Nothing
    ps -> Just (maximumBy (comparing snPitch) ps)
