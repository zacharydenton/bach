-- | Fugue subject detection: know where the theme is, and say so.
--
-- Every performer of a fugue brings out the subject at each entry —
-- Czerny's edition of the WTC prints the practice as explicit dynamics
-- at every entrance — but rule systems don't do it, because it needs
-- theme finding. In a closed corpus of fugues the subject is, by
-- definition, the opening statement of the first voice to enter:
--
--   * the piece is treated as imitative when a second voice enters
--     after the first has stated at least 'minLen' notes alone;
--   * the subject is that solo span (capped at 'maxLen' notes);
--   * entries are transposition-invariant matches of its interval and
--     rhythm profile in any voice — with tolerance for tonal answers
--     (a couple of intervals bent by a semitone at the fifth) and for
--     augmentation (durations uniformly doubled).
--
-- Matches are returned as notated (onset, pitch) identities, the same
-- key 'snSource' preserves through ornament realisation, so the
-- annotator can tag entry notes wherever they end up.
--
-- License: GPL-2.0-or-later.
module OTB.Analysis.Subject
  ( subjectEntries
  ) where

import Data.List (sortOn)
import OTB.Score (Score (..), ScoreNote (..), Voice (..))
import OTB.Units (WholeNotes (..))

minLen, maxLen :: Int
minLen = 4
maxLen = 16

-- | All notated (onset, pitch) pairs belonging to a subject entry,
-- including the original statement. Empty when the piece does not open
-- imitatively (preludes fall out here by construction).
subjectEntries :: Score -> [(WholeNotes, Int)]
subjectEntries s =
  case subject of
    Nothing -> []
    Just subj ->
      concat
        [ map ident entry
        | v <- scVoices s
        , entry <- entriesIn subj (mono v) ]
  where
    mono v = dedupe (sortOn snOnset (vNotes v))
    -- sub-spine chords: keep the first note per onset — the line
    dedupe (a : b : more)
      | snOnset a == snOnset b = dedupe (a : more)
      | otherwise = a : dedupe (b : more)
    dedupe xs = xs
    ident n = snSource n

    firstOnset v = case mono v of
      (n : _) -> Just (snOnset n)
      [] -> Nothing

    subject = do
      let entries' = [(o, v) | v <- scVoices s, Just o <- [firstOnset v]]
      (o1, v1) <- case sortOn fst entries' of
        (x : _) -> Just x
        [] -> Nothing
      o2 <- case sortOn id [o | (o, v) <- entries'
                          , vIndex v /= vIndex v1, o > o1] of
        (x : _) -> Just x
        [] -> Nothing
      let solo = takeWhile ((< o2) . snOnset) (mono v1)
          subj = take maxLen solo
      if length subj >= minLen then Just subj else Nothing

    -- profile: interval steps and duration ratios to the first note
    profile ns =
      ( zipWith (\a b -> snPitch b - snPitch a) ns (drop 1 ns)
      , case ns of
          (n0 : _) | snDur n0 > 0 ->
            [ fromRational (dr / d0r) :: Double
            | n <- ns
            , let WholeNotes dr = snDur n
                  WholeNotes d0r = snDur n0 ]
          _ -> [] )

    entriesIn subj lane = nonOverlap (go lane)
      where
        (sIv, sRh) = profile subj
        k = length subj
        go ns
          | length ns < k = []
          | otherwise =
              let w = take k ns
                  hit = matches (profile w)
               in ([w | hit]) <> go (drop 1 ns)
        matches (iv, rh) =
          let ivDev = [abs (a - b) | (a, b) <- zip sIv iv]
              rhythmOk =
                length rh == length sRh
                  && and [ b > 0 && abs (a / b - 1) < 0.25
                         | (a, b) <- zip sRh rh ]
              -- tonal answers bend an interval or two by a semitone;
              -- anything larger is a different idea
              tonalOk =
                all (<= 1) ivDev
                  && length (filter (== 1) ivDev) <= 2
           in rhythmOk && tonalOk
        -- greedy: once an entry is taken, skip past it
        nonOverlap (e : more) =
          let endO = maximum (map snOnset e)
           in e : nonOverlap
                    (filter (\e' -> minimum (map snOnset e') > endO) more)
        nonOverlap [] = []
