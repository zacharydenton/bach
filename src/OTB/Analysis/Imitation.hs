-- | Cross-voice imitation: who has the floor.
--
-- A fugue's episodes are not repetition within a line — they are
-- conversation between lines: a motif stated in one voice is answered
-- in another a beat or a bar later, at another pitch level, sometimes
-- overlapping (stretto). Harnoncourt's /Musik als Klangrede/ makes this
-- the central claim: baroque music is speech, and polyphony is
-- dialogue. Czerny's edition of the WTC marks the practice concretely —
-- emphasis at every entry, not only the subject's. This module finds
-- the dialogue so the Player can perform it.
--
-- The trap is coincidence: baroque lines are saturated with scale runs
-- and arpeggio figures, so a naive matcher hears "imitation"
-- everywhere. The filter is DISTINCTIVENESS: a match only counts when
-- the motif's contour is rare in this piece (imitation is informative
-- exactly in proportion to how unlikely the coincidence is), has at
-- least one change of direction, and answers within two bars.
--
-- Products:
--
--   * 'imTakes' — floor-takings: (onset, voice, span) where a voice
--     picks up a motif another voice just stated. The dialogue rule
--     hands lead and presence to that voice for that span.
--   * 'imSpans' — merged exchange passages (the episodes).
--
-- License: GPL-2.0-or-later.
module OTB.Analysis.Imitation
  ( Imitation (..)
  , findImitation
  ) where

import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import OTB.Score (Score (..), ScoreNote (..), Voice (..))
import OTB.Units (WholeNotes (..))

data Imitation = Imitation
  { imTakes :: [(WholeNotes, Int, WholeNotes)]
    -- ^ (onset, voice index, motif span): this voice takes the floor
  , imSpans :: [(WholeNotes, WholeNotes)]
    -- ^ merged exchange passages
  }

motifLen :: Int
motifLen = 5 -- intervals; six notes

maxFreq :: Int
maxFreq = 6 -- contours commoner than this are figuration, not speech

findImitation :: Score -> Imitation
findImitation s = Imitation
  { imTakes = takes
  , imSpans = mergeSpans (sortOn fst
      [ (t1, t2 + halfBar) | (t2, _, _, t1) <- events ])
  }
  where
    voices' = zip [0 ..] [dedupe (sortOn snOnset (vNotes v))
                         | v <- scVoices s]
    dedupe (a : b : more)
      | snOnset a == snOnset b = dedupe (a : more)
      | otherwise = a : dedupe (b : more)
    dedupe xs = xs

    barLen = case scMeter s of
      ((_, (n, d)) : _) -> WholeNotes (fromIntegral n / fromIntegral d)
      [] -> 1
    window = 2 * barLen
    halfBar = barLen / 2

    grams ns i = zipWith (\a b -> clamp (snPitch b - snPitch a))
                   (drop i ns) (drop (i + 1) ns)
    clamp x = max (-12) (min 12 x)
    contour ns i = take motifLen (grams ns i)

    -- global contour frequency: the distinctiveness filter
    freq = Map.fromListWith (+)
      [ (contour ns i, 1 :: Int)
      | (_, ns) <- voices', i <- [0 .. length ns - motifLen - 1] ]
    distinctive c =
      length c == motifLen
        && Map.findWithDefault 0 c freq <= maxFreq
        && directionChanges c >= 1
    directionChanges c =
      length [ () | (a, b) <- zip c (drop 1 c), a * b < 0 ]

    -- statement instances worth answering
    statements =
      [ (v, i, snOnset (ns !! i), c)
      | (v, ns) <- voices', i <- [0 .. length ns - motifLen - 1]
      , let c = contour ns i, distinctive c ]

    -- answers: same contour within diatonic slack, within the window,
    -- in ANOTHER voice, rhythm preserved within tolerance
    events =
      dedupeTakes
        [ (t2, v2, motifSpan ns2 j, t1)
        | (v1, i1, t1, c1) <- statements
        , (v2, ns2) <- voices', v2 /= v1
        , j <- candidateIx ns2 t1
        , let t2 = snOnset (ns2 !! j)
        , t2 > t1, t2 - t1 <= window
        , let c2 = contour ns2 j
        , length c2 == motifLen
        , matches (voiceLine v1) i1 ns2 j c1 c2 ]
      where
        voiceLine v = head [ns | (v', ns) <- voices', v' == v]
    candidateIx ns t1 =
      [ j | (j, n) <- zip [0 ..] ns
      , snOnset n > t1, snOnset n - t1 <= window ]
    matches ns1 i ns2 j c1 c2 =
      let devs = [abs (a - b) | (a, b) <- zip c1 c2]
          contourOK = and [signum a == signum b || a == b
                          | (a, b) <- zip c1 c2]
          d1 = durs ns1 i
          d2 = durs ns2 j
          rOK = and [ y > 0 && abs (x / y - 1) < 0.25
                    | (x, y) <- zip d1 d2 ]
       in and [d <= 1 | d <- devs] && contourOK && rOK
    durs ns i =
      [ fromRational r :: Double
      | n <- take (motifLen + 1) (drop i ns)
      , let WholeNotes r = snDur n ]
    motifSpan ns j =
      let seg = take (motifLen + 1) (drop j ns)
       in sum (map snDur seg)

    -- one take per (voice, onset): the earliest statement claims it
    dedupeTakes evs =
      Map.elems (Map.fromListWith earlier
        [ ((v2, t2), e) | e@(t2, v2, _, _) <- evs ])
      where
        earlier a@(_, _, _, s1) b@(_, _, _, s2) =
          if s1 <= s2 then a else b

    takes = sortOn (\(t, _, _) -> t)
      [ (t2, v2, sp) | (t2, v2, sp, _) <- events ]

    mergeSpans ((a1, b1) : (a2, b2) : more)
      | a2 <= b1 + halfBar = mergeSpans ((a1, max b1 b2) : more)
      | otherwise = (a1, b1) : mergeSpans ((a2, b2) : more)
    mergeSpans xs = xs
