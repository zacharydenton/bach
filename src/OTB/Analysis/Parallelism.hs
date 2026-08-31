-- | Sequences and repetition: the structure the fugue's episodes run on.
--
-- A melodic sequence — a figure restated immediately at a consistent
-- transposition — is the motor of Bach's episodes and the most
-- Bach-specific structure a tempo model can know about. The GBM probe
-- (structure_fit --gbm, 2026-08-31) showed human middle-tempo carries
-- signal our features miss; repetition structure is the analysis
-- neither the rules nor that model could see.
--
-- Three products, per piece:
--
--   * 'sqSpans' — passages under sequential repetition (a figure of
--     m notes restated >= 2 further times at a consistent step, rhythm
--     preserved within tolerance, intervals within tonal-answer slack);
--   * 'sqSeams' — the seams between iterations, where a player can
--     articulate the sequence's gait;
--   * 'sqNovelty' — per-onset first-appearance of the local interval
--     contour: 1 where material is new, 0 where it is recall. Humans
--     may push through the familiar and broaden for the new; that is
--     for the fitter to judge.
--
-- Detection is per voice on the deduplicated line (as in
-- OTB.Analysis.Subject), then unioned across voices.
--
-- License: GPL-2.0-or-later.
module OTB.Analysis.Parallelism
  ( Sequences (..)
  , findSequences
  ) where

import Data.List (sort, sortOn)
import qualified Data.Set as Set
import OTB.Score (Score (..), ScoreNote (..), Voice (..))
import OTB.Units (WholeNotes (..))

data Sequences = Sequences
  { sqSpans :: [(WholeNotes, WholeNotes)]
  , sqSeams :: [WholeNotes]
  , sqNovelty :: [(WholeNotes, Double)]
  }

mergeSpans :: [(WholeNotes, WholeNotes)] -> [(WholeNotes, WholeNotes)]
mergeSpans ((a1, b1) : (a2, b2) : more)
  | a2 <= b1 = mergeSpans ((a1, max b1 b2) : more)
  | otherwise = (a1, b1) : mergeSpans ((a2, b2) : more)
mergeSpans xs = xs

findSequences :: Score -> Sequences
findSequences s = Sequences
  { sqSpans = mergeSpans (sortOn fst (concatMap spansIn lines'))
  , sqSeams = sort (concatMap seamsIn lines')
  , sqNovelty = novelty
  }
  where
    lines' = [dedupe (sortOn snOnset (vNotes v)) | v <- scVoices s]
    dedupe (a : b : more)
      | snOnset a == snOnset b = dedupe (a : more)
      | otherwise = a : dedupe (b : more)
    dedupe xs = xs

    -- ---- sequences: figure of m notes restated at i+m, i+2m, ... ----
    chains ns =
      [ (i, m, k)
      | m <- [3 .. 8]
      , i <- [0 .. length ns - 2 * m - 1]
      , let k = chainLen ns i m
      -- short figures need three statements to mean anything; a longer
      -- figure restated once is already a sequence
      , if m >= 5 then k >= 2 else k >= 3 ]
    chainLen ns i m = go 1 Nothing
      where
        go k step0
          | (i + (k + 1) * m) <= length ns
          , let a = slice ns (i + (k - 1) * m) m
                b = slice ns (i + k * m) m
          , iterates a b
          , Just st <- stepOf a b
          -- the transposition must be CONSISTENT across the chain:
          -- +3 then +6 is two coincidences, not one sequence (diatonic
          -- steps may flex a semitone)
          , maybe True (\s0 -> abs (st - s0) <= 1) step0 =
              go (k + 1) (Just (maybe st id step0))
          | otherwise = k
        stepOf a b = case (a, b) of
          (x : _, y : _) -> Just (snPitch y - snPitch x)
          _ -> Nothing
    slice ns i m = take (m + 1) (drop i ns) -- m intervals need m+1 notes
    iterates a b
      | length a < 2 || length b < length a = False
      | otherwise =
          let iv xs = zipWith (\x y -> snPitch y - snPitch x) xs (drop 1 xs)
              devs = [abs (x - y) | (x, y) <- zip (iv a) (iv b)]
              contourOK = and
                [ signum x == signum y || x == y
                | (x, y) <- zip (iv a) (iv b) ]
              -- rhythm means ATTACK rhythm: inter-onset intervals,
              -- which see rests and spacing where note lengths do not
              iois xs = zipWith
                (\x y -> let WholeNotes d = snOnset y - snOnset x
                          in fromRational d :: Double)
                xs (drop 1 xs)
              -- symmetric tolerance: max/min, not abs(ratio - 1),
              -- so statement and answer order cannot change the verdict
              rOK = and
                [ x > 0 && y > 0 && max x y / min x y < 1.25
                | (x, y) <- zip (iois a) (iois b) ]
           -- diatonic transposition bends interval QUALITY freely (a
           -- major second becomes minor): every interval may deviate a
           -- semitone, but the contour must hold and rhythm persist
           in and [d <= 1 | d <- devs]
                && contourOK
                && rOK
    -- keep maximal chains only: greedy by coverage, longest figures first
    best ns =
      go (sortOn (\(i, m, k) -> (negate (m * k), i)) (chains ns)) []
      where
        go [] acc = acc
        go ((i, m, k) : more) acc
          | any (\(i', m', k') -> i < i' + (k' + 1) * m'
                                    && i' < i + (k + 1) * m) acc =
              go more acc
          | otherwise = go more ((i, m, k) : acc)
    spansIn ns =
      [ (snOnset (ns !! i), noteEnd (ns !! lastIx))
      | (i, m, k) <- best ns
      , let lastIx = min (length ns - 1) (i + k * m) ]
    seamsIn ns =
      [ snOnset (ns !! (i + j * m))
      | (i, m, k) <- best ns
      , j <- [1 .. k - 1]
      , i + j * m < length ns ]
    noteEnd n = snOnset n + snDur n

    -- ---- novelty: first appearance of the local 4-interval contour ----
    novelty = go Set.empty events
      where
        events = sortOn (\(o, _) -> o)
          [ (snOnset (ns !! i), gram ns i)
          | ns <- lines', i <- [0 .. length ns - 5] ]
        gram ns i =
          [ clamp (snPitch (ns !! (i + j + 1)) - snPitch (ns !! (i + j)))
          | j <- [0 .. 3] ]
        clamp x = max (-12) (min 12 x)
        go _ [] = []
        go seen ((o, g) : more)
          | g `Set.member` seen = (o, 0) : go seen more
          | otherwise = (o, 1) : go (Set.insert g seen) more
