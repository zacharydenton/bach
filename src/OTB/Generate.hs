-- | The generative seed: pole two of the Vault doc joins the algebra.
--
-- Ground basses (folia, romanesca, Pachelbel) are 'Music' values;
-- variations are Euterpea's own combinators ('retro', 'invert',
-- 'transpose', division figures) applied to the bass; the result is
-- converted to an ordinary 'Score' and pushed through the same
-- annotate\/interpret Player as the corpus — so the patchboard and
-- audition.py play a generated ground unchanged. Deliberately small: a
-- proof that both poles share one algebra, not a composition suite.
--
-- License: GPL-2.0-or-later.
module OTB.Generate
  ( groundNames
  , generateScore
  ) where

import EuterpeaLite.Music
  ( Control (..)
  , Dur
  , Music (..)
  , Pitch
  , Primitive (..)
  , absPitch
  , invert
  , line
  , note
  , pitch
  , retro
  , transpose
  )
import OTB.Kern.Token (Mark (..))
import OTB.Score (Score (..), ScoreNote (..), Voice (..), scoreNote)
import OTB.Units (Bpm, WholeNotes (..))

-- | The traditional bass formulas. Durations are Euterpea 'Dur'
-- (whole notes); one note per bar of the given meter.
grounds :: [(String, (Music Pitch, (Int, Int)))]
grounds =
  [ ("folia", (bassLine (3 % 4) foliaAbs, (3, 4)))
  , ("romanesca", (bassLine (3 % 4) romanescaAbs, (3, 4)))
  , ("pachelbel", (bassLine (2 % 4) pachelbelAbs, (4, 4)))
  ]
  where
    n % d = fromIntegral n / fromIntegral (d :: Int) :: Dur
    -- later Folia, d minor, 16 bars
    foliaAbs =
      [50, 45, 50, 48, 53, 48, 50, 45, 50, 45, 50, 48, 53, 48, 45, 50]
    -- romanesca, g minor, 8 bars (III VII i V, III VII i-V i)
    romanescaAbs = [46, 41, 43, 38, 39, 46, 43, 43]
    -- Pachelbel's canon bass, D major, 8 half notes
    pachelbelAbs = [50, 45, 47, 42, 43, 38, 43, 45]
    bassLine d ps = line [note d (pitch p) | p <- ps]

groundNames :: [String]
groundNames = map fst grounds

-- | Apply a figure to every note of a line, leaving structure alone.
onNotes :: (Dur -> Pitch -> Music Pitch) -> Music Pitch -> Music Pitch
onNotes f m = case m of
  Prim (Note d p) -> f d p
  Prim r@(Rest _) -> Prim r
  a :+: b -> onNotes f a :+: onNotes f b
  a :=: b -> onNotes f a :=: onNotes f b
  Modify c a -> Modify c (onNotes f a)

-- | Division-style variation table, in the order a set unfolds:
-- statement, running divisions, chordal figures, the mirror pair,
-- then broad cadential motion (its trill is attached in 'trillCadence').
variation :: Int -> Music Pitch -> Music Pitch
variation i bass = case i `mod` 6 of
  0 -> transpose 12 bass
  1 -> transpose 12 (onNotes stepFigure bass)
  2 -> transpose 12 (onNotes chordFigure bass)
  3 -> transpose 12 (retro (onNotes stepFigure bass))
  4 -> transpose 24 (invert bass)
  5 -> transpose 12 (onNotes broadFigure bass)
  _ -> bass
  where
    stepFigure d p =
      let a = absPitch p
       in line [note (d / 2) p, note (d / 4) (pitch (a + 2)),
                note (d / 4) p]
    chordFigure d p =
      let a = absPitch p
       in line [note (d / 4) p, note (d / 4) (pitch (a + 7)),
                note (d / 4) (pitch (a + 12)), note (d / 4) (pitch (a + 7))]
    broadFigure d p =
      let a = absPitch p
       in line [note (d * 3 / 4) p, note (d / 4) (pitch (a + 5))]

-- | Flatten a 'Music Pitch' to (onset, dur, midi) triples, honouring
-- 'Tempo' and 'Transpose' the way EuterpeaLite's own 'dur' does.
flatten :: Music Pitch -> [(Rational, Rational, Int)]
flatten = go 0 1 0
  where
    go o k tr m = case m of
      Prim (Note d p) -> [(o, d * k, absPitch p + tr) | d > 0]
      Prim (Rest _) -> []
      a :+: b -> go o k tr a <> go (o + sdur k tr a) k tr b
      a :=: b -> go o k tr a <> go o k tr b
      Modify (Tempo r) a -> go o (k / r) tr a
      Modify (Transpose t) a -> go o k (tr + t) a
      Modify _ a -> go o k tr a
    -- generated lines carry no rests, so the sounding span is the dur
    sdur k tr a = maximum (0 : [t + d | (t, d, _) <- go 0 k tr a])

-- | A generated 'Score': bass on voice 0 (kern discipline — bass
-- first), the variation line on voice 1, trills attached to each
-- variation's cadence so the ornament engine has something to say.
generateScore :: String -> Int -> Bpm -> Either String Score
generateScore name nvar tempo = do
  (bass, meter) <-
    maybe
      (Left ("unknown ground '" <> name <> "'; know: "
               <> unwords groundNames))
      Right
      (lookup name grounds)
  let one = flatten bass
      cycleLen = maximum (0 : [t + d | (t, d, _) <- one])
      shift dt = map (\(t, d, p) -> (t + dt, d, p))
      bassNotes =
        concat [shift (fromIntegral i * cycleLen) one | i <- [0 .. nvar - 1]]
      var i = trillCadence
                (shift (fromIntegral i * cycleLen)
                   (flatten (variation i bass)))
      melNotes = concatMap var [0 .. nvar - 1]
      mkVoice idx ns =
        Voice idx
          [ (scoreNote (WholeNotes t) (WholeNotes d) p ms) {snLane = 0}
          | (t, d, p, ms) <- ns ]
      plain ns = [(t, d, p, []) | (t, d, p) <- ns]
  if nvar < 1
    then Left "need at least one variation"
    else
      Right
        Score
          { scTempo = tempo
          , scVoices = [mkVoice 0 (plain bassNotes), mkVoice 1 melNotes]
          , scTieLeftovers = 0
          , scMergeDrifts = 0
          , scMeter = [(WholeNotes 0, meter)]
          , scGraceDropped = 0
          , scRestHolds = []
          }

-- | Whole-tone trill on the penultimate note of a flattened line.
trillCadence :: [(Rational, Rational, Int)] -> [(Rational, Rational, Int, [Mark])]
trillCadence ns = case reverse ns of
  (lst : pen : more) ->
    reverse (with [] lst : with [Trill 2] pen : map (with []) more)
  _ -> [with [] x | x <- ns]
  where
    with ms (t, d, p) = (t, d, p, ms)
