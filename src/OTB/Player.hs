-- | Score -> Performance, via Euterpea's Music algebra.
--
-- The Score's flat (onset, dur, pitch) notes are rebuilt into a
-- @Music AbsPitch@ value — voices in parallel (':=:'), each voice a
-- parallel bundle of monophonic lanes, each lane a ':+:' line of rests
-- and notes — and the performance is then a pure traversal of that value.
-- M0 interpretation is a single fixed articulation: gate as a fraction of
-- notated duration. The rule engine grows here (M2+), nowhere else.
--
-- License: GPL-2.0-or-later.
module OTB.Player
  ( PerfNote (..)
  , Performance (..)
  , perform
  , toMusic
  ) where

import Data.List (sortOn)
import EuterpeaLite.Music (AbsPitch, Music (..), Primitive (..), note, rest)
import OTB.Score
import OTB.Units (Bpm, WholeNotes)

data PerfNote = PerfNote
  { pnOnset :: !WholeNotes
  , pnDur :: !WholeNotes -- ^ sounding (post-gate) duration
  , pnPitch :: !Int
  , pnVel :: !Int
  , pnChannel :: !Int
  }
  deriving (Show)

data Performance = Performance
  { perfTempo :: !Bpm
  , perfTracks :: [[PerfNote]] -- ^ one track per voice, onset-sorted
  }
  deriving (Show)

-- | One voice's notes into monophonic lanes, greedily: a note goes to the
-- first lane that is free at its onset. Because the notes *came from*
-- monophonic spine paths, greedy assignment reconstructs them faithfully.
lanes :: [ScoreNote] -> [[ScoreNote]]
lanes = foldl place [] . sortOn snOnset
  where
    place ls sn = go ls
      where
        go [] = [[sn]]
        go (l : rest') =
          case l of
            (prev : _) | snOnset prev + snDur prev <= snOnset sn -> (sn : l) : rest'
            _ -> l : go rest'

-- | Rebuild the Euterpea Music value: a lane is rests-and-notes in ':+:',
-- a voice is its lanes in ':=:'. Durations are already whole notes — the
-- same unit as Euterpea's Dur — so no conversion happens here at all.
toMusic :: Voice -> Music AbsPitch
toMusic v = foldr1 (:=:) (map laneMusic (lanes (vNotes v)))
  where
    laneMusic l = go 0 (reverse l)
    go _ [] = rest 0
    go t (sn : rest') =
      let gap = wn (snOnset sn - t)
          n = note (wn (snDur sn)) (snPitch sn)
       in rest gap :+: n :+: go (snOnset sn + snDur sn) rest'
    wn w = toRational (num w)
    num :: WholeNotes -> Rational
    num = realToFrac

-- | Traverse a Music value back to timed events. Pure, total.
musicEvents :: Rational -> Music AbsPitch -> [(Rational, Rational, AbsPitch)]
musicEvents t m = case m of
  Prim (Note d p) -> [(t, d, p)]
  Prim (Rest _) -> []
  a :+: b -> musicEvents t a <> musicEvents (t + durOf a) b
  a :=: b -> musicEvents t a <> musicEvents t b
  Modify _ inner -> musicEvents t inner -- no controls emitted yet

durOf :: Music a -> Rational
durOf = \case
  Prim (Note d _) -> d
  Prim (Rest d) -> d
  a :+: b -> durOf a + durOf b
  a :=: b -> max (durOf a) (durOf b)
  Modify _ inner -> durOf inner

-- | M0 interpretation: fixed gate, fixed velocity, channel = voice order.
perform :: Rational -> Score -> Performance
perform gate (Score tempo voices) =
  Performance tempo
    [ sortOn pnOnset
        [ PerfNote (fromRational t) (fromRational (d * gate)) p 96 ch
        | (t, d, p) <- musicEvents 0 (toMusic v)
        ]
    | (ch, v) <- zip [0 ..] voices
    ]
