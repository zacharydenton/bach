-- | Score -> Performance, via Euterpea's Music algebra.
--
-- The Score's flat (onset, dur, pitch) notes are rebuilt into a
-- @Music (AbsPitch, gate)@ value — voices in parallel (':=:'), each voice a
-- parallel bundle of monophonic lanes, each lane a ':+:' line of rests and
-- notes. Articulation is resolved *on the lanes* (where line context
-- exists) before the algebra is built, so the payload the Music carries is
-- already the interpretation. Performance is then a pure traversal.
--
-- License: GPL-2.0-or-later.
module OTB.Player
  ( PerfNote (..)
  , Performance (..)
  , perform
  , toMusic
  ) where

import Data.List (sortOn)
import EuterpeaLite.Music (Music (..), Primitive (..), note, rest)
import OTB.Config (ArtParams)
import OTB.Interp.Articulation (articulateLane)
import OTB.Score
import OTB.Units (Bpm, WholeNotes)

data PerfNote = PerfNote
  { pnOnset :: !WholeNotes
  , pnDur :: !WholeNotes -- ^ sounding (post-articulation) duration
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
-- Lanes are built newest-first and reversed to chronological on the way out.
lanes :: [ScoreNote] -> [[ScoreNote]]
lanes = map reverse . foldl place [] . sortOn snOnset
  where
    place ls sn = go ls
      where
        go [] = [[sn]]
        go (l : rest') =
          case l of
            (prev : _) | snOnset prev + snDur prev <= snOnset sn -> (sn : l) : rest'
            _ -> l : go rest'

-- | Rebuild the Euterpea Music value with articulation resolved: payload is
-- (pitch, gate). Durations are already whole notes — the same unit as
-- Euterpea's Dur — so no time conversion happens here at all.
toMusic :: ArtParams -> Voice -> Music (Int, Rational)
toMusic ap v =
  foldr1 (:=:) (map (laneMusic . articulateLane ap) (lanes (vNotes v)))
  where
    laneMusic = go 0
    go _ [] = rest 0
    go t ((sn, gate) : rest') =
      let gap = realToFrac (snOnset sn - t)
          n = note (realToFrac (snDur sn)) (snPitch sn, gate)
       in rest gap :+: n :+: go (snOnset sn + snDur sn) rest'

-- | Traverse a Music value back to timed events. Pure, total.
musicEvents :: Rational -> Music (Int, Rational) -> [(Rational, Rational, (Int, Rational))]
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

-- | Interpretation applied per voice; channel = voice order.
perform :: ArtParams -> Score -> Performance
perform ap (Score tempo voices _) =
  Performance tempo
    [ sortOn pnOnset
        [ PerfNote (fromRational t) (fromRational (d * gate)) p 96 ch
        | (t, d, (p, gate)) <- musicEvents 0 (toMusic ap v)
        ]
    | (ch, v) <- zip [0 ..] voices
    ]
