-- | Score -> Performance, via Euterpea's Music algebra.
--
-- Each voice's notes are rebuilt into monophonic *lanes* (the spine paths
-- they came from), articulation is resolved per lane where line context
-- exists, and each lane becomes a @Music (pitch, gate)@ line whose
-- traversal yields the performed events.
--
-- **The lane, not the voice, is the monophonic unit — so the lane gets
-- the MIDI channel.** Pitch bend is per channel, and temperament needs a
-- distinct bend per sounding note; a voice whose sub-spines hold a chord
-- would smear one bend across it. Channel 9 (GM percussion) is skipped
-- for the audition path's sake; more than 15 simultaneous lanes is a
-- cardinality error, reported, not truncated.
--
-- License: GPL-2.0-or-later.
module OTB.Player
  ( PerfNote (..)
  , Performance (..)
  , Interp (..)
  , defaultInterp
  , perform
  ) where

import Data.List (sortOn)
import EuterpeaLite.Music (Music (..), Primitive (..), note, rest)
import OTB.Config (ArtParams, defaultArtParams)
import OTB.Interp.Agogics
  (AgogicParams, defaultAgogicParams, fermataFactor, tempoMap)
import OTB.Interp.Articulation (articulateLane)
import OTB.Interp.Ornament
  (OrnamentParams, defaultOrnamentParams, realizeLane)
import OTB.Interp.Phrasing (PhraseParams, breatheLane, defaultPhraseParams)
import OTB.Score
import OTB.Tuning (TuningTable, bendValue, offsetFor, werckmeister3)
import OTB.Units (Bpm, WholeNotes)

-- | Everything the Player needs beyond the score: the interpretation.
data Interp = Interp
  { iArt :: !ArtParams
  , iAgogics :: !AgogicParams
  , iPhrasing :: !PhraseParams
  , iOrnaments :: !OrnamentParams
  , iTuning :: !TuningTable
  , iBendRange :: !Double
  }

defaultInterp :: Interp
defaultInterp =
  Interp defaultArtParams defaultAgogicParams defaultPhraseParams
    defaultOrnamentParams werckmeister3 2

data PerfNote = PerfNote
  { pnOnset :: !WholeNotes
  , pnDur :: !WholeNotes -- ^ sounding (post-articulation) duration
  , pnPitch :: !Int
  , pnVel :: !Int
  , pnBend :: !Int -- ^ 14-bit, emitted on the note's channel before it sounds
  , pnChannel :: !Int
  }
  deriving (Show)

data Performance = Performance
  { perfTempoMap :: [(WholeNotes, Bpm)] -- ^ the conductor lane, generated
  , perfTracks :: [[PerfNote]] -- ^ one track per voice, onset-sorted
  }
  deriving (Show)

-- | One voice's notes into monophonic lanes, greedily: a note goes to the
-- first lane free at its onset. Because the notes came from monophonic
-- spine paths, greedy assignment reconstructs them faithfully.
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

-- | An articulated lane as a Music line: rests for the gaps, payload
-- (pitch, gate). Durations are already whole notes — Euterpea's Dur —
-- so no time conversion happens here.
laneMusic :: [(ScoreNote, Rational)] -> Music (Int, Rational)
laneMusic = go 0
  where
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
  Modify _ inner -> musicEvents t inner

durOf :: Music a -> Rational
durOf = \case
  Prim (Note d _) -> d
  Prim (Rest d) -> d
  a :+: b -> durOf a + durOf b
  a :=: b -> max (durOf a) (durOf b)
  Modify _ inner -> durOf inner

-- | Interpretation per voice; channel per lane; bend per note from the
-- tuning table at the receiver's bend range; tempo curve over the whole.
perform :: Interp -> Score -> Either String Performance
perform (Interp ap ag pp orn table bendRange) (Score tempo voices _) = do
  let voiceLanes = [(v, lanes (vNotes v)) | v <- voices]
      totalLanes = sum (map (length . snd) voiceLanes)
      end =
        maximum (0 : [snOnset n + snDur n | v <- voices, n <- vNotes v])
  if totalLanes > length usableChannels
    then Left ("score needs " <> show totalLanes
                 <> " monophonic lanes; only "
                 <> show (length usableChannels) <> " MIDI channels available")
    else
      Right . Performance (tempoMap ag tempo end) . snd $
        foldl voiceTrack (usableChannels, []) voiceLanes
  where
    usableChannels = [ch | ch <- [0 .. 15], ch /= 9] -- 9 = GM percussion
    voiceTrack (chans, acc) (_, ls) =
      let (mine, rest') = splitAt (length ls) chans
          -- laneMusic emits exactly one event per articulated note, in
          -- order, so zipping arts with the traversal re-attaches marks
          -- (fermata) to their performed events
          evs = concat
            [ [ PerfNote (fromRational t)
                  (fromRational (d * gate * fermataFactor ag sn)) p 96 bend ch
              | ((sn, gate), (t, d, (p, _))) <-
                  zip arts (musicEvents 0 (laneMusic arts))
              , let bend = bendValue bendRange (offsetFor table p)
              ]
            | (ch, l) <- zip mine ls
            , -- realise ornaments first so articulation and phrasing see
              -- real notes; the solver runs at the piece's base tempo
              let l' = realizeLane orn tempo l
                  arts = breatheLane pp l' (articulateLane ap l')
            ]
       in (rest', acc <> [sortOn pnOnset evs])
