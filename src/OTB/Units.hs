-- | Units as newtypes — Tier 1 of the type plan.
--
-- The classic bug in score-to-performance code is silently mixing notated
-- time with performed time. Kern (and Euterpea) count duration in *whole
-- notes*; MIDI files count in ticks; tempo maps between them via quarters.
-- Each gets its own type so the mixups fail to compile.
--
-- License: GPL-2.0-or-later.
module OTB.Units
  ( WholeNotes (..)
  , Seconds (..)
  , Ticks (..)
  , Cents (..)
  , Bpm (..)
  , ticksPerQuarter
  , toTicks
  , toSeconds
  , secondsAt
  ) where

-- | Notated duration/onset, in whole notes (kern's and Euterpea's native unit:
-- a kern @4c@ is 1/4 whole note).
newtype WholeNotes = WholeNotes Rational
  deriving newtype (Eq, Ord, Num, Fractional, Real, Show)

newtype Seconds = Seconds Double
  deriving newtype (Eq, Ord, Num, Show)

-- | SMF ticks at 'ticksPerQuarter' resolution.
newtype Ticks = Ticks Int
  deriving newtype (Eq, Ord, Num, Show)

newtype Cents = Cents Double
  deriving newtype (Eq, Ord, Num, Show)

-- | Quarter notes per minute (kern @*MM@).
newtype Bpm = Bpm Double
  deriving newtype (Eq, Ord, Num, Show)

ticksPerQuarter :: Int
ticksPerQuarter = 480

-- | Exact where the rational is exact (all plain kern durations are).
-- Rounded through Double rather than integer division: expressive-layer
-- rationals quantised from Doubles can carry large denominators, and a
-- numerator/denominator Int conversion can overflow — wrapping a
-- denominator to zero was a real bug. Tick counts are ~1e6, far inside
-- Double exactness.
toTicks :: WholeNotes -> Ticks
toTicks (WholeNotes w) =
  Ticks (round (fromRational (w * 4 * fromIntegral ticksPerQuarter) :: Double))

toSeconds :: Bpm -> WholeNotes -> Seconds
toSeconds (Bpm bpm) (WholeNotes w) =
  Seconds (fromRational w * 4 * 60 / bpm)

-- | Absolute wall-clock position of a score position under a tempo map
-- (piecewise integration; the map is (onset, tempo-from-here) ascending).
secondsAt :: [(WholeNotes, Bpm)] -> WholeNotes -> Seconds
secondsAt tmap target = go 0 tmap
  where
    go acc ((t1, bpm) : rest@((t2, _) : _))
      | target <= t2 = acc + toSeconds bpm (max 0 (target - t1))
      | otherwise = go (acc + toSeconds bpm (t2 - t1)) rest
    go acc [(t1, bpm)] = acc + toSeconds bpm (max 0 (target - t1))
    go acc [] = acc
