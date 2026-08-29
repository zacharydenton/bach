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
  ) where

import Data.Ratio (denominator, numerator)

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
toTicks :: WholeNotes -> Ticks
toTicks (WholeNotes w) =
  let q = w * 4 * fromIntegral ticksPerQuarter
   in Ticks (fromIntegral (numerator q) `div` fromIntegral (denominator q))

toSeconds :: Bpm -> WholeNotes -> Seconds
toSeconds (Bpm bpm) (WholeNotes w) =
  Seconds (fromRational w * 4 * 60 / bpm)
