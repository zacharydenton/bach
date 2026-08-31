-- | Spelled pitch: what the corpus writes, before it becomes a MIDI number.
--
-- Kern states every accidental explicitly on the note; collapsing to a
-- MIDI Int at the lexer discarded notation that downstream consumers
-- want back. Tie identity is a staff position (letter + octave), not a
-- chromatic pitch — the corpus occasionally drops the accidental on a
-- continuation. Counterpoint interval quality needs letter distance: a
-- diminished sixth spans seven semitones but is not a fifth. Ornament
-- auxiliaries are diatonic neighbours (still whole-tone-defaulted; the
-- harmony model can finish that job now that spelling survives).
--
-- The MIDI number is derived ('spMidi'), never stored beside a
-- contradicting spelling.
--
-- License: GPL-2.0-or-later.
module OTB.Pitch
  ( Spelled (..)
  , spMidi
  , spDegree
  , spName
  ) where

-- | Letter 0..6 = C..B, alteration in sharps (negative = flats),
-- octave in scientific pitch notation (C4 = middle C).
data Spelled = Spelled
  { spLetter :: !Int
  , spAlter :: !Int
  , spOctave :: !Int
  }
  deriving (Eq, Ord, Show)

-- | Derived MIDI note number: C4 = 60.
spMidi :: Spelled -> Int
spMidi (Spelled l a o) =
  12 * (o + 1) + ([0, 2, 4, 5, 7, 9, 11] !! (l `mod` 7)) + a

-- | Absolute diatonic degree (letter steps from C0): interval sizes are
-- differences of this, mod 7 for the class.
spDegree :: Spelled -> Int
spDegree (Spelled l _ o) = 7 * o + l

-- | @"C#4"@-style name for diagnostics.
spName :: Spelled -> String
spName (Spelled l a o) =
  ("CDEFGAB" !! (l `mod` 7))
    : (if a >= 0 then replicate a '#' else replicate (negate a) 'b')
    <> show o
