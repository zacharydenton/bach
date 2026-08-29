-- | The raw score: what the corpus says, nothing more.
--
-- Notes carry notated onset and duration in whole notes, with ties already
-- resolved into single sounding notes and ornament/articulation *marks*
-- retained unrealised — realisation is interpretation and lives in the
-- Player. This is the \"Music Raw\" stage of the plan.
--
-- License: GPL-2.0-or-later.
module OTB.Score
  ( ScoreNote (..)
  , Voice (..)
  , Score (..)
  ) where

import OTB.Kern.Token (Mark)
import OTB.Units (Bpm, WholeNotes)

data ScoreNote = ScoreNote
  { snOnset :: !WholeNotes
  , snDur :: !WholeNotes
  , snPitch :: !Int -- ^ MIDI note number
  , snMarks :: ![Mark]
  }
  deriving (Eq, Show)

data Voice = Voice
  { vIndex :: !Int -- ^ original top-level spine index
  , vNotes :: [ScoreNote] -- ^ onset-sorted
  }
  deriving (Show)

data Score = Score
  { scTempo :: !Bpm -- ^ from @*MM@, or the CLI default
  , scVoices :: [Voice]
  , scTieLeftovers :: !Int
    -- ^ ties still open at EOF, flushed as sounding notes; nonzero usually
    -- means an enharmonic respelling at the close. Diagnostic, not fatal.
  }
  deriving (Show)
