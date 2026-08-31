-- | The raw score: what the corpus says, nothing more.
--
-- Notes carry notated onset and duration in whole notes, with ties already
-- resolved into single sounding notes and ornament/articulation *marks*
-- retained unrealised — realisation is interpretation and lives in the
-- Player. This is the \"Music Raw\" stage of the plan.
--
-- A tied note keeps its *segments* (one per tied token, each with the marks
-- that token carried) so that a fermata on the closing token holds only the
-- close, not the whole chain. 'snMarks' is the union, for rules that do not
-- care where in the note a mark sat.
--
-- Every note also remembers the spine path (lane) it was read from, so the
-- Player never has to guess which of a voice's sub-spines a note belongs to.
--
-- License: GPL-2.0-or-later.
module OTB.Score
  ( ScoreNote (..)
  , scoreNote
  , Voice (..)
  , Score (..)
  ) where

import OTB.Kern.Token (Mark)
import OTB.Units (Bpm, WholeNotes)

data ScoreNote = ScoreNote
  { snOnset :: !WholeNotes
  , snDur :: !WholeNotes
  , snPitch :: !Int -- ^ MIDI note number
  , snMarks :: ![Mark] -- ^ union over 'snSegs'
  , snLane :: !Int -- ^ spine path within the voice (stable across rests)
  , snSegs :: ![(WholeNotes, [Mark])]
    -- ^ tie chain: (segment duration, that token's marks); sums to 'snDur'
  , snSource :: !(WholeNotes, Int)
    -- ^ notated (onset, pitch) of the score note this came from. Ornament
    -- realisation keeps it on every subnote, so a rule that asks "which
    -- notes were struck together?" (the final-chord roll) can still tell.
  }
  deriving (Eq, Show)

-- | An untied note on lane 0: one segment carrying all the marks.
scoreNote :: WholeNotes -> WholeNotes -> Int -> [Mark] -> ScoreNote
scoreNote t d p ms = ScoreNote t d p ms 0 [(d, ms)] (t, p)

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
  , scMergeDrifts :: !Int
    -- ^ @*v@ merges whose sub-spine clocks disagreed (the later one wins);
    -- a sub-spine short by a rest in the encoding. Diagnostic, not fatal.
  , scMeter :: ![(WholeNotes, (Int, Int))]
    -- ^ meter map: each @*M@ record as (onset, (numerator, denominator)),
    -- onset-ascending; metrical dynamics degrade gracefully when empty
  , scGraceDropped :: !Int
    -- ^ zero-duration tokens with no pitch to realise (grace rests and
    -- malformed graces) — pitched graces are retained as zero-duration
    -- 'Grace'-marked notes and realised by the Player. Counted so the
    -- residual loss stays visible rather than silent.
  , scRestHolds :: !Int
    -- ^ fermatas sitting on rests. The model has no rest to hang them on,
    -- so the hold is not realised — counted so the loss is visible.
  }
  deriving (Show)
