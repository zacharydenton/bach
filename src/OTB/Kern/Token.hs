-- | Token types for the **kern subset used by Bach keyboard corpora.
--
-- One kern line = one record; tab-separated fields, one per live spine.
-- A field is interpretation (@*...@), barline (@=...@), comment (@!...@),
-- null (@.@) or data. A data field may hold several space-separated notes
-- (a chord within one spine).
--
-- License: GPL-2.0-or-later.
module OTB.Kern.Token
  ( Record (..)
  , Field (..)
  , Interp (..)
  , NoteTok (..)
  , Tie (..)
  , Mark (..)
  ) where

import Data.Text (Text)
import OTB.Pitch (Spelled)
import OTB.Units (Bpm, WholeNotes)

data Record = Record
  { recLine :: !Int -- ^ 1-based source line, for errors
  , recFields :: [Field]
  }
  deriving (Show)

data Field
  = FInterp !Interp
  | FBar !Text          -- ^ barline token, verbatim (@=1@, @==@, @=1-@ …)
  | FNull               -- ^ @.@ — spine has no event at this slice
  | FData ![NoteTok]    -- ^ one or more notes sounding together in this spine
  | FComment
  deriving (Show)

data Interp
  = IKernStart          -- ^ @**kern@
  | IOtherExclusive !Text -- ^ @**dynam@ etc — spine to be ignored wholesale
  | ISplit              -- ^ @*^@
  | IMerge              -- ^ @*v@
  | IAdd                -- ^ @*+@ (unsupported; rejected in Spine)
  | IExchange           -- ^ @*x@ (unsupported; rejected in Spine)
  | ITerminate          -- ^ @*-@
  | ITempo !Bpm         -- ^ @*MM112@
  | IMeter !Text        -- ^ @*M4/4@ — carried, not yet consumed
  | ITandem             -- ^ any other @*...@ (clef, key sig, …) — ignored
  deriving (Show)

data Tie = TieNone | TieOpen | TieContinue | TieClose
  deriving (Eq, Show)

-- | Marks the parser retains but does not realise — realisation is
-- interpretation and belongs in the Player, not the parser.
--
-- Ornament intervals come straight from kern's case convention (uppercase
-- = whole tone, lowercase = semitone), so the auxiliary needs no scale
-- inference: the corpus states it.
data Mark
  = Staccato        -- ^ @'@
  | Tenuto          -- ^ @~@
  | Accent          -- ^ @^@
  | Fermata         -- ^ @;@
  | Trill !Int      -- ^ @T@=2 / @t@=1 semitones above
  | Mordent !Int    -- ^ @M@=2 / @m@=1 semitones below (lower neighbour)
  | InvMordent !Int -- ^ @W@=2 / @w@=1 semitones above (Pralltriller)
  | Turn            -- ^ @S@
  | InvTurn         -- ^ @$@
  | GenericOrn      -- ^ @O@ — ornament with no interval stated; retained
                    --   unrealised (realising it would need the scale
                    --   inference this module's convention avoids)
  | Sforzando       -- ^ @z@
  | Grace           -- ^ @q@\/@Q@ — grace note: zero notated duration,
                    --   realised by the Player (C.P.E. Bach: on the beat,
                    --   taking its time from the note it ornaments)
  | SlurOpen        -- ^ @(@
  | SlurClose       -- ^ @)@
  deriving (Eq, Show)

data NoteTok = NoteTok
  { ntDur :: !WholeNotes    -- ^ from reciprocal + dots
  , ntDots :: !Int
    -- ^ notated augmentation dots. Retained because duration cannot
    -- recover them: a dotted whole and a triplet breve (@2%3@) are the
    -- same rational
  , ntPitch :: !(Maybe Int) -- ^ MIDI note number; Nothing = rest
  , ntSpell :: !(Maybe Spelled)
    -- ^ the notation behind 'ntPitch' — present exactly when the pitch is
  , ntTie :: !Tie
  , ntMarks :: ![Mark]
  }
  deriving (Show)
