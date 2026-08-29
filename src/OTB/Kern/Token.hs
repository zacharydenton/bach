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
  | ITerminate          -- ^ @*-@
  | ITempo !Bpm         -- ^ @*MM112@
  | IMeter !Text        -- ^ @*M4/4@ — carried, not yet consumed
  | ITandem             -- ^ any other @*...@ (clef, key sig, …) — ignored
  deriving (Show)

data Tie = TieNone | TieOpen | TieContinue | TieClose
  deriving (Eq, Show)

-- | Marks the parser retains but does not realise — realisation is
-- interpretation and belongs in the Player, not the parser.
data Mark
  = Staccato   -- ^ @'@
  | Tenuto     -- ^ @~@
  | Accent     -- ^ @^@
  | Fermata    -- ^ @;@
  | Trill      -- ^ @T@ / @t@
  | Mordent    -- ^ @M@ / @m@
  | Turn       -- ^ @S@ / @$@ / @W@ / @w@
  | SlurOpen   -- ^ @(@
  | SlurClose  -- ^ @)@
  deriving (Eq, Show)

data NoteTok = NoteTok
  { ntDur :: !WholeNotes    -- ^ from reciprocal + dots
  , ntPitch :: !(Maybe Int) -- ^ MIDI note number; Nothing = rest
  , ntTie :: !Tie
  , ntMarks :: ![Mark]
  }
  deriving (Show)
