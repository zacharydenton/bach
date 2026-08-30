-- | The interpretation: every parameter family the compiler applies to a
-- score. Lives outside the Player so annotators, interpreter and Player
-- can all see it without cycles.
--
-- License: GPL-2.0-or-later.
module OTB.Interp
  ( Interp (..)
  , defaultInterp
  ) where

import OTB.Config (ArtParams, defaultArtParams)
import OTB.Interp.Agogics (AgogicParams, defaultAgogicParams)
import OTB.Interp.Dynamics (DynParams, defaultDynParams)
import OTB.Interp.Express (ExpressParams, defaultExpressParams)
import OTB.Interp.Ornament (OrnamentParams, defaultOrnamentParams)
import OTB.Interp.Phrasing (PhraseParams, defaultPhraseParams)
import OTB.Tuning (TuningTable, werckmeister3)

data Interp = Interp
  { iArt :: !ArtParams
  , iAgogics :: !AgogicParams
  , iPhrasing :: !PhraseParams
  , iOrnaments :: !OrnamentParams
  , iDynamics :: !DynParams
  , iExpress :: !ExpressParams
  , iPiece :: !String -- ^ seeds the deterministic jitter
  , iAdaptive :: Bool
    -- ^ per-chord adaptive temperament: Werckmeister that relaxes toward
    -- just intonation where the root stands still (bend-carried only)
  , iTuning :: !TuningTable
  , iBendRange :: !Double
  }

defaultInterp :: Interp
defaultInterp =
  Interp defaultArtParams defaultAgogicParams defaultPhraseParams
    defaultOrnamentParams defaultDynParams defaultExpressParams ""
    False werckmeister3 2
