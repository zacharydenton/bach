-- | Interpretation config: the rule parameters and the per-piece override
-- log. Overrides are the exception log — the whole point is that reading
-- the config *is* reading the interpretation.
--
-- The format is a hand-parsed TOML subset (sections, @key = value@ floats,
-- @#@ comments) — the files stay valid TOML, the binary stays lean, in the
-- bcrseqlib dependency-avoidance tradition.
--
-- License: GPL-2.0-or-later.
module OTB.Config
  ( ArtParams (..)
  , defaultArtParams
  , loadConfig
  , artParamsFor
  ) where

import Data.Map.Strict (Map)
import Data.Ratio (approxRational)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as TR

-- | All gates are sounding-fraction-of-notated-duration.
data ArtParams = ArtParams
  { apBase :: !Rational -- ^ détaché default
  , apStaccato :: !Rational -- ^ explicit @'@ mark
  , apTenuto :: !Rational -- ^ explicit @~@ mark
  , apLegato :: !Rational -- ^ inside a slur
  , apRepeated :: !Rational -- ^ note repeated immediately: needs separation
  , apCantabile :: !Rational -- ^ stepwise motion at singing note-values
  , apMinGate :: !Rational -- ^ floor, so nothing vanishes
  }
  deriving (Show, Eq)

defaultArtParams :: ArtParams
defaultArtParams = ArtParams
  { apBase = 0.78
  , apStaccato = 0.40
  , apTenuto = 0.95
  , apLegato = 1.00
  , apRepeated = 0.60
  , apCantabile = 0.90
  , apMinGate = 0.10
  }

type Config = Map Text (Map Text Double)

-- | Parse the TOML subset. Unknown keys are carried, not rejected — the
-- config is allowed to know about future rules before the code does.
loadConfig :: Text -> Config
loadConfig = snd . foldl step ("", Map.empty) . map clean . T.lines
  where
    clean = T.strip . fst . T.breakOn "#"
    step (sect, m) line
      | T.null line = (sect, m)
      | T.isPrefixOf "[" line
      , T.isSuffixOf "]" line =
          (T.strip (T.dropEnd 1 (T.drop 1 line)), m)
      | (k, rest) <- T.breakOn "=" line
      , not (T.null rest)
      , Right (v, _) <- TR.double (T.strip (T.drop 1 rest)) =
          (sect, Map.insertWith Map.union sect (Map.singleton (T.strip k) v) m)
      | otherwise = (sect, m)

-- | Parameters for a piece: defaults, overlaid with @[articulation]@,
-- overlaid with @[piece.<name>]@.
artParamsFor :: Config -> Text -> ArtParams
artParamsFor cfg piece =
  apply (Map.findWithDefault Map.empty ("piece." <> piece) cfg)
    (apply (Map.findWithDefault Map.empty "articulation" cfg) defaultArtParams)
  where
    apply m p =
      p
        { apBase = get "base" (apBase p)
        , apStaccato = get "staccato" (apStaccato p)
        , apTenuto = get "tenuto" (apTenuto p)
        , apLegato = get "legato" (apLegato p)
        , apRepeated = get "repeated" (apRepeated p)
        , apCantabile = get "cantabile" (apCantabile p)
        , apMinGate = get "min_gate" (apMinGate p)
        }
      where
        -- approxRational, not toRational: 0.9 read as Double is not 9/10 in
        -- binary, and gates should be the tidy rationals the config wrote
        get k dflt = maybe dflt (\v -> approxRational v 1e-9) (Map.lookup k m)
