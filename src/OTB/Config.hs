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
  , Config
  , defaultArtParams
  , loadConfig
  , emptyConfig
  , artParamsFor
  , agogicsFor
  , phrasingFor
  , ornamentsFor
  , dynamicsFor
  , expressFor
  , pieceTempo
  , tuningBendRange
  ) where

import Data.Map.Strict (Map)
import Data.Ratio (approxRational)
import OTB.Interp.Agogics (AgogicParams (..))
import OTB.Interp.Dynamics (DynParams (..))
import OTB.Interp.Express (ExpressParams (..))
import OTB.Interp.Ornament (OrnamentParams (..))
import OTB.Interp.Phrasing (PhraseParams (..))
import OTB.Units (WholeNotes (..))
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

-- | No file: every rule at its built-in default.
emptyConfig :: Config
emptyConfig = Map.empty

-- | Parse the TOML subset. Unknown keys are carried, not rejected — the
-- config is allowed to know about future rules before the code does — but
-- a line that is neither blank, a section, nor @key = number@ is an error
-- (a typo must not silently fall back to the default), and known keys are
-- range-checked so a bad value fails here rather than hanging the tempo
-- grid or emitting an invalid event.
loadConfig :: Text -> Either String Config
loadConfig src = do
  cfg <- snd <$> foldM step ("", Map.empty) (zip [1 :: Int ..] (T.lines src))
  case [ msg | (sect, kvs) <- Map.toList cfg, (k, v) <- Map.toList kvs
             , Just msg <- [checkValue sect k v] ] of
    [] -> Right cfg
    (msg : _) -> Left msg
  where
    foldM f z = foldl (\acc x -> acc >>= \s -> f s x) (Right z)
    clean = T.strip . fst . T.breakOn "#"
    step (sect, m) (n, raw)
      | T.null line = Right (sect, m)
      | T.isPrefixOf "[" line
      , T.isSuffixOf "]" line =
          Right (T.strip (T.dropEnd 1 (T.drop 1 line)), m)
      | (k, rest) <- T.breakOn "=" line
      , not (T.null rest)
      , not (T.null (T.strip k)) =
          case TR.double (T.strip (T.drop 1 rest)) of
            Right (v, junk) | T.null junk ->
              Right (sect, Map.insertWith Map.union sect
                             (Map.singleton (T.strip k) v) m)
            _ -> Left ("line " <> show n <> ": " <> T.unpack (T.strip k)
                         <> " must be a plain number, got "
                         <> T.unpack (T.strip (T.drop 1 rest)))
      | otherwise = Left ("line " <> show n <> ": cannot parse: " <> T.unpack line)
      where line = clean raw
    checkValue sect k v
      | isNaN v || isInfinite v = bad "a finite number"
      | k `elem` positive, v <= 0 = bad "> 0"
      | k `elem` nonNegative, v < 0 = bad ">= 0"
      | k `elem` gates, v <= 0 || v > 1 = bad "in (0, 1]"
      | k `elem` fractions, v < 0 || v >= 1 = bad "in [0, 1)"
      | k == "overhold", v < 0 || v > 1 = bad "in [0, 1]"
      | k == "fermata_hold", v < 1 = bad ">= 1"
      | k == "vel_base", v < 1 || v > 127 = bad "in [1, 127]"
      | k == "arch_bars", v < 1 = bad ">= 1 (rounded to whole bars)"
      | k == "trill_accel", v < 1 = bad ">= 1 (1 = even trill)"
      | otherwise = Nothing
      where
        bad want = Just ("[" <> T.unpack sect <> "] " <> T.unpack k <> " = "
                           <> show v <> ": must be " <> want)
    positive = ["tempo_step", "trill_rate", "bend_range", "tempo"]
    nonNegative =
      [ "rit_span", "rit_curve", "expression", "ensemble", "lead_ms", "roll_ms"
      , "jitter_ms", "jitter_vel", "dis_vel", "dis_lean", "arch_piece"
      , "breath_threshold", "w_gap", "w_dur", "w_leap"
      , "w_cadence", "cadence_span", "mel_charge", "harm_charge"
      , "subject_vel", "leap_dur", "trill_termination" ]
    gates = ["base", "staccato", "tenuto", "legato", "repeated", "cantabile"
            , "min_gate", "rit_floor"]
    -- these scale a duration or tempo multiplicatively: >= 1 would
    -- produce zero or negative durations/BPM downstream
    fractions = [ "breath", "inegal", "uphill", "double_dur"
                , "dur_contrast", "leap_pause", "cadence_depth"
                , "arch_piece", "arch_group" ]

-- | Agogic parameters from @[agogics]@ overlaid with @[piece.<name>]@.
agogicsFor :: Config -> Text -> AgogicParams -> AgogicParams
agogicsFor cfg piece dflt =
  apply (Map.findWithDefault Map.empty ("piece." <> piece) cfg)
    (apply (Map.findWithDefault Map.empty "agogics" cfg) dflt)
  where
    apply m p =
      p
        { agRitSpan = getW "rit_span" (agRitSpan p)
        , agRitFloor = maybe (agRitFloor p) id (Map.lookup "rit_floor" m)
        , agRitCurve = maybe (agRitCurve p) id (Map.lookup "rit_curve" m)
        , agTempoStep = getW "tempo_step" (agTempoStep p)
        , agFermataHold = getR "fermata_hold" (agFermataHold p)
        , agCadenceDepth = maybe (agCadenceDepth p) id (Map.lookup "cadence_depth" m)
        , agCadenceSpan = getW "cadence_span" (agCadenceSpan p)
        }
      where
        getW k dflt' = maybe dflt' (WholeNotes . (\v -> approxRational v 1e-9)) (Map.lookup k m)
        getR k dflt' = maybe dflt' (\v -> approxRational v 1e-9) (Map.lookup k m)

-- | Phrasing parameters from @[phrasing]@ overlaid with @[piece.<name>]@.
phrasingFor :: Config -> Text -> PhraseParams -> PhraseParams
phrasingFor cfg piece dflt =
  apply (Map.findWithDefault Map.empty ("piece." <> piece) cfg)
    (apply (Map.findWithDefault Map.empty "phrasing" cfg) dflt)
  where
    apply m p =
      p
        { ppThreshold = getD "breath_threshold" (ppThreshold p)
        , ppBreath = maybe (ppBreath p) (\v -> approxRational v 1e-9) (Map.lookup "breath" m)
        , ppWGap = getD "w_gap" (ppWGap p)
        , ppWDur = getD "w_dur" (ppWDur p)
        , ppWLeap = getD "w_leap" (ppWLeap p)
        , ppWCadence = getD "w_cadence" (ppWCadence p)
        }
      where
        getD k dflt' = maybe dflt' id (Map.lookup k m)

-- | Ornament parameters from @[ornaments]@ overlaid with @[piece.<name>]@.
ornamentsFor :: Config -> Text -> OrnamentParams -> OrnamentParams
ornamentsFor cfg piece dflt =
  apply (Map.findWithDefault Map.empty ("piece." <> piece) cfg)
    (apply (Map.findWithDefault Map.empty "ornaments" cfg) dflt)
  where
    apply m p =
      p { opTrillRate = maybe (opTrillRate p) id (Map.lookup "trill_rate" m)
        , opTrillAccel = maybe (opTrillAccel p) id (Map.lookup "trill_accel" m)
        , opTermination = maybe (opTermination p) (> 0.5)
            (Map.lookup "trill_termination" m)
        }

-- | Dynamic parameters from @[dynamics]@ overlaid with @[piece.<name>]@.
dynamicsFor :: Config -> Text -> DynParams -> DynParams
dynamicsFor cfg piece dflt =
  apply (Map.findWithDefault Map.empty ("piece." <> piece) cfg)
    (apply (Map.findWithDefault Map.empty "dynamics" cfg) dflt)
  where
    apply m p =
      p
        { dyBase = g "vel_base" (dyBase p)
        , dyBar = g "vel_bar" (dyBar p)
        , dyHalfBar = g "vel_halfbar" (dyHalfBar p)
        , dyBeat = g "vel_beat" (dyBeat p)
        , dyArch = g "vel_arch" (dyArch p)
        , dyHighLoud = g "vel_highloud" (dyHighLoud p)
        , dyAccent = g "vel_accent" (dyAccent p)
        }
      where
        g k dflt' = maybe dflt' id (Map.lookup k m)

-- | Expressive-layer parameters. Two master knobs in @[interpretation]@
-- (Director Musices' architecture: rule quantities scaled by family
-- multipliers), rule quantities in their own sections, and the style
-- *decisions* (inegal, overhold, tempo) living per piece.
expressFor :: Config -> Text -> ExpressParams -> ExpressParams
expressFor cfg piece dflt =
  overlay pieceSec
    (overlay (sec "performance")
      (overlay (sec "arches")
        (overlay (sec "dissonance")
          (overlay (sec "interpretation") dflt))))
  where
    sec k = Map.findWithDefault Map.empty k cfg
    pieceSec = Map.findWithDefault Map.empty ("piece." <> piece) cfg
    overlay m p =
      p
        { exExpression = g "expression" (exExpression p)
        , exEnsemble = g "ensemble" (exEnsemble p)
        , exDisVel = g "dis_vel" (exDisVel p)
        , exDisLean = g "dis_lean" (exDisLean p)
        , exArchPiece = g "arch_piece" (exArchPiece p)
        , exArchGroup = g "arch_group" (exArchGroup p)
        , exArchBars = maybe (exArchBars p) round (Map.lookup "arch_bars" m)
        , exLeadMs = g "lead_ms" (exLeadMs p)
        , exRollMs = g "roll_ms" (exRollMs p)
        , exJitterMs = g "jitter_ms" (exJitterMs p)
        , exJitterVel = g "jitter_vel" (exJitterVel p)
        , exInegal = g "inegal" (exInegal p)
        , exOverhold = g "overhold" (exOverhold p)
        , exMelCharge = g "mel_charge" (exMelCharge p)
        , exHarmCharge = g "harm_charge" (exHarmCharge p)
        , exSubjectVel = g "subject_vel" (exSubjectVel p)
        , exDurContrast = g "dur_contrast" (exDurContrast p)
        , exLeapPause = g "leap_pause" (exLeapPause p)
        , exLeapDur = g "leap_dur" (exLeapDur p)
        , exUphill = g "uphill" (exUphill p)
        , exDoubleDur = g "double_dur" (exDoubleDur p)
        }
      where
        g k dflt' = maybe dflt' id (Map.lookup k m)

-- | Per-piece tempo override (@[piece.X] tempo = 96@) — half of Book II's
-- preludes carry the encoder's 72-BPM placeholder, not a musical choice.
pieceTempo :: Config -> Text -> Maybe Double
pieceTempo cfg piece =
  Map.lookup "tempo" =<< Map.lookup ("piece." <> piece) cfg

-- | @[tuning] bend_range@ — the receiver's pitch-bend range in ± semitones.
-- 2 is the near-universal power-on default; set it to whatever the
-- hardware is actually configured for, or temperament lands scaled wrong.
tuningBendRange :: Config -> Double
tuningBendRange cfg =
  maybe 2.0 id (Map.lookup "bend_range" =<< Map.lookup "tuning" cfg)

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
