-- | The per-piece fitting rig's pure half: grids, shrinkage, section
-- building and the config writer. Ports tools/piece_fit.py (state
-- schema and writer semantics preserved: PIECE-FIT provenance lines,
-- hand-authored keys always win, edits applied bottom-up BY POSITION,
-- idempotent regeneration).
--
-- License: GPL-2.0-or-later.
module OTB.PieceFit
  ( timingKnobs
  , velocityKnobs
  , FitLayer (..)
  , PieceRec (..)
  , shrinkKnobs
  , fmtD
  , buildSections
  , applyFits
  ) where

import Data.List (foldl', sortOn)
import Data.Map.Strict qualified as Map
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Numeric (showFFloat)

mark :: Text
mark = "# PIECE-FIT"

timingKnobs :: [(Text, [Double])]
timingKnobs =
  [ ("arch_piece", [0.0, 0.012, 0.024, 0.05, 0.08])
  , ("boundary_ease", [0.0, 0.02, 0.04, 0.08])
  , ("cadence_depth", [0.0, 0.03, 0.06, 0.1])
  , ("open_push", [0.0, 0.03, 0.06, 0.12])
  , ("rit_span", [0.5, 1.0, 2.0, 4.0])
  , ("rit_floor", [0.2, 0.3, 0.45, 0.6])
  ]

velocityKnobs :: [(Text, [Double])]
velocityKnobs =
  [ ("vel_highloud", [0.0, 0.4, 0.8, 1.2])
  , ("vel_bar", [0.0, 4.0, 8.0])
  , ("vel_beat", [0.0, 3.0, 6.0])
  , ("dialogue_vel", [0.0, 2.0, 5.0])
  , ("subject_vel", [0.0, 5.0, 10.0])
  ]

-- | One layer's fit outcome for a piece.
data FitLayer = FitLayer
  { flShrunk :: [(Text, Double)]
  , flBaselineR :: !Double
  , flDeployedR :: !Double
  , flLopoDelta :: Maybe Double
  , flKept :: !Bool
  }

data PieceRec = PieceRec
  { prPiece :: String
  , prN :: !Int
  , prTempo :: Maybe (Double, Double, Double)
    -- ^ (human median, authority, fitted)
  , prTiming :: Maybe FitLayer
  , prVelocity :: Maybe FitLayer
  }

-- | Shrink fitted deltas toward the global base by n/(n+k), rounded to
-- four decimals like the reference.
shrinkKnobs :: [(Text, Double)] -> (Text -> Double) -> Int -> Double
            -> [(Text, Double)]
shrinkKnobs fitted baseOf n k =
  [ (key, roundTo 4 (b + (v - b) * fromIntegral n
                           / (fromIntegral n + k)))
  | (key, v) <- fitted
  , let b = baseOf key ]

roundTo :: Int -> Double -> Double
roundTo d x =
  let m = 10 ^^ d
   in fromIntegral (round (x * m) :: Integer) / m

-- | Python-repr-ish float: fixed decimals with trailing zeros trimmed
-- (but at least one digit after the point).
fmtD :: Int -> Double -> String
fmtD d x =
  let s = showFFloat (Just d) x ""
      t = reverse (dropWhile (== '0') (reverse s))
   in if last t == '.' then t <> "0" else t

fmtSigned :: Double -> String
fmtSigned x = (if x >= 0 then "+" else "") <> fmtD 4 x

-- | piece -> generated (key, value-text, comment) lines.
buildSections :: String -> [PieceRec] -> [(String, [(Text, String, String)])]
buildSections date recs =
  [ (prPiece rec, lns)
  | rec <- sortOn prPiece recs
  , let lns = tempoLine rec <> layers rec
  , not (null lns) ]
  where
    tempoLine rec = case prTempo rec of
      Nothing -> []
      Just (human, auth, fitted) ->
        [ ("tempo", fmtD 1 fitted
          , T.unpack mark <> " " <> date <> " n=" <> show (prN rec)
              <> " human " <> fmtD 1 human <> " vs authority "
              <> fmtD 1 auth) ]
    layers rec = concat
      [ [ (k, fmtD 4 v
          , T.unpack mark <> " " <> date <> " n=" <> show (prN rec)
              <> " r " <> fmtD 4 (flBaselineR fl) <> "->"
              <> fmtD 4 (flDeployedR fl)
              <> maybe " unvalidated(n<3)"
                   ((" LOPO " <>) . fmtSigned) (flLopoDelta fl))
        | (k, v) <- sortOn fst (flShrunk fl) ]
      | Just fl <- [prTiming rec, prVelocity rec]
      , flKept fl ]

-- | Rewrite the config text with the generated sections: existing
-- [piece.X] sections are updated in place bottom-up by POSITION (their
-- PIECE-FIT lines replaced, hand lines untouched and blocking), and
-- pieces without a section land under the banner.
applyFits :: String -> [PieceRec] -> Text -> Text
applyFits date recs cfgText =
  T.unlines (finish (foldl' patch (srcLines, sects0) byPos))
  where
    srcLines = T.lines (T.dropWhileEnd (== '\n') cfgText)
    sects0 = Map.fromList (buildSections date recs)

    spans = collect Nothing 0 (srcLines <> ["[end]"]) []
    collect cur _i [] acc = maybe acc (\(n, a, b) -> (n, (a, b)) : acc)
      (fmap (\(n, a) -> (n, a, length srcLines)) cur)
    collect cur i (l : ls) acc =
      let s = T.strip (fst (T.breakOn "#" l))
          isHdr = "[" `T.isPrefixOf` s && "]" `T.isSuffixOf` s
       in if not isHdr
            then collect cur (i + 1) ls acc
            else
              let acc' = case cur of
                    Just (n, a) -> (n, (a, i)) : acc
                    Nothing -> acc
                  name = T.drop 1 (T.dropEnd 1 s)
                  cur' = if "piece." `T.isPrefixOf` name
                           then Just (T.unpack (T.drop 6 name), i)
                           else Nothing
               in collect cur' (i + 1) ls acc'

    spanMap = Map.fromList spans
    byPos = sortOn (Down . (spanMap Map.!))
      [p | p <- Map.keys sects0, Map.member p spanMap]

    patch (lns, sects) piece =
      case (Map.lookup piece spanMap, Map.lookup piece sects) of
        (Just (a, b), Just gen) ->
          let body = take (b - a - 1) (drop (a + 1) lns)
              handKeys =
                [ T.strip (fst (T.breakOn "=" l))
                | l <- body
                , "=" `T.isInfixOf` l, not (mark `T.isInfixOf` l) ]
              kept = dropTrailingBlank
                       [l | l <- body, not (mark `T.isInfixOf` l)]
              add = [ T.pack (T.unpack k <> " = " <> v <> " " <> c)
                    | (k, v, c) <- gen, k `notElem` handKeys ]
           in ( take (a + 1) lns <> kept <> add <> [""] <> drop b lns
              , Map.delete piece sects )
        _ -> (lns, sects)

    dropTrailingBlank = reverse . dropWhile (T.null . T.strip) . reverse

    finish (lns, sects)
      | Map.null sects = lns
      | otherwise =
          lns
            <> [""]
            <> [T.pack ("# ---- fitted per piece (otb fit, " <> date
                          <> ") ----")]
            <> concat
              [ [""] <> [T.pack ("[piece." <> piece <> "]")]
                  <> [ T.pack (T.unpack k <> " = " <> v <> " " <> c)
                     | (k, v, c) <- gen ]
              | (piece, gen) <- Map.toAscList sects ]
