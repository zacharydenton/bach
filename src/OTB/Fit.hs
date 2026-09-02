-- | Shared machinery for the in-process fitting rigs: knob families,
-- config candidates, objectives, coordinate descent, and a tiny
-- deterministic PRNG (no dependency; runs must be reproducible from a
-- seed alone).
--
-- Ports tools/knob_landscape.py and the search half of
-- tools/piece_fit.py. Objectives are the OTB.Eval / OTB.Bridge
-- statistics — a candidate evaluation is a parMap over pieces, no
-- subprocesses, no serialization.
--
-- License: GPL-2.0-or-later.
module OTB.Fit
  ( KnobFamily (..)
  , timingFamily
  , velocityFamily
  , prefitStrip
  , applyKnobs
  , PieceData (..)
  , loadPieceData
  , timingRs
  , velocityRs
  , velocityRsNoOrn
  , meanFlat
  , meanOfPieceMedians
  , medianD
  , Rng
  , mkRng
  , nextInt
  , shuffle
  , choice
  , descend
  ) where

import Data.Bits (shiftR, xor)
import Data.Word (Word64)
import Data.List (foldl', sort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import OTB.Bridge qualified as B
import OTB.Config (Config)
import OTB.Eval (PerfR (..), loadDirAnnotations, pearson,
                 scorePerformances)
import OTB.Player (Performance (..))
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))

-- ---------------------------------------------------------------------
-- knob families (grids mirror the retired Python tables)

data KnobFamily = KnobFamily
  { kfName :: String
  , kfGrids :: [(Text, [Double])]
  , kfSections :: Map Text Text
  }

timingFamily :: KnobFamily
timingFamily = KnobFamily "timing" grids sections
  where
    -- asap_eval.KNOB_GRIDS, plus the fine sub-grid values the
    -- 2026-09-02 floor-only ablations motivated (novelty_brake 0.005,
    -- arch_group 0.0025 and cadence_depth 0.01 all ran at or above
    -- the coarse-lattice optimum): arch_group, cadence_depth and
    -- novelty_brake gain steps below their old smallest nonzero.
    -- Every grid keeps its OFF value; the experiment fingerprint
    -- separates fine-lattice runs from the registered coarse ones.
    grids =
      [ ("expression", [0.4, 0.6, 0.8, 1.0, 1.3])
      , ("arch_piece", [0.0, 0.015, 0.03, 0.06])
      , ("arch_group", [0.0, 0.0025, 0.005, 0.01, 0.02, 0.04])
      , ("rit_floor", [0.3, 0.4, 0.5, 0.6, 0.75])
      , ("rit_span", [0.5, 1.0, 2.0, 3.0])
      , ("cadence_depth", [0.0, 0.01, 0.02, 0.04, 0.08, 0.15])
      , ("boundary_ease", [0.0, 0.03, 0.06, 0.12])
      , ("open_push", [0.0, 0.03, 0.06, 0.1])
      , ("open_span", [1.0, 2.0, 4.0])
      , ("subject_push", [0.0, 0.015, 0.03])
      , ("novelty_brake", [0.0, 0.005, 0.01, 0.02, 0.04, 0.08])
      , ("mid_drift", [0.0, 0.01, 0.02, 0.04])
      , ("sus_lean", [0.0, 0.02, 0.05, 0.1])
      ]
    sections = Map.fromList
      [ ("expression", "interpretation")
      , ("arch_piece", "arches"), ("arch_group", "arches")
      , ("rit_floor", "agogics"), ("rit_span", "agogics")
      , ("cadence_depth", "agogics"), ("boundary_ease", "agogics")
      , ("open_push", "agogics"), ("open_span", "agogics")
      , ("subject_push", "agogics"), ("novelty_brake", "agogics")
      , ("mid_drift", "agogics")
      , ("sus_lean", "dissonance")
      ]

velocityFamily :: KnobFamily
velocityFamily = KnobFamily "velocity" grids sections
  where
    -- vel_fit.VEL_KNOB_GRIDS, verbatim
    grids =
      [ ("vel_bar", [0.0, 4.0, 8.0, 12.0, 18.0])
      , ("vel_halfbar", [0.0, 3.0, 6.0, 9.0])
      , ("vel_beat", [0.0, 1.5, 3.0, 6.0])
      , ("vel_arch", [0.0, 4.0, 8.0, 16.0])
      , ("vel_highloud", [0.0, 0.12, 0.25, 0.5, 0.8])
      , ("dis_vel", [0.0, 5.0, 10.0, 20.0])
      , ("sus_soft", [0.0, 2.0, 4.0, 8.0])
      , ("mel_charge", [0.0, 0.2, 0.4, 0.8])
      , ("harm_charge", [0.0, 0.15, 0.3, 0.6])
      , ("subject_vel", [0.0, 2.5, 5.0, 10.0])
      , ("dialogue_vel", [0.0, 2.0, 4.0, 8.0])
      , ("dialogue_yield", [0.0, 1.0, 2.0, 4.0])
      , ("seq_echo", [0.0, 1.0, 2.0, 4.0])
      ]
    sections = Map.fromList
      [ ("vel_bar", "dynamics"), ("vel_halfbar", "dynamics")
      , ("vel_beat", "dynamics"), ("vel_arch", "dynamics")
      , ("vel_highloud", "dynamics")
      , ("dis_vel", "dissonance"), ("sus_soft", "dissonance")
      , ("mel_charge", "performance"), ("harm_charge", "performance")
      , ("subject_vel", "performance")
      , ("dialogue_vel", "performance")
      , ("dialogue_yield", "performance")
      , ("seq_echo", "performance")
      ]

-- ---------------------------------------------------------------------
-- configs

-- | The immutable pre-fit baseline: config text with generated
-- PIECE-FIT lines (inside [piece.*] sections) and the banner removed.
-- Global sections are never touched.
prefitStrip :: Text -> Text
prefitStrip = T.unlines . go False . T.lines
  where
    go _ [] = []
    go inPiece (l : ls)
      | isHeader l = l : go (isPieceHeader l) ls
      | inPiece && "# PIECE-FIT" `T.isInfixOf` l = go inPiece ls
      | "# ---- fitted per piece" `T.isPrefixOf` l = go inPiece ls
      | otherwise = l : go inPiece ls
    isHeader l =
      let s = T.strip (fst (T.breakOn "#" l))
       in "[" `T.isPrefixOf` s && "]" `T.isSuffixOf` s
    isPieceHeader l =
      let s = T.strip (fst (T.breakOn "#" l))
       in "[piece." `T.isPrefixOf` s

-- | Override knobs in their sections (later wins, like appended TOML).
-- When a piece name is given, everything lands in [piece.<name>]
-- instead — the per-piece fit's coordinate system.
applyKnobs :: KnobFamily -> Maybe Text -> [(Text, Double)] -> Config
           -> Config
applyKnobs fam mpiece knobs cfg = foldl' put cfg knobs
  where
    put c (k, v) =
      let sect = case mpiece of
            Just piece -> "piece." <> piece
            Nothing -> Map.findWithDefault "agogics" k (kfSections fam)
       in Map.insertWith Map.union sect (Map.singleton k v) c

-- ---------------------------------------------------------------------
-- per-piece data, loaded once per run

data PieceData = PieceData
  { pdPiece :: String
  , pdSource :: Text -- ^ kern source (edition-resolved by the caller)
  , pdAnnotations :: [(Text, [(String, Text)])]
  , pdMatches :: [(String, Text)]
  }

-- | wtc1f01 -> Bach/Fugue/bwv_846 (Nothing otherwise).
bwvSub :: String -> Maybe FilePath
bwvSub p = case p of
  ['w', 't', 'c', b, k, n1, n2]
    | b `elem` ("12" :: String), k `elem` ("pf" :: String)
    , all (`elem` ("0123456789" :: String)) [n1, n2] ->
      let num = read [n1, n2] :: Int
          bwv = (if b == '1' then 845 else 869) + num
          kind = if k == 'p' then "Prelude" else "Fugue"
       in Just ("Bach" </> kind </> ("bwv_" <> show bwv))
  _ -> Nothing

-- | Load a piece's human data across source roots. Nothing when no
-- source has anything for it.
loadPieceData :: [FilePath] -> String -> Text -> IO (Maybe PieceData)
loadPieceData roots piece src = case bwvSub piece of
  Nothing -> pure Nothing
  Just sub -> do
    anns <- mapM (\r -> loadDirAnnotations (r </> sub)) roots
    matches <- fmap concat . mapM (loadMatches . (</> sub)) $ roots
    if all (== Nothing) anns && null matches
      then pure Nothing
      else pure (Just (PieceData piece src [a | Just a <- anns] matches))
  where
    loadMatches d = do
      ok <- doesDirectoryExist d
      if not ok then pure [] else do
        fs <- sort <$> listDirectory d
        mapM (\f -> (,) (take (length f - 6) f)
                      <$> TIO.readFile (d </> f))
             [f | f <- fs, ".match" `isSuffixOfS` f]
    isSuffixOfS suf s = T.pack suf `T.isSuffixOf` T.pack s

-- ---------------------------------------------------------------------
-- objectives (per piece; aggregation split out so subsets and medians
-- compose)

-- | Per-performance beat-level tempo r for one performed piece.
timingRs :: Performance -> PieceData -> [(String, Double)]
timingRs p pd =
  [ (prPerformer x, prR x)
  | x <- concatMap (scorePerformances (perfTempoMap p))
           (pdAnnotations pd) ]

-- | Per-performance velocity r (>=30 bridged rows, ornaments included).
velocityRs :: Performance -> PieceData -> [(String, Double)]
velocityRs = velocityRsWith False

-- | The per-piece fit's variant: ornament rows excluded before the
-- 30-row floor (piece_fit's Evaluator.velocity), where the scoreboard
-- and the landscape keep them (vel_fit.piece_r's default).
velocityRsNoOrn :: Performance -> PieceData -> [(String, Double)]
velocityRsNoOrn = velocityRsWith True

velocityRsWith :: Bool -> Performance -> PieceData
               -> [(String, Double)]
velocityRsWith excludeOrn p pd =
  [ (perf, r)
  | (perf, mt) <- pdMatches pd
  , let (rows0, _) = B.bridge irn (B.parseMatch mt)
        rows = if excludeOrn
                 then [x | x <- rows0, not (B.brIsOrn x)]
                 else rows0
  , length rows >= 30
  , let r = maybe (0 / 0) id (pearson
              [fromIntegral (B.brOtbVel x) | x <- rows]
              [fromIntegral (B.brHumanVel x) | x <- rows])
  ]
  where irn = B.loadIrNotes p

meanFlat :: [[Double]] -> Maybe Double
meanFlat rss =
  let xs = [r | rs <- rss, r <- rs, not (isNaN r)]
   in if null xs then Nothing
      else Just (sum xs / fromIntegral (length xs))

meanOfPieceMedians :: [[Double]] -> Maybe Double
meanOfPieceMedians rss =
  let ms = [ medianD (sort ok)
           | rs <- rss
           , let ok = [r | r <- rs, not (isNaN r)]
           , not (null ok) ]
   in if null ms then Nothing
      else Just (sum ms / fromIntegral (length ms))

medianD :: [Double] -> Double
medianD ms =
  let n = length ms
   in if even n
        then (ms !! (n `div` 2 - 1) + ms !! (n `div` 2)) / 2
        else ms !! (n `div` 2)

-- ---------------------------------------------------------------------
-- a tiny deterministic PRNG (splitmix64 core): reproducible without a
-- dependency

newtype Rng = Rng Word64 deriving Show

mkRng :: Int -> Rng
mkRng s = Rng (fromIntegral s * 0x9E3779B97F4A7C15 + 1)

nextInt :: Rng -> (Int, Rng)
nextInt (Rng s0) =
  let s = s0 + 0x9E3779B97F4A7C15
      z0 = (s `xor` (s `shiftR` 30)) * 0xBF58476D1CE4E5B9
      z1 = (z0 `xor` (z0 `shiftR` 27)) * 0x94D049BB133111EB
      z = z1 `xor` (z1 `shiftR` 31)
   in (fromIntegral (z `shiftR` 1), Rng s)

shuffle :: Rng -> [a] -> ([a], Rng)
shuffle rng0 xs0 = go rng0 xs0 []
  where
    go rng [] acc = (acc, rng)
    go rng xs acc =
      let (i, rng') = nextInt rng
          j = i `mod` length xs
          x = xs !! j
       in go rng' (take j xs <> drop (j + 1) xs) (x : acc)

choice :: Rng -> [a] -> (a, Rng)
choice rng xs =
  let (i, rng') = nextInt rng
   in (xs !! (i `mod` length xs), rng')

-- ---------------------------------------------------------------------
-- coordinate descent to a sweep fixpoint

-- | Randomized-order coordinate descent. The objective is total (may
-- return Nothing = unevaluable); improvement threshold 1e-4 mirrors
-- the retired rigs.
descend
  :: ([(Text, Double)] -> Maybe Double)
  -> [(Text, [Double])]
  -> Rng
  -> [(Text, Double)]
  -> Int
  -> (Maybe ([(Text, Double)], Double, Int), Rng)
descend objective grids rng0 init0 maxSweeps =
  case objective init0 of
    Nothing -> (Nothing, rng0)
    Just r0 -> go rng0 (Map.fromList init0) r0 1
  where
    go rng cur best sweep
      | sweep > maxSweeps = (done cur best (sweep - 1), rng)
      | otherwise =
          let (order, rng') = shuffle rng (map fst grids)
              ((cur', best', improved), rng'') =
                foldl' step ((cur, best, False), rng') order
           in if improved
                then go rng'' cur' best' (sweep + 1)
                else (done cur' best' sweep, rng'')
    step ((cur, best, imp), rng) k =
      let grid = maybe [] id (lookup k grids)
          try (c, b, i) v
            | Just v == Map.lookup k c = (c, b, i)
            | otherwise =
                let cand = Map.insert k v c
                 in case objective (Map.toList cand) of
                      Just r | r > b + 1e-4 -> (cand, r, True)
                      _ -> (c, b, i)
       in (foldl' try (cur, best, imp) grid, rng)
    done cur best sweeps = Just (Map.toList cur, best, sweeps)
