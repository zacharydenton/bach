-- | otb — the One-Take Bach interpretation compiler.
--
--   otb compile SCORE.krn -o out.mid [--emit-json ..] [--emit-scl ..]
--   otb explain SCORE.krn [--bar N | --ch C --note I]
--
-- (bare @otb SCORE.krn ...@ still works: compile is the default command.)
-- The piece name for per-piece config overrides is the input basename
-- (wtc1p01.krn -> [piece.wtc1p01]).
--
-- License: GPL-2.0-or-later.
module Main (main) where

import Control.DeepSeq (force)
import Control.Concurrent
  ( MVar, forkIO, getNumCapabilities, modifyMVar, newEmptyMVar, newMVar
  , putMVar, readMVar, takeMVar )
import Control.Concurrent.QSem (QSem, newQSem, signalQSem, waitQSem)
import Control.Exception
  (SomeException, bracket_, evaluate, throwIO, try)
import Control.Monad (foldM, forM, forM_, unless, void, when, (<=<))
import Control.Parallel.Strategies (parMap, rdeepseq)
import Data.ByteString.Lazy qualified as BL
import Data.List (foldl', isSuffixOf, sort, sortOn)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import OTB.Config
  ( agogicsFor, artParamsFor, dynamicsFor, expressFor, emptyConfig, loadConfig
  , ornamentsFor, phrasingFor, pieceTempo, tuningBendRange )
import OTB.Config qualified
import OTB.Edition (editionsFor, readKernSource)
import OTB.Eval
  ( PerfR (..), loadDirAnnotations, pearson, perfBeatTimes
  , scoreBeatPositions, scorePerformances )
import OTB.Bridge qualified as B
import OTB.Fit qualified as F
import OTB.PieceFit qualified as PF
import OTB.Maestro qualified as M
import OTB.BakeSite qualified as BS
import OTB.Json qualified as J
import OTB.MaestroAlign qualified as MA
import OTB.Smf (readSmf)
import Data.Vector qualified as BV
import Data.IORef
  (atomicModifyIORef', modifyIORef', newIORef, readIORef)
import OTB.Emit.Midi (renderSmf, writeSmf)
import OTB.Generate (generateScore)
import Data.List (intercalate)
import Data.Map.Strict qualified as Map
import Numeric (showFFloat)
import OTB.Analysis.Grouping (groupSpans)
import OTB.Analysis.Harmony (Harmony (..), analyzeHarmony)
import OTB.Analysis.Imitation (Imitation (..), findImitation)
import OTB.Analysis.Parallelism (Sequences (..), findSequences)
import OTB.Analysis.Subject (subjectEntries)
import OTB.Interp.Express (chargesForLane)
import OTB.Interp.Phrasing (boundaryStrengths)

import OTB.Kern.Token (Mark (..))
import OTB.Emit.Json (renderJson)
import OTB.Explain (renderWhys)
import OTB.Instrument (hardwareTracks)
import OTB.Interp.Agogics (defaultAgogicParams)
import OTB.Interp.Dynamics (defaultDynParams)
import OTB.Interp.Express (defaultExpressParams)
import OTB.Interp.Ornament (defaultOrnamentParams)
import OTB.Interp.Phrasing (defaultPhraseParams)
import OTB.Kern.Parser (parseKern)
import OTB.Player
  ( Interp (..), PerfNote (..), Performance (..), Structure (..)
  , analyzeStructure, defaultInterp, perform )
import OTB.TempoGiusto (tempoGiusto)
import OTB.Score (Score (..), ScoreNote (..), Voice (..))
import OTB.Tuning (TuningTable, equalTable, parseScl, renderScl, werckmeister3)
import OTB.Units (Bpm (..), WholeNotes (..))
import Options.Applicative
import System.Directory
  ( createDirectoryIfMissing, doesDirectoryExist, doesFileExist
  , getHomeDirectory, listDirectory, renameFile )
import System.Environment (getExecutablePath, lookupEnv)
import System.Exit (die)
import System.Info (arch, compilerName, fullCompilerVersion, os)
import Data.Version (showVersion)
import System.IO (hPutStrLn, stderr)
import System.Process (readProcess, readProcessWithExitCode)
import System.FilePath (takeBaseName, takeFileName, (</>))

data Common = Common
  { cInput :: FilePath
  , cConfig :: FilePath
  , cTempo :: TempoOpt
  , cTemperament :: String
  , cEditions :: Maybe FilePath
  }

-- | @--tempo@: a number is the fallback for scores with no @*MM@;
-- @giusto@ FORCES the notation-derived tempo (Kirnberger/Quantz) even
-- over a declared one — useful against the corpus's 72-BPM encoder
-- placeholders. With no flag at all, giusto is the fallback.
data TempoOpt = TempoDefault | TempoGiusto | TempoBpm Double

data Cmd
  = Compile Common FilePath (Maybe FilePath) (Maybe FilePath) String
  | Explain Common (Maybe Int) (Maybe (Int, Int))
  | Album FilePath FilePath FilePath TempoOpt String (Maybe FilePath)
  | Eval FilePath FilePath TempoOpt String (Maybe FilePath) [FilePath] [String] Bool
  | BridgeDump FilePath FilePath FilePath
  | Landscape FilePath FilePath String Int Int Double Double Double
      (Maybe FilePath)
  | Fit FilePath FilePath Double [String] Bool Bool
  | MaestroFetch Bool
  | MaestroAlign Bool (Maybe String) (Maybe Int)
  | BakeSite BS.BakeOpts
  | Stats FilePath FilePath Double
  | Ground String Int Double String FilePath (Maybe FilePath) (Maybe FilePath)
  | Analyze Common

common :: Parser Common
common =
  Common
    <$> argument str (metavar "SCORE.krn")
    <*> strOption (long "config" <> metavar "RULES.toml"
          <> value "config/default.toml"
          <> help "interpretation rules + per-piece override log")
    <*> option (eitherReader tempoR)
          (long "tempo" <> metavar "BPM|giusto" <> value TempoDefault
             <> help ("fallback tempo when the score has no *MM "
                        <> "(default: tempo giusto, derived from the "
                        <> "notation); 'giusto' forces the derivation "
                        <> "even over a declared *MM"))
    <*> strOption (long "temperament" <> metavar "NAME|FILE.scl"
          <> value "werckmeister3"
          <> help "werckmeister3 (default), equal, or a Scala .scl file")
    <*> optional (strOption (long "editions" <> metavar "DIR"
          <> help ("edition-override directory (default: editions/ "
                     <> "next to the config file)")))

compileCmd :: Parser Cmd
compileCmd =
  Compile
    <$> common
    <*> strOption (long "output" <> short 'o' <> metavar "OUT.mid"
          <> value "out.mid")
    <*> optional (strOption (long "emit-scl" <> metavar "OUT.scl"
          <> help "also write the temperament as .scl (Surge native tuning)"))
    <*> optional (strOption (long "emit-json" <> metavar "OUT.json"
          <> help "also write PerformanceIR as JSON (the renderer seam)"))
    <*> option (eitherReader targetR)
          (long "target" <> metavar "surge|hardware" <> value "surge"
             <> help "hardware remaps lanes onto the rig (A4 x4, Model D, BS2) with capabilities enforced")
  where
    targetR t
      | t `elem` ["surge", "hardware"] = Right t
      | otherwise = Left ("unknown target '" <> t <> "' (surge|hardware)")

explainCmd :: Parser Cmd
explainCmd =
  Explain
    <$> common
    <*> optional (option auto (long "bar" <> metavar "N"
          <> help "explain every note in bar N (1-based)"))
    <*> optional
          ((,) <$> option auto (long "ch" <> metavar "C")
               <*> option auto (long "note" <> metavar "I"))

fitCmd :: Parser Cmd
fitCmd =
  Fit
    <$> argument str (metavar "CORPUS_DIR")
    <*> strOption (long "config" <> value "config/default.toml")
    <*> option auto (long "shrink-k" <> value 2.0)
    <*> many (strOption (long "piece" <> metavar "NAME"))
    <*> switch (long "apply"
          <> help "write the fitted [piece.X] sections into the config")
    <*> switch (long "dry-run"
          <> help "print the config diff instead of writing")

bakeSiteCmd :: Parser Cmd
bakeSiteCmd = fmap BakeSite $
  BS.BakeOpts
    <$> strOption (long "site" <> value "site")
    <*> strOption (long "surge-dir" <> value ""
          <> help "surge fork checkout (default: $SURGE_DIR or ~/code/surge)")
    <*> strOption (long "emscripten-bin" <> value "/usr/lib/emscripten"
          <> help "prepended to PATH for emcmake (empty to skip)")
    <*> optional (strOption (long "perf-dir"
          <> help "copy IRs from here instead of running the album"))
    <*> strOption (long "casting-dir"
          <> value ("config" </> "casting"))
    <*> strOption (long "calibration"
          <> value ("config" </> "calibration.json"))
    <*> switch (long "skip-wasm")
    <*> switch (long "skip-perf")
    <*> switch (long "skip-patches")

maestroFetchCmd :: Parser Cmd
maestroFetchCmd =
  MaestroFetch
    <$> switch (long "catalog"
          <> help "rebuild the catalog from cached files only")

maestroAlignCmd :: Parser Cmd
maestroAlignCmd =
  MaestroAlign
    <$> switch (long "validate"
          <> help "score the aligner against ASAP ground truth")
    <*> optional (strOption (long "piece" <> metavar "NAME"))
    <*> optional (option auto (long "limit" <> metavar "N"))

famR :: String -> Either String String
famR f
  | f `elem` ["timing", "velocity"] = Right f
  | otherwise = Left ("unknown family '" <> f
                        <> "' (timing | velocity)")

landscapeCmd :: Parser Cmd
landscapeCmd =
  Landscape
    <$> argument str (metavar "CORPUS_DIR")
    <*> strOption (long "config" <> value "config/default.toml")
    <*> option (eitherReader famR) (long "family" <> value "timing"
          <> help "timing | velocity")
    <*> option auto (long "starts" <> value 20)
    <*> option auto (long "seed" <> value 1)
    <*> option auto (long "slack" <> value 0.005)
    <*> option auto (long "zero-floor" <> value 0.0 <> metavar "FRAC"
          <> help ("positive-contribution condition: replace each "
                     <> "grid's 0 with FRAC x its smallest nonzero "
                     <> "value (0 = unconstrained, zeros allowed)"))
    <*> option auto (long "diversity-bonus" <> value 0.0
          <> metavar "LAMBDA"
          <> help ("regularized condition: descend on r + LAMBDA x "
                     <> "nonzero-fraction; the reported r stays raw"))
    <*> optional (strOption (long "emit-elite" <> metavar "DIR"
          <> help ("write each elite final as a runnable config for "
                     <> "perceptual A/B (prefit + knob overrides)")))

bridgeDumpCmd :: Parser Cmd
bridgeDumpCmd =
  BridgeDump
    <$> argument str (metavar "SCORE.krn")
    <*> argument str (metavar "PERF.match")
    <*> strOption (long "config" <> value "config/default.toml")

evalCmd :: Parser Cmd
evalCmd =
  Eval
    <$> argument str (metavar "CORPUS_DIR")
    <*> strOption (long "config" <> value "config/default.toml")
    <*> option (eitherReader tempoR)
          (long "tempo" <> metavar "BPM|giusto" <> value TempoDefault)
    <*> strOption (long "temperament" <> value "werckmeister3")
    <*> optional (strOption (long "editions" <> metavar "DIR"))
    <*> many (strOption (long "root" <> metavar "DIR"
          <> help ("performance source root (repeatable; default: "
                     <> "corpus/asap and corpus/maestro-wtc)")))
    <*> many (strOption (long "piece" <> metavar "NAME"
          <> help "restrict to these pieces (repeatable)"))
    <*> switch (long "velocity"
          <> help ("note-level velocity r through the bridge instead "
                     <> "of beat-level tempo r"))

albumCmd :: Parser Cmd
albumCmd =
  Album
    <$> argument str (metavar "CORPUS_DIR")
    <*> argument str (metavar "OUT_DIR")
    <*> strOption (long "config" <> value "config/default.toml")
    <*> option (eitherReader tempoR)
          (long "tempo" <> metavar "BPM|giusto" <> value TempoDefault)
    <*> strOption (long "temperament" <> value "werckmeister3")
    <*> optional (strOption (long "editions" <> metavar "DIR"))

groundCmd :: Parser Cmd
groundCmd =
  Ground
    <$> strOption (long "bass" <> metavar "NAME"
          <> help "folia | romanesca | pachelbel")
    <*> option auto (long "variations" <> value 6)
    <*> option auto (long "tempo" <> value 96)
    <*> strOption (long "temperament" <> value "werckmeister3")
    <*> strOption (long "output" <> short 'o' <> value "ground.mid")
    <*> optional (strOption (long "emit-scl" <> metavar "OUT.scl"))
    <*> optional (strOption (long "emit-json" <> metavar "OUT.json"))

analyzeCmd :: Parser Cmd
analyzeCmd = Analyze <$> common

statsCmd :: Parser Cmd
statsCmd =
  Stats
    <$> argument str (metavar "CORPUS_DIR")
    <*> strOption (long "config" <> value "config/default.toml")
    <*> option auto (long "tempo" <> value 72)

cmdP :: Parser Cmd
cmdP =
  hsubparser
    ( command "compile"
        (info compileCmd (progDesc "score -> performed MIDI"))
        <> command "explain"
             (info explainCmd
                (progDesc "per-note rule traces with citations"))
        <> command "album"
             (info albumCmd
                (progDesc "compile a corpus directory in parallel"))
        <> command "eval"
             (info evalCmd
                (progDesc ("beat-level tempo r vs human performances, "
                             <> "whole corpus in one process")))
        <> command "fit"
             (info fitCmd
                (progDesc ("per-piece hierarchical fits: tempo, "
                             <> "timing and velocity, LOPO-guarded")))
        <> command "bake-site"
             (info bakeSiteCmd
                (progDesc "bake the static patchboard into site/"))
        <> command "maestro-fetch"
             (info maestroFetchCmd
                (progDesc "fetch the MAESTRO v3 archive + WTC catalog"))
        <> command "maestro-align"
             (info maestroAlignCmd
                (progDesc ("align MAESTRO WTC performances to otb "
                             <> "scores -> corpus/maestro-wtc")))
        <> command "landscape"
             (info landscapeCmd
                (progDesc ("multi-start knob-landscape search: are "
                             <> "the zeros real?")))
        <> command "bridge-dump"
             (info bridgeDumpCmd
                (progDesc ("canonical TSV of the note bridge's rows — "
                             <> "the porting parity gate")))
        <> command "stats"
             (info statsCmd
                (progDesc "whole-corpus interpretation statistics"))
        <> command "ground"
             (info groundCmd
                (progDesc "generate a ground-bass variation set (pole two)"))
        <> command "analyze"
             (info analyzeCmd
                (progDesc "the piece's structure as JSON (boundaries, tree, cadences, subject)"))
    )
    <|> compileCmd -- bare invocation = compile

main :: IO ()
main = do
  c <- execParser (info (cmdP <**> helper)
        (fullDesc <> progDesc "One-Take Bach interpretation compiler"))
  case c of
    Compile com out mscl mjson tgt -> runCompile com out mscl mjson tgt
    Explain com mbar mnote -> runExplain com mbar mnote
    Album corpus outDir cfgPath tempo temp eds -> runAlbum corpus outDir cfgPath tempo temp eds
    Eval corpus cfgPath tempo temp eds roots ps vel ->
      runEval corpus cfgPath tempo temp eds roots ps vel
    BridgeDump krn match cfgPath -> runBridgeDump krn match cfgPath
    Landscape corpus cfgPath fam st sd sl zf db ee ->
      runLandscape corpus cfgPath fam st sd sl zf db ee
    Fit corpus cfgPath sk ps ap dr -> runFit corpus cfgPath sk ps ap dr
    MaestroFetch catOnly -> M.runMaestroFetch catOnly
    BakeSite opts0 -> do
      opts <- if null (BS.boSurgeDir opts0)
        then do
          menv <- lookupEnv "SURGE_DIR"
          home <- getHomeDirectory
          pure opts0 {BS.boSurgeDir =
            maybe (home </> "code" </> "surge") id menv}
        else pure opts0
      BS.runBakeSite
        (\stage -> runAlbum ("corpus" </> "bach-wtc" </> "kern") stage
           "config/default.toml" TempoDefault "werckmeister3" Nothing)
        opts
    MaestroAlign validate mp ml -> runMaestroAlign validate mp ml
    Stats corpus cfgPath tempo -> runStats corpus cfgPath tempo
    Ground bass nvar tempo temp out mscl mjson ->
      runGround bass nvar tempo temp out mscl mjson
    Analyze com -> runAnalyze com

-- | Every tempo that enters the pipeline — CLI or per-piece config
-- override — passes through here; a non-finite or non-positive BPM
-- would emit a nonsense FF 51 tempo meta (or divide by zero).
badBpm :: Double -> Maybe String
badBpm b
  | isNaN b || isInfinite b || b <= 0 =
      Just ("tempo must be a finite number > 0, got " <> show b)
  | otherwise = Nothing

checkBpm :: String -> Double -> IO ()
checkBpm ctx b = mapM_ (die . ((ctx <> ": ") <>)) (badBpm b)

tempoR :: String -> Either String TempoOpt
tempoR "giusto" = Right TempoGiusto
tempoR s = case reads s of
  [(b, "")] -> maybe (Right (TempoBpm b)) Left (badBpm b)
  _ -> Left ("expected a BPM number or 'giusto', got '" <> s <> "'")

-- | Parse-time fallback for the option (giusto lands post-parse).
tempoFallback :: TempoOpt -> Bpm
tempoFallback (TempoBpm b) = Bpm b
tempoFallback _ = Bpm 72

-- | Resolve the option against what the score declared — shared by
-- every compile path (single-piece and album alike).
applyTempoOpt :: TempoOpt -> Score -> Score
applyTempoOpt opt s = case opt of
  TempoGiusto -> s {scTempo = tempoGiusto s}
  TempoBpm _ -> s
  TempoDefault
    | scTempoDeclared s -> s
    | otherwise -> s {scTempo = tempoGiusto s}

load :: Common -> IO (String, Score, Performance, TuningTable, Bool, Interp)
load com = do
  let eds = maybe (editionsFor (cConfig com)) id (cEditions com)
  src <- readKernSource eds (cInput com)
  score0 <- either (die . ("parse: " <>)) pure
              (parseKern (tempoFallback (cTempo com)) src)
  haveCfg <- doesFileExist (cConfig com)
  cfg <- if haveCfg
           then either (die . (("config " <> cConfig com <> ": ") <>)) pure
                  . loadConfig =<< TIO.readFile (cConfig com)
           else pure emptyConfig
  let adaptive = cTemperament com == "adaptive"
  table <- resolveTemperament
             (if adaptive then "werckmeister3" else cTemperament com)
  let piece = T.pack (takeBaseName (cInput com))
      -- tempo authority, outermost last: notation-derived giusto when
      -- nothing better is known, the score's own *MM, an explicit
      -- --tempo BPM as fallback, --tempo giusto forcing the derivation,
      -- and the per-piece config override (the exception log) over all
      score1 = applyTempoOpt (cTempo com) score0
      score = maybe score1 (\bpm -> score1 {scTempo = Bpm bpm})
                (pieceTempo cfg piece)
  mapM_ (checkBpm ("config tempo for " <> T.unpack piece))
    (pieceTempo cfg piece)
  let interp = Interp
        { iArt = artParamsFor cfg piece
        , iAgogics = agogicsFor cfg piece defaultAgogicParams
        , iPhrasing = phrasingFor cfg piece defaultPhraseParams
        , iOrnaments = ornamentsFor cfg piece defaultOrnamentParams
        , iDynamics = dynamicsFor cfg piece defaultDynParams
        , iExpress = expressFor cfg piece defaultExpressParams
        , iPiece = T.unpack piece
        , iAdaptive = adaptive
        , iTuning = table
        , iBendRange = tuningBendRange cfg
        }
  p <- either (die . ("perform: " <>)) pure (perform interp score)
  pure (T.unpack piece, score, p, table, haveCfg, interp)

runCompile :: Common -> FilePath -> Maybe FilePath -> Maybe FilePath -> String -> IO ()
runCompile com out mscl mjson tgt = do
  when (cTemperament com == "adaptive" && mscl /= Nothing)
    (die ("adaptive temperament is bend-carried per chord; no static "
            <> ".scl can express it — drop --emit-scl"))
  (piece, score, p0, table, haveCfg, _) <- load com
  (p, hwClips) <- case tgt of
    "hardware" -> either die pure (hardwareTracks p0)
    _ -> pure (p0, 0)
  writeSmf out p
  mapM_ (\sp -> TIO.writeFile sp
           (renderScl (T.pack (cTemperament com)) table)) mscl
  mapM_ (\jp -> writeFile jp (renderJson piece p)) mjson
  let Bpm bpm = scTempo score
  putStrLn $
    "voices " <> show (length (scVoices score))
      <> " | notes " <> show (sum (map (length . vNotes) (scVoices score)))
      <> " | tempo " <> show bpm
      <> " | piece " <> piece
      <> (if haveCfg then "" else " | WARN no config file, defaults only")
      <> (if scTieLeftovers score > 0
            then " | WARN tie-leftovers " <> show (scTieLeftovers score)
            else "")
      <> (if scMergeDrifts score > 0
            then " | WARN merge-drifts " <> show (scMergeDrifts score)
            else "")
      <> (if scGraceDropped score > 0
            then " | WARN grace-notes-dropped " <> show (scGraceDropped score)
            else "")
      <> (if hwClips > 0
            then " | hardware mono-reduction clipped " <> show hwClips
            else "")
      <> " | " <> cTemperament com
      <> " | -> " <> out

runExplain :: Common -> Maybe Int -> Maybe (Int, Int) -> IO ()
runExplain com mbar mnote = do
  (piece, score, p, _, _, _) <- load com
  let whys = perfWhys p
      notes = sortOn pnSrcOn (concat (perfTracks p))
      -- bar N's notated span, walked through the FULL meter map (meter
      -- changes shift every later barline); selection is by pnSrcOn —
      -- melody lead and jitter move pnOnset across barlines. The grid
      -- anchors at the first meter entry's onset (the parser moves it
      -- to the pickup's end when the piece opens with an anacrusis);
      -- bar 0 names the pickup itself.
      gridStart = case scMeter score of
        ((o, _) : _) -> o
        [] -> 0
      barSpan b
        | b < 1 = (0, gridStart)
        | otherwise = walk 1 gridStart (case scMeter score of
                                          [] -> [(0, (4, 4))]
                                          ms -> ms)
        where
          walk k t ((_, (n, d)) : more@((next, _) : _)) =
            let bl = WholeNotes (fromIntegral n / fromIntegral d)
                WholeNotes spanR = next - t
                WholeNotes blR = bl
                barsHere = max 0 (floor (spanR / blR)) :: Int
             in if b < k + barsHere
                  then let lo = t + fromIntegral (b - k) * bl in (lo, lo + bl)
                  else walk (k + barsHere) next more
          walk k t [(_, (n, d))] =
            let bl = WholeNotes (fromIntegral n / fromIntegral d)
                lo = t + fromIntegral (b - k) * bl
             in (lo, lo + bl)
          walk _ _ [] = (0, 0)
      inBar b n =
        let (lo, hi) = barSpan b in lo <= pnSrcOn n && pnSrcOn n < hi
      selected = case (mbar, mnote) of
        (Just b, _) -> filter (inBar b) notes
        (_, Just (ch, i)) ->
          filter (\n -> pnChannel n == ch && pnIndex n == i) notes
        _ -> take 8 notes
  putStrLn ("== " <> piece <> maybe "" ((" bar " <>) . show) mbar <> " ==")
  mapM_ (explainNote whys) selected
  where
    explainNote whys n = do
      let WholeNotes t = pnOnset n
          ws = fromMaybe [] (lookup (pnChannel n, pnIndex n) whys)
      putStrLn ""
      putStrLn $
        "note ch" <> show (pnChannel n) <> " #" <> show (pnIndex n)
          <> "  pitch " <> show (pnPitch n)
          <> "  t=" <> show (fromRational t :: Double) <> "wn"
          <> "  vel " <> show (pnVel n)
          <> "  bend " <> show (pnBend n)
      putStrLn (renderWhys ws)

resolveTemperament :: String -> IO TuningTable
resolveTemperament name = case name of
  "werckmeister3" -> pure werckmeister3
  "equal" -> pure equalTable
  path -> do
    src <- TIO.readFile path
    either (die . (("scl " <> path <> ": ") <>)) pure (parseScl src)

-- ---------------------------------------------------------------------
-- W6: the album, in parallel; the corpus, in numbers

loadCfg :: FilePath -> IO OTB.Config.Config
loadCfg cfgPath = do
  haveCfg <- doesFileExist cfgPath
  if haveCfg
    then either (die . (("config " <> cfgPath <> ": ") <>)) pure
           . loadConfig =<< TIO.readFile cfgPath
    else pure emptyConfig

mkInterp :: OTB.Config.Config -> TuningTable -> Bool -> String -> Interp
mkInterp cfg table adaptive piece0 =
  let piece = T.pack piece0
   in Interp
        { iArt = artParamsFor cfg piece
        , iAgogics = agogicsFor cfg piece defaultAgogicParams
        , iPhrasing = phrasingFor cfg piece defaultPhraseParams
        , iOrnaments = ornamentsFor cfg piece defaultOrnamentParams
        , iDynamics = dynamicsFor cfg piece defaultDynParams
        , iExpress = expressFor cfg piece defaultExpressParams
        , iPiece = piece0
        , iAdaptive = adaptive
        , iTuning = table
        , iBendRange = tuningBendRange cfg
        }

runAlbum :: FilePath -> FilePath -> FilePath -> TempoOpt -> String
         -> Maybe FilePath -> IO ()
runAlbum corpus outDir cfgPath tempo temp meds = do
  let eds = maybe (editionsFor cfgPath) id meds
  cfg <- loadCfg cfgPath
  let adaptive = temp == "adaptive"
  table <- resolveTemperament (if adaptive then "werckmeister3" else temp)
  files <- filter (isSuffixOf ".krn") <$> listDirectory corpus
  srcs <- mapM (\f -> (,) f <$> readKernSource eds (corpus </> f)) (sort files)
  createDirectoryIfMissing True outDir
  -- the pure pipeline fans out across cores; IO stays sequential
  let one (f, src) =
        let piece = takeBaseName f
            r = do
              s0 <- parseKern (tempoFallback tempo) src
              -- the same tempo authority as the single-piece path:
              -- giusto for undeclared scores, --tempo giusto forcing
              let s1 = applyTempoOpt tempo s0
                  s = maybe s1 (\b -> s1 {scTempo = Bpm b})
                        (pieceTempo cfg (T.pack piece))
              mapM_ (Left . ("config tempo: " <>))
                (badBpm . (\(Bpm b) -> b) . scTempo $ s)
              p <- perform (mkInterp cfg table adaptive piece) s
              pure ( force (renderSmf p)
                   , force (renderJson piece p) )
         in (piece, r)
      results = parMap rdeepseq one srcs
  forM_ results $ \(piece, r) -> case r of
    Left e -> putStrLn ("FAIL " <> piece <> ": " <> e)
    Right (smf, json) -> do
      BL.writeFile (outDir </> piece <> ".mid") smf
      writeFile (outDir </> piece <> ".json") json
  when (not adaptive) $
    TIO.writeFile (outDir </> "w3.scl") (renderScl (T.pack temp) table)
  let failed = length [() | (_, Left _) <- results]
      ok = length results - failed
  putStrLn (show ok <> " pieces -> " <> outDir
              <> (if failed > 0 then " | " <> show failed <> " FAILED"
                    else ""))
  when (failed > 0)
    (die (show failed <> " of " <> show (length results)
            <> " pieces failed; artifacts above are incomplete"))

-- | The hot loop of every fitting campaign, in one process: compile
-- each piece (pure, in parallel) and correlate its tempo curve with
-- every human performance's beat annotations. Output is TSV
-- (piece, performer, r, beats) plus a grand-mean footer — the Python
-- rigs parse it instead of orchestrating one subprocess per piece.
runEval :: FilePath -> FilePath -> TempoOpt -> String -> Maybe FilePath
        -> [FilePath] -> [String] -> Bool -> IO ()
runEval corpus cfgPath tempo temp meds roots0 wanted velocity = do
  cfg <- loadCfg cfgPath
  let adaptive = temp == "adaptive"
  table <- resolveTemperament (if adaptive then "werckmeister3" else temp)
  let eds = maybe (editionsFor cfgPath) id meds
      roots = if null roots0
                then ["corpus" </> "asap", "corpus" </> "maestro-wtc"]
                else roots0
  files <- filter (isSuffixOf ".krn") <$> listDirectory corpus
  let pieces0 = sort (map takeBaseName files)
      pieces = if null wanted then pieces0
               else filter (`elem` wanted) pieces0
  inputs <- fmap concat . forM pieces $ \piece ->
    case bwvDir piece of
      Nothing -> pure []
      Just sub -> do
        anns <- mapM (\r -> loadDirAnnotations (r </> sub)) roots
        matches <- fmap concat . forM roots $ \r -> do
          let d = r </> sub
          ok <- doesDirectoryExist d
          if not ok then pure [] else do
            fs <- sort <$> listDirectory d
            forM [f | f <- fs, ".match" `isSuffixOf` f] $ \f -> do
              t <- TIO.readFile (d </> f)
              pure (take (length f - 6) f, t)
        if all (== Nothing) anns && null matches
          then pure []
          else do
            src <- readKernSource eds (corpus </> piece <> ".krn")
            pure [(piece, src, [a | Just a <- anns], matches)]
  let pearsonD xs ys = maybe (0 / 0) id (pearson xs ys)
      velRs p matches =
        let irn = B.loadIrNotes p
         in [ (perf, r, length rows)
            | (perf, mt) <- matches
            , let (rows, _) = B.bridge irn (B.parseMatch mt)
            , length rows >= 30
            , let r = pearsonD [fromIntegral (B.brOtbVel x) | x <- rows]
                              [fromIntegral (B.brHumanVel x) | x <- rows]
            ]
      one (piece, src, anns, matches) =
        let r = do
              s0 <- parseKern (tempoFallback tempo) src
              let s1 = applyTempoOpt tempo s0
                  s = maybe s1 (\b -> s1 {scTempo = Bpm b})
                        (pieceTempo cfg (T.pack piece))
              p <- perform (mkInterp cfg table adaptive piece) s
              pure $ if velocity
                then velRs p matches
                else [ (prPerformer x, prR x, prBeats x)
                     | x <- concatMap
                         (scorePerformances (perfTempoMap p)) anns ]
         in (piece, either (const []) id r)
      results = parMap rdeepseq one inputs
  forM_ results $ \(piece, rs) ->
    forM_ rs $ \(perf, r, nb) ->
      putStrLn (piece <> "\t" <> perf <> "\t" <> show r
                  <> "\t" <> show nb)
  if velocity
    then do
      let medianD ms =
            let n = length ms
             in if even n
                  then (ms !! (n `div` 2 - 1) + ms !! (n `div` 2)) / 2
                  else ms !! (n `div` 2)
          medians =
            [ medianD ms
            | (_, rs) <- results
            , let ms = sort [r | (_, r, _) <- rs, not (isNaN r)]
            , not (null ms) ]
      putStrLn ("# " <> show (length medians)
                  <> " pieces, grand mean velocity r = "
                  <> show (sum medians
                             / fromIntegral (max 1 (length medians))))
    else do
      let allRs = [r | (_, rs) <- results, (_, r, _) <- rs, not (isNaN r)]
      putStrLn ("# grand mean r = "
                  <> show (sum allRs / fromIntegral (max 1 (length allRs)))
                  <> " over " <> show (length allRs) <> " performances")

-- | The MAESTRO aligner's IO orchestration (@otb maestro-align@):
-- catalog in, aligned mirror tree (or validation verdicts) out.
runMaestroAlign :: Bool -> Maybe String -> Maybe Int -> IO ()
runMaestroAlign validate mpiece mlimit = do
  catalog0 <- M.readCatalog
  let catalog1 = maybe catalog0
        (\p -> filter ((p `elem`) . M.ceCandidates) catalog0) mpiece
      catalog = maybe catalog1 (`take` catalog1) mlimit
  perfMap <- if validate then asapPerfMap else pure Map.empty
  cfg <- loadCfg cfgPath
  table <- resolveTemperament "werckmeister3"
  irCache <- newIORef
    (Map.empty :: Map.Map String (Maybe [(Int, [(Int, Int)])]))
  seqRef <- newIORef (Map.empty :: Map.Map String Int)
  verdictsRef <- newIORef ([] :: [(String, String, MA.Verdict)])
  forM_ catalog $ \entry -> do
    let rel = T.unpack (M.ceMidi entry)
    notes <- readSmf (M.maestroDir </> rel)
    let notesV = BV.fromList notes
    forM_ (M.ceCandidates entry) $ \piece -> do
      let (kind, bwv) = bwvOf piece
          folder = T.pack ("Bach/" <> kind <> "/" <> bwv)
          skip
            | maybe False (/= piece) mpiece = True
            | validate = folder `notElem` M.ceAsapFolders entry
            | otherwise = folder `elem` M.ceAsapFolders entry
      unless skip $ do
        msc <- cachedChords cfg table irCache piece
        forM_ (maybe [] (maybe [] pure . flip MA.alignOne notesV) msc)
          $ \res -> do
          let tag = piece <> " <- " <> take 40 (takeFileName rel)
          if MA.aoReject res
            then putStrLn ("REJECT " <> tag <> " "
                             <> statsLine (MA.aoStats res))
            else if validate
              then forM_ (Map.findWithDefault [] (M.ceMidi entry)
                            perfMap) $ \(f_, perf) ->
                when (f_ == folder) $ do
                  mv <- validateIO res notesV folder perf
                  forM_ mv $ \v -> do
                    modifyIORef' verdictsRef ((piece, perf, v) :)
                    putStrLn ("VALID  " <> tag <> " vs " <> perf
                      <> ": agree "
                      <> showFFloat (Just 3) (MA.vAgree v) ""
                      <> " beat|D| "
                      <> maybe "-" MA.pyRepr (MA.vBeatMs v)
                      <> " ms (rate "
                      <> MA.pyRepr (MA.stRate (MA.aoStats res))
                      <> ")")
              else do
                let outdir = "corpus" </> "maestro-wtc" </> "Bach"
                               </> kind </> bwv
                createDirectoryIfMissing True outdir
                n0 <- Map.findWithDefault 0 piece
                        <$> readIORef seqRef
                let n1 = n0 + 1
                modifyIORef' seqRef (Map.insert piece n1)
                let pid = "m" <> T.unpack (M.ceYear entry) <> "_"
                            <> (if n1 < 10 then "0" else "") <> show n1
                TIO.writeFile (outdir </> pid <> ".match")
                  (MA.matchText piece (M.ceMidi entry) res notesV)
                TIO.writeFile (outdir </> pid <> "_annotations.txt")
                  (MA.annotationsText (MA.aoGrid res) (MA.aoTimes res))
                TIO.writeFile (outdir </> pid <> ".json")
                  (provFor piece entry res)
                let sgrid = outdir </> "midi_score_annotations.txt"
                haveGrid <- doesFileExist sgrid
                unless haveGrid $ TIO.writeFile sgrid
                  (MA.annotationsText (MA.aoGrid res)
                     [ fromIntegral g / fromIntegral B.wnTicks
                     | g <- MA.aoGrid res ])
                putStrLn ("EMIT   " <> tag <> " -> " <> pid <> " "
                            <> statsLine (MA.aoStats res))
  when validate $ do
    verdicts <- reverse <$> readIORef verdictsRef
    unless (null verdicts) $ do
      let agrees = [MA.vAgree v | (_, _, v) <- verdicts]
          beats = [b | (_, _, v) <- verdicts
                     , Just b <- [MA.vBeatMs v]]
          -- the reference's falsy-or: a missing OR ZERO beat reads 999
          bm v = case MA.vBeatMs v of
            Just b | b /= 0 -> b
            _ -> 999
          passes v = (MA.vAgree v >= 0.95 && bm v <= 20)
                       || (MA.vAgree v >= 0.97 && bm v <= 60)
          good = length [() | (_, _, v) <- verdicts, passes v]
      putStrLn ("\n" <> show (length verdicts)
        <> " validated: agree median "
        <> showFFloat (Just 3) (medianOf agrees) ""
        <> ", beat|D| median "
        <> showFFloat (Just 1) (medianOf beats) ""
        <> " ms, " <> show good <> "/" <> show (length verdicts)
        <> " pass the gate (agree>=.95 & beat<=20ms, "
        <> "or agree>=.97 & beat<=60ms)")
  where
    cfgPath = "config" </> "default.toml"
    kernDir = "corpus" </> "bach-wtc" </> "kern"
    medianOf = F.medianD . sort

    cachedChords cfg table irCache piece = do
      cache <- readIORef irCache
      case Map.lookup piece cache of
        Just v -> pure v
        Nothing -> do
          have <- doesFileExist (kernDir </> piece <> ".krn")
          v <- if not have then pure Nothing else do
            src <- readKernSource (editionsFor cfgPath)
                     (kernDir </> piece <> ".krn")
            pure $ case parseKern (tempoFallback TempoDefault) src of
              Left _ -> Nothing
              Right s0 ->
                let s1 = applyTempoOpt TempoDefault s0
                    s = maybe s1 (\b -> s1 {scTempo = Bpm b})
                          (pieceTempo cfg (T.pack piece))
                 in either (const Nothing) (Just . MA.scoreChords)
                      (perform (mkInterp cfg table False piece) s)
          modifyIORef' irCache (Map.insert piece v)
          pure v

    bwvOf piece =
      let book = read [piece !! 3] :: Int
          kind = if piece !! 4 == 'p'
                   then "Prelude" else "Fugue" :: String
          num = read (take 2 (drop 5 piece)) :: Int
       in ( kind
          , "bwv_" <> show ((if book == 1 then 845 else 869) + num) )

    statsLine st = "{score_notes " <> show (MA.stScoreNotes st)
      <> ", matched " <> show (MA.stMatched st)
      <> ", rate " <> MA.pyRepr (MA.stRate st)
      <> ", deleted " <> show (MA.stDeleted st)
      <> ", inserted " <> show (MA.stInserted st)
      <> maybe "" ((", median_dev_ms " <>) . MA.pyRepr)
           (MA.stMedianDevMs st)
      <> "}"

    provFor piece entry res =
      let st = MA.aoStats res
          q s = "\"" <> s <> "\""
       in MA.provJson $
            [ ("piece", q piece)
            , ("maestro", q (T.unpack (M.ceMidi entry)))
            , ("aligner", q "otb maestro-align 1.0")
            , ("score_notes", show (MA.stScoreNotes st))
            , ("matched", show (MA.stMatched st))
            , ("rate", MA.pyRepr (MA.stRate st))
            , ("deleted", show (MA.stDeleted st))
            , ("inserted", show (MA.stInserted st)) ]
            <> [ ( "span_s"
                 , "[\n  " <> MA.pyRepr a <> ",\n  " <> MA.pyRepr b
                     <> "\n ]" )
               | Just (a, b) <- [MA.stSpan st] ]
            <> [ ("median_dev_ms", MA.pyRepr d)
               | Just d <- [MA.stMedianDevMs st] ]

    validateIO res notesV folder perf = do
      let adir = "corpus" </> "asap" </> T.unpack folder
          mpath = adir </> perf <> ".match"
          ann = adir </> perf <> "_annotations.txt"
          sann = adir </> "midi_score_annotations.txt"
      okM <- doesFileExist mpath
      okA <- doesFileExist ann
      if not (okM && okA) then pure Nothing else do
        mp <- B.parseMatch <$> TIO.readFile mpath
        scoreT <- TIO.readFile sann
        annT <- TIO.readFile ann
        pure (MA.validateOne res notesV mp scoreT annT)

    asapPerfMap = do
      ok <- doesFileExist M.asapMetaPath
      if not ok then pure Map.empty else do
        rows <- M.csvDicts <$> TIO.readFile M.asapMetaPath
        pure $ foldl'
          (\mm r ->
             let mp = T.strip (fromMaybe ""
                        (lookup "maestro_midi_performance" r))
                 perf = takeBaseName (T.unpack
                          (fromMaybe "" (lookup "midi_performance" r)))
              in if T.null mp then mm
                 else Map.insertWith (flip (<>))
                        (T.replace "{maestro}/" "" mp)
                        [(fromMaybe "" (lookup "folder" r), perf)] mm)
          Map.empty rows

-- | The per-piece hierarchical fit, in-process: tempo shrunk toward
-- the authority, timing and velocity knob descents in the [piece.X]
-- coordinate system, deployed-candidate scoring and LOPO acceptance.
-- Everything scores against the PRE-FIT config.
runFit :: FilePath -> FilePath -> Double -> [String] -> Bool -> Bool
       -> IO ()
runFit corpus cfgPath shrinkK wanted apply dryRun = do
  cfgText <- TIO.readFile cfgPath
  prefit <- either (die . ("config: " <>)) pure
              (OTB.Config.loadConfig (F.prefitStrip cfgText))
  table <- resolveTemperament "werckmeister3"
  date <- takeWhile (/= '\n')
            <$> readProcess "date" ["+%F"] ""
  let roots = ["corpus" </> "asap", "corpus" </> "maestro-wtc"]
      eds = editionsFor cfgPath
  files <- filter (isSuffixOf ".krn") <$> listDirectory corpus
  let pieces0 = sort (map takeBaseName files)
      pieces = if null wanted then pieces0
               else filter (`elem` wanted) pieces0
  pds0 <- forM pieces $ \piece -> do
    src <- readKernSource eds (corpus </> piece <> ".krn")
    F.loadPieceData roots piece src
  let pds = [pd | Just pd <- pds0]
  -- pieces are independent: fan the descents out across cores (the
  -- Python reference ground through them one subprocess at a time)
  caps <- getNumCapabilities
  -- one global gate on candidate evaluations: descents, LOPO folds
  -- and layers all fork freely, but at most `caps` compiles+bridges
  -- run at once
  sem <- newQSem (max 1 caps)
  recs0 <- parallelForM (length pds) pds $ \pd -> do
    mrec <- fitOnePiece sem prefit table shrinkK pd
    forM_ mrec $ \rec ->
      hPutStrLn stderr (PF.prPiece rec <> ": n=" <> show (PF.prN rec)
                  <> maybe "" (\(_, _, f) -> " tempo " <> PF.fmtD 1 f)
                       (PF.prTempo rec)
                  <> " timing "
                  <> show (maybe False PF.flKept (PF.prTiming rec))
                  <> " vel "
                  <> show (maybe False PF.flKept (PF.prVelocity rec)))
    pure mrec
  let recs = [rec | Just rec <- recs0]
  let newText = PF.applyFits date recs cfgText
  if dryRun
    then do
      -- the documented contract: a diff, not the whole rewritten file
      (_, out, _) <- readProcessWithExitCode "diff"
        ["-u", cfgPath, "-"] (T.unpack newText)
      putStr out
    else when apply $ do
      TIO.writeFile cfgPath newText
      putStrLn ("applied "
                  <> show (sum (map (length . snd)
                                 (PF.buildSections date recs)))
                  <> " keys — validate with a compile")

-- | A fixed-width worker pool: results keep input order, exceptions
-- propagate.
parallelForM :: forall a b. Int -> [a] -> (a -> IO b) -> IO [b]
parallelForM cap xs f = do
  slots <- mapM (const newEmptyMVar) xs
  queue <- newMVar (zip xs slots)
  let worker = do
        next <- modifyMVar queue $ \q ->
          pure (drop 1 q, take 1 q)
        case next of
          [] -> pure ()
          ((x, slot) : _) -> do
            r <- try (f x)
            putMVar slot (r :: Either SomeException b)
            worker
  forM_ [1 .. cap] (const (forkIO worker))
  mapM (either throwIO pure <=< takeMVar) slots

-- | Fit one piece against its humans.
fitOnePiece :: QSem -> OTB.Config.Config -> TuningTable -> Double
            -> F.PieceData -> IO (Maybe PF.PieceRec)
fitOnePiece sem prefit table shrinkK pd = do
  let piece = F.pdPiece pd
      perfNames = map fst (F.pdMatches pd)
      n = length perfNames
  if n == 0 then pure Nothing else do
    let ms0 = parseKern (tempoFallback TempoDefault) (F.pdSource pd)
        performWith knobs =
          case ms0 of
            Left _ -> Nothing
            Right s0 ->
              let cfg' = F.applyKnobs F.timingFamily
                           (Just (T.pack piece)) knobs prefit
                  s1 = applyTempoOpt TempoDefault s0
                  s = maybe s1 (\b -> s1 {scTempo = Bpm b})
                        (pieceTempo cfg' (T.pack piece))
               in either (const Nothing) Just
                    (perform (mkInterp cfg' table False piece) s)
    -- authority + human body tempo
    let mAuth = do
          p <- performWith []
          pure (F.medianD (sort [b | (_, Bpm b) <- perfTempoMap p]))
        humanTempos =
          [ t
          | (scoreT, perfs) <- F.pdAnnotations pd
          , (name, perfT) <- perfs
          , name `elem` perfNames
          , Just t <- [bodyTempo scoreT perfT] ]
        mTempo = case (mAuth, humanTempos) of
          (Just auth, ts@(_ : _)) ->
            let human = F.medianD (sort ts)
                fitted = auth + (human - auth) * fromIntegral n
                           / (fromIntegral n + 1)
             in Just ( roundTo1 human, roundTo1 auth, roundTo1 fitted )
          _ -> Nothing
        roundTo1 x = fromIntegral (round (x * 10) :: Integer) / 10
    -- cached per-candidate performer scores
    cacheRef <- newIORef (Map.empty
      :: Map.Map (String, [(T.Text, Double)])
           (MVar (Either SomeException [(String, Double)])))
    -- in-flight dedup: the first caller claims the key and computes
    -- (gated by the global sem), racers block on the MVar instead of
    -- recomputing. The cell holds an Either so a failed evaluation
    -- fills it anyway — waiters rethrow instead of blocking forever
    let scored fam rsFn knobs = do
          let key = (F.kfName fam, sortOn fst knobs)
          fresh <- newEmptyMVar
          claim <- atomicModifyIORef' cacheRef $ \c ->
            case Map.lookup key c of
              Just mv -> (c, Left mv)
              Nothing -> (Map.insert key fresh c, Right fresh)
          case claim of
            Left mv -> either throwIO pure =<< readMVar mv
            Right mv -> do
              r <- try $ bracket_ (waitQSem sem) (signalQSem sem) $
                evaluate (force
                  (maybe [] (\p -> rsFn p pd) (performWith knobs)))
              putMVar mv r
              either throwIO pure r
        objective fam rsFn only knobs = do
          rs <- scored fam rsFn knobs
          let ok = [ r | (perf, r) <- rs, not (isNaN r)
                   , maybe True (perf `elem`) only ]
          pure $ if null ok then Nothing
                 else Just (sum ok / fromIntegral (length ok))
        -- fixed-order two-sweep descent (the reference's semantics)
        descend2 obj grids only = do
          mb <- obj only []
          case mb of
            Nothing -> pure Nothing
            Just b0 -> do
              (cur, best) <- foldM
                (\acc _sweep -> foldM (stepK obj only) acc
                                  (map fst grids))
                ([], b0) [1 :: Int, 2]
              pure (Just (cur, b0, best))
          where
            stepK obj' only' (cur, best) k = do
              let grid = maybe [] id (lookup k grids)
              -- warm the knob's whole grid concurrently: within one
              -- knob every trial equals (insert k v cur) regardless
              -- of the fold's improvements, so the sequential logic
              -- below only ever reads warmed keys
              forM_ grid $ \v -> forkIO . void . obj' only' $
                Map.toList (Map.insert k v (Map.fromList cur))
              foldM
                (\(c, b) v ->
                   if lookup k c == Just v then pure (c, b) else do
                     r <- obj' only'
                            (Map.toList (Map.insert k v
                                           (Map.fromList c)))
                     pure $ case r of
                       Just x | x > b + 1e-4 ->
                         (Map.toList (Map.insert k v (Map.fromList c))
                         , x)
                       _ -> (c, b))
                (cur, best) grid
        baseOf k = Map.findWithDefault 0 k
          (Map.findWithDefault Map.empty
             (Map.findWithDefault "agogics" k
                (F.kfSections F.timingFamily
                   <> F.kfSections F.velocityFamily)) prefit)
        layer fam rsFn grids = do
          let obj = objective fam rsFn
          mres <- descend2 obj grids Nothing
          case mres of
            Nothing -> pure Nothing
            Just (fitted, base, _rawBest) -> do
              let shrunk = PF.shrinkKnobs fitted baseOf n shrinkK
              mdep <- obj Nothing shrunk
              case mdep of
                Nothing -> pure Nothing
                Just dep -> do
                  let beats = dep >= base - 1e-4
                  lopo <- if n >= 3 && not (null fitted)
                    then do
                      ds <- parallelForM n perfNames $ \hold -> do
                        let keep = filter (/= hold) perfNames
                        mf <- descend2 obj grids (Just keep)
                        case mf of
                          Nothing -> pure Nothing
                          Just (f2, _, _) -> do
                            let s2 = PF.shrinkKnobs f2 baseOf
                                       (n - 1) shrinkK
                            rf <- obj (Just [hold]) s2
                            rb <- obj (Just [hold]) []
                            pure ((-) <$> rf <*> rb)
                      let good = [d | Just d <- ds]
                      pure $ if null good then Nothing
                        else Just (sum good
                                     / fromIntegral (length good))
                    else pure Nothing
                  let kept = case lopo of
                        Just d -> beats && d >= -1e-3
                        Nothing -> not (null fitted) && beats
                  pure (Just (PF.FitLayer shrunk base dep lopo kept))
    layers <- parallelForM 2
      [ layer F.timingFamily F.timingRs PF.timingKnobs
      , layer F.velocityFamily F.velocityRsNoOrn PF.velocityKnobs ]
      id
    case layers of
      [timing, vel] ->
        pure (Just (PF.PieceRec piece n mTempo timing vel))
      _ -> pure Nothing

-- | Quarter-note bpm over the middle 80% of a performance's beats.
bodyTempo :: T.Text -> T.Text -> Maybe Double
bodyTempo scoreT perfT =
  let sw = [fromRational w :: Double
           | WholeNotes w <- scoreBeatPositions scoreT]
      ts = perfBeatTimes perfT
      nn = min (length sw) (length ts)
   in if nn < 5 then Nothing else
        let lo = (nn * 10) `div` 100
            hi = (nn * 90) `div` 100
            dw = sw !! hi - sw !! lo
            dt = ts !! hi - ts !! lo
         in if dt <= 0 || dw <= 0 then Nothing
            else Just (dw / dt * 240)

-- | Multi-start fixpoint search over a global knob family, evaluated
-- against the PRE-FIT config on Book I with Book II held out. The
-- whole run is in-process: a candidate evaluation is a parMap over
-- the train pieces, cached by knob tuple.
runLandscape :: FilePath -> FilePath -> String -> Int -> Int -> Double
             -> Double -> Double -> Maybe FilePath -> IO ()
runLandscape corpus cfgPath famName starts seed slack zeroFloor
             divBonus emitElite = do
  when (starts < 1) $ die "--starts must be at least 1"
  let badD nm v = isNaN v || isInfinite v
  when (badD "slack" slack || slack < 0) $
    die "--slack must be finite and >= 0"
  when (badD "zero-floor" zeroFloor
          || zeroFloor < 0 || zeroFloor > 1) $
    die "--zero-floor must be finite and in [0, 1]"
  when (badD "diversity-bonus" divBonus || divBonus < 0) $
    die "--diversity-bonus must be finite and >= 0"
  -- one registered condition per run: a mixed transformation would
  -- carry an unlabeled objective into the record
  when (zeroFloor > 0 && divBonus > 0) $
    die "pick one condition: --zero-floor or --diversity-bonus"
  cfgText <- TIO.readFile cfgPath
  prefit <- either (die . ("config: " <>)) pure
              (OTB.Config.loadConfig (F.prefitStrip cfgText))
  table <- resolveTemperament "werckmeister3"
  let fam = if famName == "velocity"
              then F.velocityFamily else F.timingFamily
      roots = ["corpus" </> "asap", "corpus" </> "maestro-wtc"]
      eds = editionsFor cfgPath
  files <- filter (isSuffixOf ".krn") <$> listDirectory corpus
  pds0 <- forM (sort (map takeBaseName files)) $ \piece -> do
    src <- readKernSource eds (corpus </> piece <> ".krn")
    F.loadPieceData roots piece src
  let pds = [pd | Just pd <- pds0]
      train = [pd | pd <- pds, "wtc1" `isPrefixOf'` F.pdPiece pd]
      test = [pd | pd <- pds, "wtc2" `isPrefixOf'` F.pdPiece pd]
      isPrefixOf' a b = take (length a) b == a
      rsFn = if famName == "velocity" then F.velocityRs else F.timingRs
      agg = if famName == "velocity"
              then F.meanOfPieceMedians else F.meanFlat
      -- parse once per piece; candidates only re-perform
      parsed = Map.fromList
        [ (F.pdPiece pd, s0)
        | pd <- pds
        , Right s0 <- [parseKern (tempoFallback TempoDefault)
                         (F.pdSource pd)] ]
      score cfg' pd = case Map.lookup (F.pdPiece pd) parsed of
        Nothing -> []
        Just s0 ->
          let s1 = applyTempoOpt TempoDefault s0
              s = maybe s1 (\b -> s1 {scTempo = Bpm b})
                    (pieceTempo cfg' (T.pack (F.pdPiece pd)))
           in either (const []) (\p -> map snd (rsFn p pd))
                (perform (mkInterp cfg' table False (F.pdPiece pd)) s)
      evalSet set knobs =
        let cfg' = F.applyKnobs fam Nothing knobs prefit
         in agg (parMap rdeepseq (score cfg') set)
  cache <- newIORef (Map.empty :: Map.Map [(T.Text, Double)]
                                   (Maybe Double))
  let -- condition 2 (positive contribution): the lattice loses its
      -- OFF values — each grid's zero becomes FRAC x its smallest
      -- nonzero step, so every rule must participate at least a little
      floorVal g = case [v | v <- g, v > 0] of
        [] -> 0
        pos -> zeroFloor * minimum pos
      floorGrid g =
        [if v == 0 then floorVal g else v | v <- g]
      grids = if zeroFloor <= 0
                then F.kfGrids fam
                else [(k, floorGrid g) | (k, g) <- F.kfGrids fam]
      nKnobs = fromIntegral (length grids) :: Double
      nonzeroFrac ks =
        fromIntegral (length [() | (_, v) <- ks, v /= 0]) / nKnobs
      objectiveRaw knobs = do
        let key = sortOn fst knobs
        c <- readIORef cache
        case Map.lookup key c of
          Just v -> pure v
          Nothing -> do
            let v = evalSet train knobs
            v `seq` modifyIORef' cache (Map.insert key v)
            pure v
      -- condition 3 (regularized): selection rewards rule diversity;
      -- every REPORTED r stays the raw statistic
      objective knobs
        | divBonus == 0 = objectiveRaw knobs
        | otherwise =
            fmap (+ divBonus * nonzeroFrac knobs) <$>
              objectiveRaw knobs
      baseKnobs =
        [ (k, if zeroFloor > 0 && v == 0
                then floorVal (maybe [] id (lookup k (F.kfGrids fam)))
                else v)
        | (k, _) <- grids
        , let v = Map.findWithDefault 0
                    k (Map.findWithDefault Map.empty
                         (Map.findWithDefault "agogics" k
                            (F.kfSections fam)) prefit) ]
      -- randomized-order coordinate descent to a FIXPOINT: sweeps
      -- until a full pass improves nothing (capped), so observed
      -- spread across starts is landscape structure, not incomplete
      -- convergence
      descendIO rng0 init0 = do
        r0 <- objective init0
        case r0 of
          Nothing -> pure Nothing
          Just b0 -> Just <$> loop rng0 (Map.fromList init0) b0 1
        where
          loop rng cur best sweep = do
            let (order, rng') = F.shuffle rng (map fst grids)
            (cur', best', imp) <- foldM stepK (cur, best, False) order
            if imp && sweep < 8
              then loop rng' cur' best' (sweep + 1)
              else pure (cur', best', sweep)
          stepK (cur, best, imp) k = do
            let grid = maybe [] id (lookup k grids)
                tryV (c, b, i) v
                  | Just v == Map.lookup k c = pure (c, b, i)
                  | otherwise = do
                      r <- objective (Map.toList (Map.insert k v c))
                      pure $ case r of
                        Just x | x > b + 1e-4 ->
                          (Map.insert k v c, x, True)
                        _ -> (c, b, i)
            foldM tryV (cur, best, imp) grid

  -- ---- experiment identity ----
  -- the checkpoint must identify the EXPERIMENT: results from
  -- different compiler code, configs, editions or human data must
  -- never be combined. Each component is digested separately so a
  -- mismatch names what changed.
  let statePath = "corpus" </> ("knob-landscape-" <> famName <> ".json")
      pieceBlob pd = T.concat
        ( [T.pack (F.pdPiece pd), F.pdSource pd]
            <> concat [ scoreT : concat [[T.pack nm, t]
                                        | (nm, t) <- perfs]
                      | (scoreT, perfs) <- F.pdAnnotations pd ]
            <> concat [[T.pack nm, mt] | (nm, mt) <- F.pdMatches pd] )
      sha t = takeWhile (/= ' ') <$> readProcess "sha256sum" [] t
  corpusDigests <- forM pds $ \pd ->
    (,) (F.pdPiece pd) <$> sha (T.unpack (pieceBlob pd))
  prefitDigest <- sha (T.unpack (F.prefitStrip cfgText))
  binDigest <- do
    exe <- getExecutablePath
    takeWhile (/= ' ') <$> readProcess "sha256sum" [exe] ""
  paramsDigest <- sha (show grids <> show (sortOn fst baseKnobs)
                         <> famName <> show seed <> show zeroFloor
                         <> show divBonus)
  fp <- sha (paramsDigest <> prefitDigest <> binDigest
               <> concatMap snd corpusDigests)
  let digestsJson = J.JObj
        ( [ ("params", J.JStr (T.pack paramsDigest))
          , ("prefit_config", J.JStr (T.pack prefitDigest))
          , ("binary", J.JStr (T.pack binDigest))
          , ("corpus", J.JObj [ (T.pack p, J.JStr (T.pack d))
                              | (p, d) <- corpusDigests ]) ] )
      metaFor nStarts mprod = J.JObj
        ( [ ("fingerprint", J.JStr (T.pack fp))
          , ("family", J.JStr (T.pack famName))
          , ("seed", J.JNum (T.pack (show seed)))
          , ("args", J.JObj
              [ ("corpus", J.JStr (T.pack corpus))
              , ("config", J.JStr (T.pack cfgPath))
              , ("starts", J.JNum (T.pack (show nStarts)))
              , ("slack", J.JNum (T.pack (MA.pyRepr slack)))
              , ("zero_floor", J.JNum (T.pack (MA.pyRepr zeroFloor)))
              , ("diversity_bonus",
                  J.JNum (T.pack (MA.pyRepr divBonus))) ])
          , ("digests", digestsJson)
          , ("train", J.JArr [ J.JStr (T.pack (F.pdPiece pd))
                             | pd <- train ])
          , ("test", J.JArr [ J.JStr (T.pack (F.pdPiece pd))
                            | pd <- test ]) ]
          <> [ ("producer", p) | Just p <- [mprod] ] )
      metaJson = metaFor starts Nothing

  haveState <- doesFileExist statePath
  stored0 <- if not haveState then pure [] else do
    raw <- TIO.readFile statePath
    case J.parseJson raw of
      Right st
        | (J.jStr =<< J.jLookup "fingerprint"
             =<< J.jLookup "_meta" st) == Just (T.pack fp) ->
            pure (J.jArrOf (fromMaybe (J.JArr [])
                              (J.jLookup "finals" st)))
        | otherwise -> do
            let old k = J.jStr =<< J.jLookup k
                  =<< J.jLookup "digests" =<< J.jLookup "_meta" st
                changed =
                  [ nm | (nm, cur, stO) <-
                      [ ("params", paramsDigest, old "params")
                      , ("config", prefitDigest, old "prefit_config")
                      , ("binary", binDigest, old "binary") ]
                  , stO /= Just (T.pack cur) ]
                oldCorpus = fromMaybe (J.JObj [])
                  (J.jLookup "corpus" =<< J.jLookup "digests"
                     =<< J.jLookup "_meta" st)
                corpusChanged =
                  [ p | (p, d) <- corpusDigests
                      , (J.jStr =<< J.jLookup (T.pack p) oldCorpus)
                          /= Just (T.pack d) ]
            putStrLn ("landscape inputs changed ("
              <> intercalate ", "
                   (changed <> take 3 corpusChanged
                      <> [ "+" <> show (length corpusChanged - 3)
                             <> " more pieces"
                         | length corpusChanged > 3 ])
              <> ") — starting fresh")
            pure []
      _ -> do
        putStrLn "landscape state unreadable — starting fresh"
        pure []
  when (not (null stored0)) $
    putStrLn ("resuming: " <> show (length stored0)
                <> " starts checkpointed in " <> statePath)
  finalsRef <- newIORef stored0
  let knobsJson ks = J.JObj
        [ (k, J.JNum (T.pack (MA.pyRepr v))) | (k, v) <- ks ]
      stateJson entries = J.dumpJson (Just 1) (J.JObj
        [("_meta", metaJson), ("finals", J.JArr entries)])
      saveState = do
        entries <- readIORef finalsRef
        -- atomic: a kill mid-write must not truncate the state (a
        -- parse failure would discard every completed start)
        let tmpPath = statePath <> ".tmp"
        TIO.writeFile tmpPath (stateJson entries)
        renameFile tmpPath statePath
      r4 x = fromIntegral (round (x * 1e4) :: Integer) / 1e4
      runStart i = do
        let rng = F.mkRng (seed * 1000 + i)
            (initK, rngAfter) =
              if i == 0 then (baseKnobs, rng) -- start 0 = committed
              else foldl'
                (\(acc, r) (k, g) ->
                   let (v, r') = F.choice r g in (acc <> [(k, v)], r'))
                ([], rng) grids
        res <- descendIO rngAfter initK
        case res of
          Nothing -> die ("start " <> show i
                            <> ": objective undefined — empty corpus?")
          Just (cur, sel, sweeps) -> do
            rawM <- objectiveRaw (Map.toList cur)
            let raw = fromMaybe sel rawM
                testR = evalSet test (Map.toList cur)
            putStrLn ("start " <> show i <> ": train " <> show raw
                        <> (if divBonus /= 0
                              then " sel " <> show sel else "")
                        <> " test " <> show testR
                        <> " sweeps " <> show sweeps)
            modifyIORef' finalsRef (<> [J.JObj
              ( [ ("start", J.JNum (T.pack (show i)))
                , ("init", knobsJson initK)
                , ("final", knobsJson (Map.toList cur))
                , ("sweeps", J.JNum (T.pack (show sweeps)))
                , ("train_r", J.JNum (T.pack (MA.pyRepr (r4 raw)))) ]
                <> [ ("sel_r", J.JNum (T.pack (MA.pyRepr (r4 sel))))
                   | divBonus /= 0 ]
                <> [ ("test_r", maybe J.JNull
                       (J.JNum . T.pack . MA.pyRepr . r4) testR) ] )])
            saveState
  forM_ [length stored0 .. starts - 1] runStart

  -- ---- report, from the state (fresh and resumed runs agree) ----
  entries <- readIORef finalsRef
  let finals =
        [ (ks, tr, te)
        | e <- entries
        , Just tr <- [J.jNum =<< J.jLookup "train_r" e]
        , let te = J.jNum =<< J.jLookup "test_r" e
              ks = [ (k, v)
                   | J.JObj kvs <- [fromMaybe (J.JObj [])
                                      (J.jLookup "final" e)]
                   , (k, jv) <- kvs, Just v <- [J.jNum jv] ] ]
  when (null finals) $ die "no completed starts"
  let bestTrain = maximum [b | (_, b, _) <- finals]
      elite = sortOn (\(_, b, _) -> negate b)
        [f | f@(_, b, _) <- finals, b >= bestTrain - slack]
      eliteTests = [t | (_, _, Just t) <- elite]
      spread = if null eliteTests then "" else
        " (test spread " <> show (minimum eliteTests) <> ".."
          <> show (maximum eliteTests) <> ")"
      condition
        | zeroFloor > 0 = "positive-contribution (zero-floor "
            <> show zeroFloor <> ")"
        | divBonus /= 0 = "regularized (diversity-bonus "
            <> show divBonus <> ")"
        | otherwise = "unconstrained (zeros allowed)"
  putStrLn ("\ncondition: " <> condition)
  putStrLn (show (length finals) <> " starts; best train "
              <> show bestTrain <> "; " <> show (length elite)
              <> " within slack " <> show slack <> spread)
  forM_ (map fst grids) $ \k -> do
    let vals fs = [maybe 0 id (lookup k f) | (f, _, _) <- fs]
        za = length [() | v <- vals finals, v == 0]
        ze = length [() | v <- vals elite, v == 0]
        uniqSorted = Map.keys . Map.fromList . map (, ())
    putStrLn (T.unpack k <> "  zero " <> show za <> "/"
                <> show (length finals) <> " elite-zero " <> show ze
                <> "/" <> show (length elite) <> "  elite values "
                <> show (uniqSorted (vals elite)))

  -- ---- experiment manifest: the committed record of a completed
  -- run. Its id derives from the COMPLETE content (metadata incl.
  -- producer + results), so different runs can never share a name and
  -- an existing manifest is never overwritten; args.starts records
  -- what actually ran, not what this invocation asked for.
  when (length finals >= starts) $ do
    createDirectoryIfMissing True "experiments"
    date <- takeWhile (/= '\n') <$> readProcess "date" ["+%F"] ""
    producer <- do
      let tryP c as = either
            (\e -> let _ = (e :: SomeException) in "unknown")
            (takeWhile (/= '\n'))
            <$> try (readProcess c as "")
      commit <- tryP "git" ["rev-parse", "HEAD"]
      porc <- either
        (\e -> let _ = (e :: SomeException) in "unknown")
        id <$> try (readProcess "git"
                      ["status", "--porcelain", "-uno"] "")
      pure (J.JObj
        [ ("commit", J.JStr (T.pack commit))
        , ("dirty", J.JBool (not (null porc)))
        , ("compiler", J.JStr (T.pack
            (compilerName <> "-" <> showVersion fullCompilerVersion)))
        , ("os", J.JStr (T.pack os))
        , ("arch", J.JStr (T.pack arch)) ])
    let manifestJson = J.dumpJson (Just 1) (J.JObj
          [ ("_meta", metaFor (length entries) (Just producer))
          , ("finals", J.JArr entries) ])
    mid <- sha (T.unpack manifestJson)
    let manifestPath = "experiments"
          </> ("landscape-" <> famName <> "-" <> date <> "-"
                 <> take 12 mid <> ".json")
    exists <- doesFileExist manifestPath
    if exists
      then putStrLn ("\nmanifest already registered: " <> manifestPath)
      else do
        TIO.writeFile manifestPath manifestJson
        putStrLn ("\nmanifest: " <> manifestPath
                    <> " — hashes, arguments, membership, results; "
                    <> "commit it with the conclusions it backs")

  -- ---- condition 4 bridge: equally predictive solutions, rendered
  forM_ emitElite $ \dir -> do
    createDirectoryIfMissing True dir
    let prefitText = F.prefitStrip cfgText
        secOf k = Map.findWithDefault "agogics" k (F.kfSections fam)
        overrides ks = T.concat
          [ "\n[" <> sec <> "]\n" <> T.concat
              [ k <> " = " <> T.pack (MA.pyRepr v) <> "\n"
              | (k, v) <- kvs ]
          | (sec, kvs) <- Map.toList (foldl'
              (\mm (k, v) -> Map.insertWith (flip (<>))
                 (secOf k) [(k, v)] mm)
              Map.empty ks) ]
    forM_ (zip [1 :: Int ..] elite) $ \(rank, (ks, tr, _)) -> do
      let path = dir </> ("elite-"
            <> (if rank < 10 then "0" else "") <> show rank
            <> ".toml")
      TIO.writeFile path
        ( prefitText
            <> "\n# landscape elite " <> T.pack (show rank)
            <> ": family " <> T.pack famName
            <> ", train r " <> T.pack (show tr)
            <> " — appended overrides win\n"
            <> overrides ks )
    putStrLn ("elite configs: " <> show (length elite) <> " -> "
                <> dir <> " (compile with --config for A/B)")

-- | The parity gate for the bridge port: identical invocation of the
-- Python reference and this dump must produce byte-identical TSV.
runBridgeDump :: FilePath -> FilePath -> FilePath -> IO ()
runBridgeDump krn match cfgPath = do
  cfg <- loadCfg cfgPath
  table <- resolveTemperament "werckmeister3"
  let piece = takeBaseName krn
  src <- readKernSource (editionsFor cfgPath) krn
  matchT <- TIO.readFile match
  case parseKern (tempoFallback TempoDefault) src of
    Left e -> die ("parse: " <> e)
    Right s0 -> do
      let s1 = applyTempoOpt TempoDefault s0
          s = maybe s1 (\b -> s1 {scTempo = Bpm b})
                (pieceTempo cfg (T.pack piece))
      case perform (mkInterp cfg table False piece) s of
        Left e -> die ("perform: " <> e)
        Right p -> do
          let irn = B.loadIrNotes p
              (rows, c) = B.bridge irn (B.parseMatch matchT)
              d9 x = showFFloat (Just 9) x ""
              md9 = maybe "-" d9
              mi = maybe "-" show
              mt = maybe "-" T.unpack
              passName pass = case pass of
                B.PassExact -> "exact"; B.PassSeg -> "seg"
                B.PassFuzzy -> "fuzzy"; B.PassOrn -> "orn"
              rules r = intercalate ";"
                [ T.unpack k <> "=" <> T.unpack v
                | (k, v) <- Map.toAscList (B.brRules r) ]
              bool b = if b then "1" else "0"
          forM_ rows $ \r -> putStrLn (intercalate "\t"
            [ d9 (B.brWn r), show (B.brPitch r)
            , T.unpack (B.brXmlId r), T.unpack (B.brVoice r)
            , passName (B.brPass r)
            , show (B.brHumanVel r), d9 (B.brHumanOnS r)
            , d9 (B.brHumanOffS r)
            , show (B.brOtbVel r), d9 (B.brOtbOnS r), d9 (B.brOtbDurS r)
            , md9 (B.brOtbOnWn r), md9 (B.brOtbDurWn r), mi (B.brCh r)
            , rules r, bool (B.brIsFinal r), bool (B.brGrace r)
            , mi (B.brGatePct r), mt (B.brGateLabel r)
            , bool (B.brIsOrn r) ])
          putStrLn ("# " <> intercalate " "
            [ "matched=" <> show (B.cMatched c)
            , "exact=" <> show (B.cExact c)
            , "seg=" <> show (B.cSeg c)
            , "fuzzy=" <> show (B.cFuzzy c)
            , "orn=" <> show (B.cOrn c)
            , "seg_runs=" <> show (B.cSegRuns c)
            , "unmatched=" <> show (B.cUnmatched c)
            , "deleted=" <> show (B.cDeleted c)
            , "scale=" <> show (B.cScale c)
            , "offset_wn=" <> showFFloat (Just 9) (B.cOffsetWn c) "" ])

-- | wtc1f01 -> Bach/Fugue/bwv_846 (Nothing for non-WTC names).
bwvDir :: String -> Maybe FilePath
bwvDir p = case p of
  ['w', 't', 'c', b, k, n1, n2]
    | b `elem` ("12" :: String)
    , k `elem` ("pf" :: String)
    , all (`elem` ("0123456789" :: String)) [n1, n2] ->
      let num = read [n1, n2] :: Int
          bwv = (if b == '1' then 845 else 869) + num
          kind = if k == 'p' then "Prelude" else "Fugue"
       in Just ("Bach" </> kind </> ("bwv_" <> show bwv))
  _ -> Nothing

runStats :: FilePath -> FilePath -> Double -> IO ()
runStats corpus cfgPath tempo = do
  _cfg <- loadCfg cfgPath
  files <- filter (isSuffixOf ".krn") <$> listDirectory corpus
  parsed <- mapM
    (\f -> do
        src <- readKernSource (editionsFor cfgPath) (corpus </> f)
        pure (takeBaseName f, parseKern (Bpm tempo) src))
    (sort files)
  let scores = [(n, s) | (n, Right s) <- parsed]
      allNotes s = [sn | v <- scVoices s, sn <- vNotes v]
      sounding s = [(snOnset n, snDur n, snPitch n) | n <- allNotes s]
      charges =
        [ c | (_, s) <- scores
        , v <- scVoices s
        , c <- chargesForLane (sounding s) (vNotes v) ]
      hist =
        [ (lo, length [() | c <- charges, c >= lo, c < lo + 0.2])
        | lo <- [0, 0.2, 0.4, 0.6, 0.8] ]
      ornaments =
        sum [ 1 :: Int | (_, s) <- scores, n <- allNotes s
            , m <- snMarks n, isOrn m ]
      isOrn m = case m of
        Trill _ -> True; Mordent _ -> True; InvMordent _ -> True
        Turn _ _ -> True; InvTurn _ _ -> True; _ -> False
      placeholders =
        [n | (n, s) <- scores, scTempo s == Bpm 72]
  putStrLn ("pieces parsed: " <> show (length scores) <> "/"
              <> show (length parsed))
  putStrLn ("dissonance charge histogram (0.2 bins): " <> show hist)
  putStrLn ("ornament marks: " <> show ornaments)
  putStrLn ("suspect 72-BPM placeholder tempos ("
              <> show (length placeholders) <> "): "
              <> unwords placeholders)

-- ---------------------------------------------------------------------
-- W7: pole two — a generated ground through the same Player

runGround
  :: String -> Int -> Double -> String
  -> FilePath -> Maybe FilePath -> Maybe FilePath -> IO ()
runGround bass nvar tempo temp out mscl mjson = do
  checkBpm "--tempo" tempo
  when (temp == "adaptive" && mscl /= Nothing)
    (die ("adaptive temperament is bend-carried per chord; no static "
            <> ".scl can express it — drop --emit-scl"))
  let adaptive = temp == "adaptive"
  table <- resolveTemperament (if adaptive then "werckmeister3" else temp)
  s <- either die pure (generateScore bass nvar (Bpm tempo))
  let piece = "ground-" <> bass
      ip = defaultInterp {iPiece = piece, iTuning = table, iAdaptive = adaptive}
  p <- either die pure (perform ip s)
  writeSmf out p
  putStrLn (out <> ": " <> bass <> ", " <> show nvar <> " variations")
  case mscl of
    Nothing -> pure ()
    Just fp -> TIO.writeFile fp (renderScl (T.pack temp) table)
  case mjson of
    Nothing -> pure ()
    Just fp -> writeFile fp (renderJson piece p)

-- ---------------------------------------------------------------------
-- analyze: the structural features, exported for the basis-function
-- optimizer (tools/structure_fit.py) — the same analysis code the
-- Player itself runs, so what gets fitted is what gets performed

runAnalyze :: Common -> IO ()
runAnalyze com = do
  (_, score, _, _, _, interp) <- load com
  -- the SAME analysis perform runs: configured interpretation,
  -- prepared lanes, filtered boundaries — what gets exported for
  -- fitting is what gets played
  let st = analyzeStructure interp score
      allBounds = stBounds st
      end = stEnd st
      harm = analyzeHarmony (scMeter score) (stSounding st) end
      tree = stTree st
      subj = subjectEntries score
      sq = findSequences score
      im = findImitation score
      wn (WholeNotes r) = show (fromRational r :: Double)
      arr xs = "[" <> intercalate "," xs <> "]"
      pair a b = "[" <> a <> "," <> b <> "]"
  putStrLn $ "{"
    <> "\"end\":" <> wn end
    <> ",\"boundaries\":" <> arr [ pair (wn t) (show s)
                                 | (t, s) <- sortOn fst allBounds ]
    <> ",\"tree\":" <> arr [ "[" <> wn a <> "," <> wn b <> ","
                               <> show depth <> "]"
                           | (a, b, depth) <- tree ]
    <> ",\"cadences\":" <> arr (map wn (hCadences harm))
    <> ",\"charge\":" <> arr [ pair (wn (WholeNotes t))
                                 (show (hChargeAt harm (WholeNotes t)))
                             | let WholeNotes er = end
                             , t <- takeWhile (< er) (iterate (+ 1 / 4) 0) ]
    <> ",\"subject\":" <> arr (map (wn . fst) subj)
    <> ",\"sequences\":" <> arr [ pair (wn a) (wn b) | (a, b) <- sqSpans sq ]
    <> ",\"seams\":" <> arr (map wn (sqSeams sq))
    <> ",\"takes\":" <> arr [ "[" <> wn t <> "," <> show v <> "," <> wn sp <> "]"
                            | (t, v, sp) <- imTakes im ]
    <> ",\"exchanges\":" <> arr [ pair (wn a) (wn b) | (a, b) <- imSpans im ]
    <> ",\"novelty\":" <> arr [ pair (wn o) (show v)
                              | (o, v) <- sqNovelty sq ]
    <> ",\"onsets\":" <> arr (map (wn . (\(o, _, _) -> o)) (stSounding st))
    <> "}"
