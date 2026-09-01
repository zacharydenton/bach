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
import Control.Monad (forM, forM_, when)
import Control.Parallel.Strategies (parMap, rdeepseq)
import Data.ByteString.Lazy qualified as BL
import Data.List (isSuffixOf, sort, sortOn)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import OTB.Config
  ( agogicsFor, artParamsFor, dynamicsFor, expressFor, emptyConfig, loadConfig
  , ornamentsFor, phrasingFor, pieceTempo, tuningBendRange )
import OTB.Config qualified
import OTB.Edition (editionsFor, readKernSource)
import OTB.Eval (PerfR (..), loadDirAnnotations, scorePerformances)
import OTB.Bridge qualified as B
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
import System.Directory (createDirectoryIfMissing, doesFileExist, listDirectory)
import System.Exit (die)
import System.FilePath (takeBaseName, (</>))

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
  | Eval FilePath FilePath TempoOpt String (Maybe FilePath) [FilePath] [String]
  | BridgeDump FilePath FilePath FilePath
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
    Eval corpus cfgPath tempo temp eds roots ps ->
      runEval corpus cfgPath tempo temp eds roots ps
    BridgeDump krn match cfgPath -> runBridgeDump krn match cfgPath
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
        -> [FilePath] -> [String] -> IO ()
runEval corpus cfgPath tempo temp meds roots0 wanted = do
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
        if all (== Nothing) anns
          then pure []
          else do
            src <- readKernSource eds (corpus </> piece <> ".krn")
            pure [(piece, src, [a | Just a <- anns])]
  let one (piece, src, anns) =
        let r = do
              s0 <- parseKern (tempoFallback tempo) src
              let s1 = applyTempoOpt tempo s0
                  s = maybe s1 (\b -> s1 {scTempo = Bpm b})
                        (pieceTempo cfg (T.pack piece))
              p <- perform (mkInterp cfg table adaptive piece) s
              pure (concatMap (scorePerformances (perfTempoMap p)) anns)
         in ( piece
            , either (const []) id r
            )
      results = parMap rdeepseq
        (\i -> let (p, rs) = one i
                in (p, [(prPerformer x, prR x, prBeats x) | x <- rs]))
        inputs
  forM_ results $ \(piece, rs) ->
    forM_ rs $ \(perf, r, nb) ->
      putStrLn (piece <> "\t" <> perf <> "\t" <> show r
                  <> "\t" <> show nb)
  let allRs = [r | (_, rs) <- results, (_, r, _) <- rs, not (isNaN r)]
  putStrLn ("# grand mean r = "
              <> show (sum allRs / fromIntegral (max 1 (length allRs)))
              <> " over " <> show (length allRs) <> " performances")

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
