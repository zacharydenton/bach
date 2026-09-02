-- | Bake the static patchboard: site/ becomes a self-contained board.
-- Ports tools/bake_site.py; the album regeneration runs in-process
-- (Main injects it), the wasm build stays a cmake subprocess.
--
-- The board renders in the listener's browser via the surge fork's
-- WebAssembly build (~/code/surge, branch wasm-headless-audioworklet).
-- Everything it needs to know is baked into site/data/:
--
--   data/perf/\<piece\>.json  the PerformanceIRs, regenerated from HEAD
--                             (otb album) unless --perf-dir copies
--   data/w3.scl               the temperament (loaded per synth)
--   data/patches/...          the Surge factory bank, category/name
--   data/patches.json         {category: [{name, url}]}
--   data/casting.json         config/casting/*.json, paths rewritten
--                             to site-relative patch urls
--   data/calibration.json     config/calibration.json rekeyed to bank
--                             tails (Category/Name.fxp)
--   data/manifest.json        ordered album + per-piece endS/maxCh +
--                             global nInstances
--
-- site/engine/ receives the wasm build artifacts. Neither data/ nor
-- engine/ is committed; @otb bake-site@ regenerates both.
--
-- License: GPL-2.0-or-later.
module OTB.BakeSite
  ( BakeOpts (..)
  , runBakeSite
  , albumKey
  , patchUrl
  , pieceInstances
  , bakeCasting
  , bakeManifest
  ) where

import Control.Monad (filterM, forM, forM_, unless, when)
import Control.Concurrent (getNumCapabilities)
import Data.List (foldl', isInfixOf, isSuffixOf, sort, sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import OTB.Json
import OTB.MaestroAlign (pyRepr)
import System.Directory
  ( copyFileWithMetadata, createDirectoryIfMissing, doesDirectoryExist
  , doesFileExist, getFileSize, getHomeDirectory, getModificationTime
  , listDirectory, removeDirectoryRecursive, removeFile, renamePath )
import System.Environment (getEnvironment, lookupEnv)
import System.Exit (ExitCode (..), die)
import System.FilePath (takeBaseName, takeFileName, (</>))
import System.Process
  (CreateProcess (..), createProcess, proc, waitForProcess)

data BakeOpts = BakeOpts
  { boSite :: FilePath
  , boSurgeDir :: FilePath
  , boEmscriptenBin :: String
  , boPerfDir :: Maybe FilePath
  , boCastingDir :: FilePath
  , boCalibration :: FilePath
  , boSkipWasm :: Bool
  , boSkipPerf :: Bool
  , boSkipPatches :: Bool
  }

-- | Surge patch library roots, in precedence order (inherited from
-- the retired live patchboard).
patchDirs :: IO [FilePath]
patchDirs = do
  envd <- fromMaybe "" <$> lookupEnv "SURGE_PATCHES"
  home <- getHomeDirectory
  let cands =
        [ envd -- explicit override wins
        , "/usr/share/surge-xt/patches_factory"
        , "/usr/share/surge-xt/patches_3rdparty"
        , home </> "Library/Application Support/Surge XT/"
            <> "patches_factory"
        , "/Library/Application Support/Surge XT/patches_factory"
        -- a surge checkout works without installing Surge at all
        , home </> "code/surge/resources/data/patches_factory"
        ]
  filterM (\d -> if null d then pure False else doesDirectoryExist d)
    cands

-- | (category -> [(name, path)]): a system install and a surge
-- checkout carry the same factory bank; first tree wins per
-- (category, name) so nothing lists twice.
scanPatches :: IO (Map.Map String [(String, FilePath)])
scanPatches = do
  roots <- patchDirs
  (cats, _) <- foldForM (Map.empty, mempty) roots $ \(m0, s0) root -> do
    catNames <- sort <$> listDirectory root
    foldForM (m0, s0) catNames $ \(m, s) cat -> do
      let catdir = root </> cat
      isDir <- doesDirectoryExist catdir
      if not isDir || cat `elem` ["Tutorials", "Templates"]
        then pure (m, s)
        else do
          fs <- sort <$> listDirectory catdir
          pure $ foldl'
            (\(m', s') f ->
               if ".fxp" `isSuffixOf` f && not (elemPair (cat, f) s')
                 then ( Map.insertWith (flip (<>)) cat
                          [(take (length f - 4) f, catdir </> f)] m'
                      , (cat, f) : s' )
                 else (m', s'))
            (m, s) fs
  pure cats
  where
    foldForM z xs f = go z xs
      where
        go acc [] = pure acc
        go acc (x : rest) = f acc x >>= \acc' -> go acc' rest
    elemPair = elem

wasmFiles :: [String]
wasmFiles = ["surge-worklet.js", "surge-worklet.wasm", "worklet-shim.js"]

-- The emcmake configure from the fork's src/surge-wasm/README.md.
wasmCmakeFlags :: [String]
wasmCmakeFlags =
  [ "-DCMAKE_BUILD_TYPE=Release"
  , "-DSURGE_BUILD_32BIT_LINUX=TRUE"
  , "-DSURGE_SKIP_JUCE_FOR_RACK=TRUE"
  , "-DSURGE_SKIP_LUA=TRUE"
  , "-DSURGE_SKIP_ODDSOUND_MTS=TRUE"
  , "-DSURGE_BUILD_XT=OFF"
  , "-DSURGE_BUILD_FX=OFF"
  , "-DSURGE_BUILD_TESTRUNNER=OFF"
  , "-DSURGE_SKIP_WERROR=TRUE"
  , "-DENABLE_LTO=OFF"
  , "-DSURGE_BUILD_WASM=ON"
  ]

-- | Album order: the WTC first (prelude before fugue per key number),
-- then the chorales by number, then anything else alphabetically.
albumKey :: FilePath -> (Int, Int, Int, Int, String)
albumKey path =
  let b = takeBaseName path
   in if length b == 7 && take 3 b == "wtc"
        && (b !! 4) `elem` ("pf" :: String)
        then ( 0, read [b !! 3], read (drop 5 b)
             , if b !! 4 == 'p' then 0 else 1, "" )
        else case choraleNum b of
          Just n -> (1, n, 0, 0, "")
          Nothing
            | take 9 b == "offering-" -> (2, 0, 0, 0, b)
            | otherwise -> (3, 0, 0, 0, b)

choraleNum :: String -> Maybe Int
choraleNum b = case splitAt 4 b of
  ("chor", ds) | not (null ds), all (`elem` ("0123456789" :: String)) ds
    -> Just (read ds)
  _ -> Nothing

-- | The dropdown's section header for a slug.
groupOf :: String -> String
groupOf b
  | take 4 b == "wtc1" = "The Well-Tempered Clavier, Book I"
  | take 4 b == "wtc2" = "The Well-Tempered Clavier, Book II"
  | choraleNum b /= Nothing = "Chorales"
  | take 9 b == "offering-" = "Musical Offering"
  | otherwise = "Extras"

-- | Site-relative url for a bank patch:
-- data/patches/Category/Name.fxp.
patchUrl :: FilePath -> String
patchUrl abspath =
  let parts = splitOn '/' (map (\c -> if c == '\\' then '/' else c)
                             abspath)
      tail2 = drop (length parts - 2) parts
   in "data/patches/" <> intercalate1 "/" tail2
  where
    splitOn c = foldr step [[]]
      where
        step x acc@(cur : rest)
          | x == c = [] : acc
          | otherwise = (x : cur) : rest
        step _ [] = [[]]
    intercalate1 sep = foldr1 (\a b -> a <> sep <> b)

runProc :: [(String, String)] -> String -> [String] -> IO ExitCode
runProc env cmd args = do
  (_, _, _, h) <- createProcess (proc cmd args) {env = Just env}
  waitForProcess h

checkedProc :: [(String, String)] -> String -> [String] -> IO ()
checkedProc env cmd args = do
  code <- runProc env cmd args
  when (code /= ExitSuccess) $
    die (cmd <> " " <> unwords args <> " failed: " <> show code)

bakeWasm :: FilePath -> FilePath -> String -> IO ()
bakeWasm surgeDir engineDir emscriptenBin = do
  let srcDir = surgeDir </> "src" </> "surge-wasm"
      bridge = srcDir </> "surge-worklet.cpp"
  okBridge <- doesFileExist bridge
  unless okBridge $
    die ("no wasm bridge at " <> bridge <> " — is " <> surgeDir
           <> " the fork on branch wasm-headless-audioworklet?")
  bridgeSrc <- readFile bridge
  forM_ ["loadSCLString", "loadScenePatches"] $ \method ->
    unless (method `isInfixOf` bridgeSrc) $
      die ("surge-worklet.cpp lacks " <> method <> " — the static "
             <> "board needs SCL tuning and the scene-pair patch "
             <> "loader; update the fork branch")
  -- the submodule needs its emscripten guards (see the fork README)
  let sub = surgeDir </> "libs" </> "sst" </> "sst-plugininfra"
  env0 <- getEnvironment
  clean <- runProc env0 "git" ["-C", sub, "diff", "--quiet"]
  when (clean == ExitSuccess) $
    die ("sst-plugininfra submodule is unpatched; run:\n  git -C "
           <> sub <> " apply "
           <> "../../../src/surge-wasm/sst-plugininfra-emscripten"
           <> ".patch")

  let build = surgeDir </> "build-wasm"
      out = build </> "src" </> "surge-wasm"
      wasm = out </> "surge-worklet.wasm"
      env = if null emscriptenBin then env0
        else [ ( k, if k == "PATH"
                      then emscriptenBin <> ":" <> v else v )
             | (k, v) <- env0 ]
  haveWasm <- doesFileExist wasm
  stale <- if not haveWasm then pure True else do
    tw <- getModificationTime wasm
    tb <- getModificationTime bridge
    pure (tw < tb)
  when stale $ do
    haveBuild <- doesDirectoryExist build
    unless haveBuild $
      checkedProc env "emcmake"
        (["cmake", "-B", build, "-S", surgeDir] <> wasmCmakeFlags)
    ncpu <- getNumCapabilities
    checkedProc env "cmake"
      [ "--build", build, "-j", show (max 4 ncpu)
      , "--target", "surge-worklet" ]

  createDirectoryIfMissing True engineDir
  forM_ wasmFiles $ \f ->
    copyFileWithMetadata (out </> f) (engineDir </> f)
  size <- getFileSize wasm
  putStrLn ("engine: " <> show (length wasmFiles) <> " files ("
              <> show (size `div` 1024) <> " KB wasm)")

-- | Build into a clean staging directory and swap it in, so a re-bake
-- never inherits performances the source no longer produces. The
-- album action is injected (Main runs it in-process).
bakePerf :: (FilePath -> IO ()) -> Maybe FilePath -> FilePath -> IO ()
bakePerf albumInto perfDir dataDir = do
  let out = dataDir </> "perf"
      stage = out <> ".staging"
  haveStage <- doesDirectoryExist stage
  when haveStage $ removeDirectoryRecursive stage
  createDirectoryIfMissing True stage
  case perfDir of
    Just src -> do
      fs <- sort <$> listDirectory src
      let keep = [ f | f <- fs
                     , ".json" `isSuffixOf` f || ".scl" `isSuffixOf` f ]
      forM_ keep $ \f ->
        copyFileWithMetadata (src </> f) (stage </> f)
      putStrLn ("perf: copied " <> show (length keep)
                  <> " files from " <> src)
    Nothing -> do
      -- house rule: the committed board regenerates from HEAD
      albumInto stage
      fs <- listDirectory stage
      forM_ [f | f <- fs, ".mid" `isSuffixOf` f] $ \f ->
        removeFile (stage </> f)
      putStrLn "perf: regenerated via otb album"
  okScl <- doesFileExist (stage </> "w3.scl")
  unless okScl $ do
    removeDirectoryRecursive stage
    die ("no w3.scl landed in " <> stage)
  haveOut <- doesDirectoryExist out
  when haveOut $ removeDirectoryRecursive out
  renamePath stage out
  copyFileWithMetadata (out </> "w3.scl") (dataDir </> "w3.scl")

bakePatches :: FilePath -> IO ()
bakePatches dataDir = do
  cats <- scanPatches
  when (Map.null cats) $
    die "no Surge patch library found (patches_factory)"
  counts <- forM (Map.toAscList cats) $ \(cat, entries) -> do
    let outdir = dataDir </> "patches" </> cat
    createDirectoryIfMissing True outdir
    forM_ entries $ \(name, path) ->
      copyFileWithMetadata path (outdir </> name <> ".fxp")
    pure (length entries)
  let listing = JObj
        [ ( T.pack cat
          , JArr [ JObj [ ("name", JStr (T.pack name))
                        , ("url", JStr (T.pack (patchUrl path))) ]
                 | (name, path) <- entries ] )
        | (cat, entries) <- Map.toAscList cats ]
  TIO.writeFile (dataDir </> "patches.json")
    (dumpJson Nothing listing)
  -- the client's "(init)" is a real Init Saw: with two voices per
  -- synth (scene A/B), every scene needs actual patch bytes to merge
  -- from
  roots <- patchDirs
  inits <- filterM doesFileExist
    [r </> "Templates" </> "Init Saw.fxp" | r <- roots]
  case inits of
    (i : _) -> copyFileWithMetadata i (dataDir </> "init.fxp")
    [] -> die "no Templates/Init Saw.fxp in any patch library"
  putStrLn ("patches: " <> show (sum counts) <> " across "
              <> show (Map.size cats) <> " categories (+ init.fxp)")

-- | Rewrite casting + calibration from filesystem paths to bank urls.
-- Casting entries resolve like the live board's resolve_patch: the
-- Category/Name.fxp tail is the identity, the machine path is not.
bakeCasting :: FilePath -> FilePath -> FilePath -> IO ()
bakeCasting dataDir castingDir calibrationFile = do
  haveDir <- doesDirectoryExist castingDir
  castings <- if not haveDir then pure [] else do
    fs <- sort <$> listDirectory castingDir
    fmap concat . forM [f | f <- fs, ".json" `isSuffixOf` f] $ \f -> do
      raw <- TIO.readFile (castingDir </> f)
      case parseJson raw of
        Left e -> die (castingDir </> f <> ": " <> e)
        Right (JObj kvs) -> do
          -- keys are slot names (the static board's own vocabulary)
          -- or channel numbers (the live board's); "_" comments drop
          let entry =
                [ (k, JStr (T.pack (patchUrl (T.unpack p))))
                | (k, JStr p) <- kvs
                , T.all (`elem` ("0123456789" :: String)) k
                    && not (T.null k)
                    || k `elem` ["bass", "tenor", "alto", "soprano"] ]
          pure [ (T.pack (take (length f - 5) f), JObj entry)
               | not (null entry) ]
        Right _ -> die (castingDir </> f <> ": not an object")
  TIO.writeFile (dataDir </> "casting.json")
    (dumpJson (Just 1) (JObj castings))

  haveCal <- doesFileExist calibrationFile
  cal <- if not haveCal then pure [] else do
    raw <- TIO.readFile calibrationFile
    case parseJson raw of
      Right (JObj kvs) ->
        pure [ (T.pack (patchUrl (T.unpack p)), v) | (p, v) <- kvs ]
      _ -> die (calibrationFile <> ": not an object")
  TIO.writeFile (dataDir </> "calibration.json")
    (dumpJson (Just 1) (JObj cal))
  putStrLn ("casting: " <> show (length castings) <> " castings, "
              <> show (length cal) <> " calibrations")

-- | Synth instances a piece needs: mirrors site/routing.js slotMap +
-- lanePlacement — channels ranked bass-first by mean pitch, spread
-- over four register slots, lanes paired only WITHIN a slot (an
-- instance's two scenes wear the same preset). truncate (x + 0.5),
-- not round: JS Math.round is half-UP where round is banker's — they
-- disagree exactly at the .5 ranks.
pieceInstances :: [(Int, Int)] -> Int
pieceInstances chPitch =
  let sums = foldl'
        (\m (c, p) -> Map.insertWith addPair c (p, 1 :: Int) m)
        Map.empty chPitch
      addPair (p1, c1) (p2, c2) = (p1 + p2, c1 + c2)
      chans = map fst $ sortOn
        (\(c, (s, k)) ->
           (fromIntegral s / fromIntegral k :: Double, c))
        (Map.toList sums)
      n = length chans
      slotOf r = if n == 1 then 3
        else truncate ((fromIntegral r * 3
                          / fromIntegral (n - 1) :: Double) + 0.5)
      perSlot = foldl'
        (\m r -> Map.insertWith (+) (slotOf r :: Int) (1 :: Int) m)
        Map.empty [0 .. n - 1]
   in sum [(c + 1) `div` 2 | c <- Map.elems perSlot]

bakeManifest :: FilePath -> IO ()
bakeManifest dataDir = do
  let perfDir = dataDir </> "perf"
  fs <- listDirectory perfDir
  let paths = sortOn albumKey
        [perfDir </> f | f <- fs, ".json" `isSuffixOf` f]
  (pieces, nInst) <- foldPieces paths
  let manifest = JObj
        [ ("pieces", JArr pieces)
        , ("nInstances", JNum (T.pack (show nInst)))
        , ("scl", JStr "data/w3.scl") ]
  TIO.writeFile (dataDir </> "manifest.json")
    (dumpJson (Just 1) manifest)
  putStrLn ("manifest: " <> show (length pieces) <> " pieces, "
              <> show nInst <> " synth instances")
  where
    foldPieces paths = go paths [] 0
      where
        go [] acc n = pure (reverse acc, n)
        go (p : rest) acc n = do
          raw <- TIO.readFile p
          perf <- either (die . ((p <> ": ") <>)) pure (parseJson raw)
          let name = fromMaybe (T.pack (takeBaseName p))
                (jStr =<< jLookup "piece" perf)
              mtitle = jStr =<< jLookup "title" perf
              msct = jStr =<< jLookup "sct" perf
              notes =
                [ nt | tr <- jArrOf (fromMaybe (JArr [])
                                       (jLookup "tracks" perf))
                     , nt <- jArrOf tr ]
              numOf k nt = fromMaybe 0 (jNum =<< jLookup k nt)
              intOf k nt = truncate (numOf k nt) :: Int
              end0 = maximum (0.0 :
                [numOf "onS" nt + numOf "durS" nt | nt <- notes])
              end = max end0 (fromMaybe 0.0
                      (jNum =<< jLookup "endS" perf))
              chs = [intOf "ch" nt | nt <- notes]
              inst = pieceInstances
                [(intOf "ch" nt, intOf "pitch" nt) | nt <- notes]
              entry = JObj $
                [ ("name", JStr name)
                , ("group", JStr (T.pack (groupOf (T.unpack name)))) ]
                <> [ ("title", JStr v) | Just v <- [mtitle] ]
                <> [ ("sct", JStr v) | Just v <- [msct] ]
                <>
                [ ("url", JStr (T.pack ("data/perf/"
                                          <> takeFileName p)))
                , ("endS", JNum (T.pack (pyRepr (roundD 3 end))))
                , ("maxCh", JNum (T.pack
                    (show (maximum ((-1) : chs) + 1)))) ]
          go rest (entry : acc) (max n inst)
    roundD d x =
      let m = 10 ^^ (d :: Int)
       in fromIntegral (round (x * m) :: Integer) / m

-- | The whole bake; the album action is injected by Main so the IR
-- regeneration stays in-process.
runBakeSite :: (FilePath -> IO ()) -> BakeOpts -> IO ()
runBakeSite albumInto o = do
  let dataDir = boSite o </> "data"
  createDirectoryIfMissing True dataDir
  unless (boSkipWasm o) $
    bakeWasm (boSurgeDir o) (boSite o </> "engine")
      (boEmscriptenBin o)
  unless (boSkipPerf o) $ bakePerf albumInto (boPerfDir o) dataDir
  unless (boSkipPatches o) $ bakePatches dataDir
  bakeCasting dataDir (boCastingDir o) (boCalibration o)
  bakeManifest dataDir
  putStrLn ("baked: " <> boSite o)
