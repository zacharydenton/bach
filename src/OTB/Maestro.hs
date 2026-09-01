-- | Fetch the MAESTRO v3 MIDI archive and catalog its WTC
-- performances. Ports tools/maestro_fetch.py.
--
-- MAESTRO (piano-e-competition Disklavier captures) is ASAP's parent:
-- real performed MIDI with real velocities. ASAP aligned only part of
-- it; the aligner (OTB.MaestroAlign) does the rest. This module
--
--   1. downloads maestro-v3.0.0-midi.zip (~57 MB, checksum-verified)
--      into corpus/maestro/ and extracts ONLY the WTC-referenced files
--      (curl / sha256sum / unzip subprocesses — no new library deps);
--   2. writes corpus/maestro/wtc-catalog.tsv — every MAESTRO WTC row,
--      joined against corpus/asap/metadata.csv's
--      maestro_midi_performance column so performances ASAP already
--      aligned are flagged (their ground truth validates the aligner);
--   3. names the candidate otb pieces per file (a competition file may
--      hold the prelude, the fugue, or both — segmentation is decided
--      later, by the alignment itself).
--
-- The catalog is TSV rather than the Python's JSON purely so the
-- reader stays a two-line splitOn; same rows, same order.
--
-- License: GPL-2.0-or-later.
module OTB.Maestro
  ( CatalogEntry (..)
  , maestroDir
  , asapMetaPath
  , parseCsv
  , csvDicts
  , pieceNames
  , titleKeyBwv
  , folderPieces
  , buildCatalog
  , writeCatalog
  , readCatalog
  , runMaestroFetch
  ) where

import Control.Monad (forM_, unless, when)
import Data.Char (isDigit)
import Data.List (foldl', nub, sort, sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Directory
  (createDirectoryIfMissing, doesFileExist, renameFile)
import System.Exit (die)
import System.FilePath (takeDirectory, (</>))
import System.Process (callProcess, readProcess)

maestroDir :: FilePath
maestroDir = "corpus" </> "maestro"

asapMetaPath :: FilePath
asapMetaPath = "corpus" </> "asap" </> "metadata.csv"

zipUrl, csvUrl :: String
zipUrl = "https://storage.googleapis.com/magentadata/datasets/maestro/\
         \v3.0.0/maestro-v3.0.0-midi.zip"
csvUrl = "https://storage.googleapis.com/magentadata/datasets/maestro/\
         \v3.0.0/maestro-v3.0.0.csv"

-- sha256 of maestro-v3.0.0-midi.zip, recorded at first fetch
-- 2026-09-01 (Google's canonical distribution publishes no digest;
-- this pins ours)
zipSha256 :: String
zipSha256 =
  "70470ee253295c8d2c71e6d9d4a815189e35c89624b76d22fce5a019d5dde12c"

-- ---------------------------------------------------------------------
-- CSV (RFC-4180 enough for MAESTRO/ASAP metadata: quoted fields with
-- embedded commas and doubled quotes; no embedded newlines occur)

parseCsv :: Text -> [[Text]]
parseCsv = map row . filter (not . T.null) . T.lines
  where
    row = go []
      where
        -- each call consumes exactly one field; recursion only after
        -- a consumed separator, so trailing empty fields survive
        go acc t
          | not (T.null t), T.head t == '"' =
              let (fld, rest) = quoted [] (T.drop 1 t)
                  rest' = T.drop 1 rest -- the closing quote
               in if "," `T.isPrefixOf` rest'
                    then go (fld : acc) (T.drop 1 rest')
                    else reverse (fld : acc)
          | otherwise =
              let (fld, rest) = T.breakOn "," t
               in if T.null rest
                    then reverse (fld : acc)
                    else go (fld : acc) (T.drop 1 rest)
        quoted parts t =
          let (chunk, rest) = T.breakOn "\"" t
           in if "\"\"" `T.isPrefixOf` rest
                then quoted (chunk <> "\"" : parts) (T.drop 2 rest)
                else (T.concat (reverse (chunk : parts)), rest)

csvDicts :: Text -> [[(Text, Text)]]
csvDicts t = case parseCsv t of
  [] -> []
  (hdr : rows) -> map (zip hdr) rows

field :: Text -> [(Text, Text)] -> Text
field k = fromMaybe "" . lookup k

-- ---------------------------------------------------------------------
-- WTC naming

-- | otb piece slugs for a BWV number: (prelude, fugue).
pieceNames :: Int -> (String, String)
pieceNames bwv =
  let (book, num) = if bwv <= 869
        then (1 :: Int, bwv - 845) else (2, bwv - 869)
      pad n = (if n < 10 then "0" else "") <> show n
   in ( "wtc" <> show book <> "p" <> pad num
      , "wtc" <> show book <> "f" <> pad num )

-- WTC ordering: number = chromatic position of the key, major first
keyNum :: [(( Text, Text), Int)]
keyNum =
  [ (("C", "maj"), 1), (("C", "min"), 2), (("C-sharp", "maj"), 3)
  , (("C-sharp", "min"), 4), (("D-flat", "maj"), 3)
  , (("D", "maj"), 5), (("D", "min"), 6)
  , (("E-flat", "maj"), 7), (("E-flat", "min"), 8)
  , (("D-sharp", "min"), 8), (("E", "maj"), 9), (("E", "min"), 10)
  , (("F", "maj"), 11), (("F", "min"), 12)
  , (("F-sharp", "maj"), 13), (("F-sharp", "min"), 14)
  , (("G", "maj"), 15), (("G", "min"), 16)
  , (("A-flat", "maj"), 17), (("G-sharp", "min"), 18)
  , (("A", "maj"), 19), (("A", "min"), 20)
  , (("B-flat", "maj"), 21), (("B-flat", "min"), 22)
  , (("B", "maj"), 23), (("B", "min"), 24) ]

-- | BWV inferred from "in \<Key\> \<Major|Minor\>" + "WTC \<I|II\>" —
-- the titles' explicit BWV numbers are sometimes WRONG (a 2014 file
-- says "BWV 846" and "WTC II" in one breath; ASAP proves it is 870),
-- so the key+book reading is computed independently.
titleKeyBwv :: Text -> Maybe Int
titleKeyBwv title = do
  (key, mode) <- findKey
  book <- findBook
  num <- lookup (key, mode) keyNum
  pure ((if book == (1 :: Int) then 845 else 869) + num)
  where
    ws = T.words (T.replace "  " " " title)
    findBook
      | any (`T.isInfixOf` title) ["WTC II", "WTCII", "Book II"] =
          Just 2
      | any (`T.isInfixOf` title) ["WTC I", "WTCI", "Book I"] = Just 1
      | otherwise = Nothing
    findKey = go ws
      where
        go (a : rest@(b : c)) | a == "in" =
          case pick b c of
            Just r -> Just r
            Nothing -> go rest
        go (_ : rest) = go rest
        go [] = Nothing
        pick b c =
          let (letter, accid) = T.break (== '-') b
              norm = case c of
                (acc : d : _)
                  | acc `elem` ["flat", "sharp"], T.null accid ->
                      Just (letter <> "-" <> acc, d)
                _ -> Just (b, head' c)
           in do
                (key, modeW) <- norm
                _ <- if T.length letter == 1
                       && T.head letter `elem` ("ABCDEFG" :: String)
                       then Just () else Nothing
                mode <- case modeW of
                  m | "Major" `T.isPrefixOf` m -> Just "maj"
                    | "Minor" `T.isPrefixOf` m -> Just "min"
                  _ -> Nothing
                pure (key, mode)
        head' (x : _) = x
        head' [] = ""

-- | "Bach/Fugue/bwv_870" -> ["wtc2f01"].
folderPieces :: Text -> [String]
folderPieces folder = case T.splitOn "/" folder of
  ["Bach", kind, bwvT]
    | Just bwvS <- T.stripPrefix "bwv_" bwvT
    , T.all isDigit bwvS ->
        let bwv = read (T.unpack bwvS)
            (pre, fug) = pieceNames bwv
         in [if kind == "Prelude" then pre else fug]
  _ -> []

-- ---------------------------------------------------------------------
-- catalog

data CatalogEntry = CatalogEntry
  { ceMidi :: !Text
  , ceBwv :: !Int
  , ceYear :: !Text
  , ceDuration :: !Text
  , ceConflict :: !Bool
  , ceCandidates :: [String]
  , ceAsapFolders :: [Text]
  , ceTitle :: !Text
  }

-- | maestro paths ASAP already aligned -> [asap folders].
asapUsed :: IO (Map.Map Text [Text])
asapUsed = do
  ok <- doesFileExist asapMetaPath
  if not ok then pure Map.empty else do
    rows <- csvDicts <$> TIO.readFile asapMetaPath
    pure $ foldl'
      (\m r ->
         let mp = T.strip (field "maestro_midi_performance" r)
          in if T.null mp then m
             else Map.insertWith (flip (<>))
                    (T.replace "{maestro}/" "" mp)
                    [field "folder" r] m)
      Map.empty rows

buildCatalog :: [[(Text, Text)]] -> Map.Map Text [Text]
             -> [CatalogEntry]
buildCatalog metaRows used =
  [ entry bwv r | (bwv, r) <- sortOn key wtcRows ]
  where
    wtcRows = mapMaybe pick metaRows
    pick r = do
      let title = field "canonical_title" r
      bwv <- bwvOfTitle title
      if 846 <= bwv && bwv <= 893 then Just (bwv, r) else Nothing
    bwvOfTitle title = go (T.words title)
      where
        go ("BWV" : n : _)
          | Just b <- readIntT (T.takeWhile isDigit n)
          , 840 <= b, b <= 899 = Just b
        go (_ : rest) = go rest
        go [] = Nothing
    key (bwv, r) = (bwv, field "midi_filename" r)
    entry bwv r =
      let title = field "canonical_title" r
          rel = field "midi_filename" r
          folders = Map.findWithDefault [] rel used
          kb = titleKeyBwv title
          bwvs = sort (nub (bwv : maybe [] pure kb))
          hasP = "Prelude" `T.isInfixOf` title
                   || not ("Fugue" `T.isInfixOf` title)
          hasF = "Fugue" `T.isInfixOf` title
                   || not ("Prelude" `T.isInfixOf` title)
          cands0 = concat
            [ [pre | hasP] <> [fug | hasF]
            | b <- bwvs, let (pre, fug) = pieceNames b ]
          -- ASAP's mapping is definitive where present
          cands = foldl'
            (\acc p -> if p `elem` acc then acc else acc <> [p])
            cands0 (concatMap folderPieces folders)
       in CatalogEntry rel bwv (field "year" r) (field "duration" r)
            (maybe False (/= bwv) kb) cands folders title

readIntT :: Text -> Maybe Int
readIntT t
  | not (T.null t), T.all isDigit t = Just (read (T.unpack t))
  | otherwise = Nothing

catalogPath :: FilePath
catalogPath = maestroDir </> "wtc-catalog.tsv"

writeCatalog :: [CatalogEntry] -> IO ()
writeCatalog entries =
  TIO.writeFile catalogPath . T.unlines $
    hdr : map line entries
  where
    hdr = T.intercalate "\t"
      [ "midi", "bwv", "year", "duration", "title_bwv_conflict"
      , "candidates", "asap_folders", "title" ]
    line e = T.intercalate "\t"
      [ ceMidi e, T.pack (show (ceBwv e)), ceYear e, ceDuration e
      , if ceConflict e then "1" else "0"
      , T.intercalate "," (map T.pack (ceCandidates e))
      , T.intercalate "," (ceAsapFolders e)
      , ceTitle e ]

readCatalog :: IO [CatalogEntry]
readCatalog = do
  ok <- doesFileExist catalogPath
  unless ok $ die (catalogPath
    <> " missing — run otb maestro-fetch (or --catalog) first")
  (_ : rows) <- T.lines <$> TIO.readFile catalogPath
  pure (map parse rows)
  where
    parse row = case T.splitOn "\t" row of
      (midi : bwv : year : dur : confl : cands : folders : title) ->
        CatalogEntry midi (fromMaybe 0 (readIntT bwv)) year dur
          (confl == "1")
          (map T.unpack (splitCsvish cands))
          (splitCsvish folders)
          (T.intercalate "\t" title)
      _ -> CatalogEntry row 0 "" "" False [] [] ""
    splitCsvish t = [x | x <- T.splitOn "," t, not (T.null x)]

-- ---------------------------------------------------------------------
-- fetch

runMaestroFetch :: Bool -> IO ()
runMaestroFetch catalogOnly = do
  createDirectoryIfMissing True maestroDir
  let csvPath = maestroDir </> "maestro-v3.0.0.csv"
      zipPath = maestroDir </> "maestro-v3.0.0-midi.zip"
  unless catalogOnly $ do
    haveCsv <- doesFileExist csvPath
    unless haveCsv $ do
      putStrLn "fetching metadata csv…"
      callProcess "curl" ["-fsSL", "-o", csvPath, csvUrl]
    haveZip <- doesFileExist zipPath
    unless haveZip $ do
      putStrLn "fetching MIDI archive (~57 MB)…"
      callProcess "curl" ["-fsSL", "-o", zipPath, zipUrl]
    digest <- takeWhile (/= ' ')
                <$> readProcess "sha256sum" [zipPath] ""
    when (digest /= zipSha256) $
      die ("archive digest mismatch: " <> digest)

  rows <- csvDicts <$> TIO.readFile csvPath
  used <- asapUsed
  let catalog = buildCatalog rows used

  -- the archive stores files under a maestro-v3.0.0/ prefix
  listing <- readProcess "unzip" ["-Z1", zipPath] ""
  let prefix = case [l | l <- lines listing
                       , ".csv" `T.isSuffixOf` T.pack l] of
        (l : _) | '/' `elem` l ->
          reverse (dropWhile (/= '/') (reverse l))
        _ -> "maestro-v3.0.0/"

  -- extract binary-safe: unzip into the maestro dir (it lands under
  -- the archive prefix), then move each file into its rel path
  missing <- fmap concat . forM' catalog $ \e -> do
    let rel = T.unpack (ceMidi e)
    have <- doesFileExist (maestroDir </> rel)
    pure [rel | not have]
  let missing' = nub missing
  unless (null missing') $ do
    callProcess "unzip"
      (["-qq", "-o", zipPath, "-d", maestroDir]
         <> map (prefix <>) missing')
    forM_ missing' $ \rel -> do
      let dest = maestroDir </> rel
      createDirectoryIfMissing True (takeDirectory dest)
      renameFile (maestroDir </> prefix </> rel) dest
  let extracted = length missing'

  writeCatalog catalog
  let inAsap = length [() | e <- catalog, not (null (ceAsapFolders e))]
  putStrLn ("catalog: " <> show (length catalog)
    <> " WTC performances (" <> show inAsap
    <> " already in ASAP — the aligner's validation set), "
    <> show extracted <> " newly extracted -> " <> catalogPath)
  where
    forM' = flip mapM
