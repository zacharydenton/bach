-- | The edition layer. The corpus is external and unversioned, and some
-- of its files carry encoder notes requesting manual edits (wtc1p08's
-- RWG record: "Appoggiaturas ... need manual editing in measures: 36").
-- An edition in @editions/@ next to the config file IS that edit,
-- versioned in this repo and preferred wherever kern is read.
--
-- Two guards keep the substitution honest:
--
--   * the lookup dir is explicit (otb --editions; defaults to
--     editions/ next to the config file) — never the process working
--     directory, and temp-dir configs can keep the real editions;
--   * an edition names the exact source it replaces via an
--     @!!!EDITION-OF-KEY:@ record matching the corpus file's own CCARH
--     @!!!KEY:@ checksum, so a stranger's file that merely shares the
--     basename — or an upstream corpus revision the edit no longer
--     applies to — is left untouched.
--
-- License: GPL-2.0-or-later.
module OTB.Edition (editionsFor, readKernSource) where

import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Directory (doesFileExist)
import System.FilePath (takeDirectory, takeFileName, (</>))

recordOf :: Text -> Text -> Maybe Text
recordOf name t = listToMaybe
  [T.strip r | l <- T.lines t, Just r <- [T.stripPrefix name l]]

-- | The default editions root for a given config path.
editionsFor :: FilePath -> FilePath
editionsFor cfgPath = takeDirectory cfgPath </> "editions"

-- | Read a kern source, substituting the edition from @dir@ when one
-- exists AND names this exact file's @!!!KEY:@ checksum.
readKernSource :: FilePath -> FilePath -> IO Text
readKernSource dir path = do
  src <- TIO.readFile path
  let edition = dir </> takeFileName path
  hasEdition <- doesFileExist edition
  if not hasEdition
    then pure src
    else do
      ed <- TIO.readFile edition
      let target = recordOf "!!!EDITION-OF-KEY:" ed
      pure $ if target /= Nothing && target == recordOf "!!!KEY:" src
               then ed
               else src
