-- | The edition layer. The corpus is external and unversioned, and some
-- of its files carry encoder notes requesting manual edits (wtc1p08's
-- RWG record: "Appoggiaturas ... need manual editing in measures: 36").
-- An edition in @editions/@ next to the config file IS that edit,
-- versioned in this repo and preferred wherever kern is read.
--
-- Two guards keep the substitution honest:
--
--   * the lookup is anchored to the CONFIG's directory, not the process
--     working directory — the config path is already the run's one
--     repo-anchored input;
--   * an edition names the exact source it replaces via an
--     @!!!EDITION-OF-KEY:@ record matching the corpus file's own CCARH
--     @!!!KEY:@ checksum, so a stranger's file that merely shares the
--     basename — or an upstream corpus revision the edit no longer
--     applies to — is left untouched.
--
-- License: GPL-2.0-or-later.
module OTB.Edition (readKernSource) where

import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Directory (doesFileExist)
import System.FilePath (takeDirectory, takeFileName, (</>))

recordOf :: Text -> Text -> Maybe Text
recordOf name t = listToMaybe
  [T.strip r | l <- T.lines t, Just r <- [T.stripPrefix name l]]

-- | Read a kern source, substituting the config-adjacent edition when
-- one exists AND names this exact file's @!!!KEY:@ checksum.
readKernSource :: FilePath -> FilePath -> IO Text
readKernSource cfgPath path = do
  src <- TIO.readFile path
  let edition = takeDirectory cfgPath </> "editions" </> takeFileName path
  hasEdition <- doesFileExist edition
  if not hasEdition
    then pure src
    else do
      ed <- TIO.readFile edition
      let target = recordOf "!!!EDITION-OF-KEY:" ed
      pure $ if target /= Nothing && target == recordOf "!!!KEY:" src
               then ed
               else src
