-- | Unit tests plus the M1 corpus sweep.
--
-- The sweep needs corpus/bach-wtc (gitignored); when absent it degrades to
-- a no-op with a note, the bcrsim ZAQ_SYX pattern — tests must run on a
-- bare clone.
--
-- License: GPL-2.0-or-later.
module Main (main) where

import Control.Monad (forM)
import Data.List (isSuffixOf, sort)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import OTB.Analysis.Counterpoint (parallelPerfects)
import OTB.Kern.Lexer (lexNoteTok)
import OTB.Kern.Parser (parseKern)
import OTB.Kern.Token (NoteTok (..), Tie (..))
import OTB.Score (Score (..), ScoreNote (..), Voice (..))
import OTB.Units (Bpm (..))
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit

corpusDir :: FilePath
corpusDir = "corpus/bach-wtc/kern"

main :: IO ()
main = do
  sweep <- corpusSweep
  defaultMain $ testGroup "all" [units, sweep]

-- | Parse every WTC file; assert full coverage and the known-baseline
-- number of tie leftovers (encoding lapses in the corpus itself — see
-- Parser.hs). Run counterpoint as an oracle: Bach doesn't write parallel
-- perfects in fugues; a blowup here means the parser mangled voices.
corpusSweep :: IO TestTree
corpusSweep = do
  present <- doesDirectoryExist corpusDir
  if not present
    then pure $ testCase "corpus sweep (SKIPPED: corpus not cloned)" (pure ())
    else do
      files <- sort . filter (".krn" `isSuffixOf`) <$> listDirectory corpusDir
      results <- forM files $ \f -> do
        src <- TIO.readFile (corpusDir </> f)
        pure (f, parseKern (Bpm 72) src)
      pure $ testGroup "corpus sweep"
        [ testCase "all 96 files parse" $ do
            let failures = [f <> ": " <> e | (f, Left e) <- results]
            assertEqual (unlines failures) 96 (length [() | (_, Right _) <- results])
        , testCase "tie-leftover baseline holds (corpus encoding lapses)" $ do
            -- exactly 31 unterminated ties across 16 files at corpus commit
            -- 0b4f4d8 — chord subtokens missing their ] (see Parser.hs).
            -- Any change to this number is a parser regression.
            let leftovers = sum [scTieLeftovers s | (_, Right s) <- results]
            assertEqual "leftover notes" 31 leftovers
        , testCase "counterpoint oracle: fugues stay under threshold" $ do
            -- Bach doesn't write parallel perfects — mostly. The ceiling is
            -- calibrated to the corpus: wtc1f10 (E minor, the only 2-voice
            -- fugue in Book I) peaks at 27 because it doubles its voices in
            -- octaves *on purpose* as texture. The oracle hunts parser
            -- blowups (voice duplication would score in the hundreds), not
            -- musicology.
            let bad =
                  [ f <> "=" <> show n
                  | (f, Right s) <- results
                  , length f > 4, f !! 4 == 'f' -- wtcNfMM.krn = fugue
                  , let n = parallelPerfects s
                  , n > 40
                  ]
            assertEqual (unlines bad) [] bad
        ]

units :: TestTree
units = testGroup "otb"
  [ testGroup "lexer"
      [ testCase "middle c quarter" $ ntPitch (lexNoteTok "4c") @?= Just 60
      , testCase "C3" $ ntPitch (lexNoteTok "4C") @?= Just 48
      , testCase "C5 doubled letter" $ ntPitch (lexNoteTok "16cc") @?= Just 72
      , testCase "flat" $ ntPitch (lexNoteTok "8b-") @?= Just 70
      , testCase "sharp" $ ntPitch (lexNoteTok "8f#") @?= Just 66
      , testCase "rest" $ ntPitch (lexNoteTok "16r") @?= Nothing
      , testCase "dotted duration" $ ntDur (lexNoteTok "8.e") @?= (3 / 16)
      , testCase "tie open with stem junk" $ ntTie (lexNoteTok "[8.e\\") @?= TieOpen
      , testCase "tie close" $ ntTie (lexNoteTok "4e\\]") @?= TieClose
      ]
  , testGroup "parser"
      [ testCase "two-voice mini score" $ do
          let src = T.unlines
                [ "**kern\t**kern"
                , "*MM96\t*"
                , "4c\t4e"
                , "4d\t4f"
                , "*-\t*-"
                ]
          case parseKern (Bpm 72) src of
            Left e -> assertFailure e
            Right (Score (Bpm t) vs 0) -> do
              t @?= 96
              map (length . vNotes) vs @?= [2, 2]
            Right s -> assertFailure ("unexpected shape: " <> show s)
      , testCase "tie merges duration" $ do
          let src = T.unlines
                [ "**kern"
                , "[4c"
                , "4c]"
                , "*-"
                ]
          case parseKern (Bpm 72) src of
            Left e -> assertFailure e
            Right (Score _ [Voice _ [n]] 0) ->
              snDur n @?= (1 / 2)
            Right s -> assertFailure ("unexpected shape: " <> show s)
      ]
  ]
