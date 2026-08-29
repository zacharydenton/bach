-- | M0 smoke tests; the golden/corpus-sweep suites arrive in M1.
-- License: GPL-2.0-or-later.
module Main (main) where

import Data.Text qualified as T
import OTB.Kern.Lexer (lexNoteTok)
import OTB.Kern.Parser (parseKern)
import OTB.Kern.Token (NoteTok (..), Tie (..))
import OTB.Score (Score (..), ScoreNote (..), Voice (..))
import OTB.Units (Bpm (..))
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup "otb"
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
            Right (Score (Bpm t) vs) -> do
              t @?= 96
              map (length . vNotes) vs @?= [2, 2]
      , testCase "tie merges duration" $ do
          let src = T.unlines
                [ "**kern"
                , "[4c"
                , "4c]"
                , "*-"
                ]
          case parseKern (Bpm 72) src of
            Left e -> assertFailure e
            Right (Score _ [Voice _ [n]]) ->
              snDur n @?= (1 / 2)
            Right s -> assertFailure ("unexpected shape: " <> show s)
      ]
  ]
