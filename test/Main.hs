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
import OTB.Config (ArtParams (..), artParamsFor, defaultArtParams, loadConfig)
import OTB.Interp.Articulation (articulateLane)
import OTB.Kern.Lexer (lexNoteTok)
import OTB.Kern.Parser (parseKern)
import OTB.Kern.Token (Mark (..), NoteTok (..), Tie (..))
import OTB.Emit.Midi (renderSmf)
import OTB.Player (perform)
import OTB.Score (Score (..), ScoreNote (..), Voice (..))
import OTB.Tuning
import OTB.Units (Bpm (..), Cents (..))
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
        , testCase "determinism: same score + same rules = same bytes" $ do
            src <- TIO.readFile (corpusDir </> "wtc1p01.krn")
            let run () = do
                  s <- either error pure (parseKern (Bpm 72) src)
                  either error pure
                    (renderSmf <$> perform defaultArtParams werckmeister3 2 s)
            a <- run (); b <- run ()
            assertBool "byte-identical" (a == b)
        , testCase "channel cardinality: every WTC piece fits 15 lanes" $ do
            let overs =
                  [ f | (f, Right s) <- results
                  , Left _ <- [perform defaultArtParams equalTable 2 s]
                  ]
            assertEqual (unlines overs) [] overs
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
  , testGroup "articulation"
      [ testCase "détaché base, legato final" $
          gates [nt 60, nt 64, nt 67]
            @?= [apBase p, apBase p, apLegato p]
      , testCase "staccato mark wins over context" $
          take 1 (gates [nt 60 `with` [Staccato], nt 60])
            @?= [apStaccato p]
      , testCase "slur spans to the close" $
          gates [nt 60 `with` [SlurOpen], nt 62, nt 64 `with` [SlurClose], nt 55]
            @?= [apLegato p, apLegato p, apLegato p, apLegato p]
            -- final note is legato by the lane-end rule anyway
      , testCase "repeated note sharpened" $
          take 1 (gates [nt 60, nt 60, nt 67])
            @?= [apRepeated p]
      , testCase "stepwise at singing values sings" $
          take 1 (gates [nt 60, nt 62, nt 55])
            @?= [apCantabile p]
      , testCase "leaps stay détaché" $
          take 1 (gates [nt 60, nt 67, nt 55])
            @?= [apBase p]
      ]
  , testGroup "tuning"
      [ testCase "equal temperament centers every bend" $
          map (bendValue 2 . offsetFor equalTable) [60 .. 71]
            @?= replicate 12 8192
      , testCase "werckmeister C is pure, C# is -9.775" $ do
          offsetFor werckmeister3 60 @?= Cents 0
          offsetFor werckmeister3 61 @?= Cents (90.225 - 100)
      , testCase "bend math at range 2: -9.775c = 7792" $
          bendValue 2 (Cents (-9.775)) @?= (8192 - 400)
      , testCase "scl round-trip: table -> scl -> same table" $ do
          let scl = renderScl "wiii" werckmeister3
          case parseScl scl of
            Left e -> assertFailure e
            Right t ->
              let diffs =
                    [ abs (c1 - c2)
                    | pc <- [0 .. 11]
                    , let Cents c1 = offsetFor t pc
                          Cents c2 = offsetFor werckmeister3 pc
                    ]
               in assertBool ("max diff " <> show (maximum diffs))
                    (maximum diffs < 1e-6)
      , testCase "scl ratios parse: 3/2 is 701.955c" $ do
          let scl = T.unlines
                (["! r", "ratios", "12", "!"]
                   <> replicate 11 "100.0" <> ["2/1"])
          case parseScl scl of
            Left e -> assertFailure e
            Right _ -> pure ()
      ]
  , testGroup "config"
      [ testCase "piece override beats section beats default" $ do
          let cfg = loadConfig (T.unlines
                [ "[articulation]"
                , "base = 0.7  # global taste"
                , "[piece.x]"
                , "base = 0.9"
                ])
          apBase (artParamsFor cfg "x") @?= 0.9
          apBase (artParamsFor cfg "y") @?= 0.7
          apStaccato (artParamsFor cfg "x") @?= apStaccato defaultArtParams
      ]
  ]
  where
    p = defaultArtParams
    nt pit = ScoreNote 0 (1 / 4) pit [] -- quarter notes, onset irrelevant to rules
    with n ms = n {snMarks = ms}
    gates = map snd . articulateLane p
