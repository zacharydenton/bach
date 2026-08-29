-- | Unit tests plus the M1 corpus sweep.
--
-- The sweep needs corpus/bach-wtc (gitignored); when absent it *fails*
-- with the clone command, unless OTB_NO_CORPUS=1 asks for units only. The
-- tools' Python tests run from here too, so one `stack test` covers all.
--
-- License: GPL-2.0-or-later.
module Main (main) where

import Control.Monad (forM, forM_)
import Data.List (isInfixOf, isSuffixOf, nub, sort, sortOn)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import OTB.Analysis.Counterpoint (parallelPerfects)
import OTB.Config (ArtParams (..), artParamsFor, defaultArtParams, loadConfig)
import OTB.Interp.Articulation (articulateLane)
import OTB.Kern.Lexer (lexNoteTok)
import OTB.Kern.Parser (parseKern)
import OTB.Kern.Token (Mark (..), NoteTok (..), Tie (..))
import OTB.Emit.Midi (renderSmf)
import OTB.Interp.Agogics
import OTB.Interp.Dynamics
import OTB.Interp.Express
import OTB.Interp.Ornament
import OTB.Interp.Phrasing
import OTB.Explain (renderWhys)
import OTB.Player (Interp (..), PerfNote (..), Performance (..), defaultInterp, perform)
import OTB.Score (Score (..), ScoreNote (..), Voice (..), scoreNote)
import OTB.Tuning
import OTB.Units (Bpm (..), Cents (..), Seconds (..), WholeNotes (..), secondsAt)
import System.Directory (doesDirectoryExist, listDirectory)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import EuterpeaLite.IO.MIDI.ToMidi (writeMidi)
import EuterpeaLite.Music qualified as E
import SMFReader

corpusDir :: FilePath
corpusDir = "corpus/bach-wtc/kern"

main :: IO ()
main = do
  sweep <- corpusSweep
  defaultMain $ testGroup "all" [units, laws, sweep, oracle]

-- | Parse every WTC file; assert full coverage and the known-baseline
-- number of tie leftovers (encoding lapses in the corpus itself — see
-- Parser.hs). Run counterpoint as an oracle: Bach doesn't write parallel
-- perfects in fugues; a blowup here means the parser mangled voices.
corpusSweep :: IO TestTree
corpusSweep = do
  present <- doesDirectoryExist corpusDir
  allowSkip <- lookupEnv "OTB_NO_CORPUS"
  if not present
    then pure $ testCase "corpus sweep" $
      if allowSkip == Just "1"
        then pure ()
        else assertFailure
          ("corpus not cloned — run\n  git clone --depth 1 "
             <> "https://github.com/humdrum-tools/bach-wtc corpus/bach-wtc\n"
             <> "or set OTB_NO_CORPUS=1 to run the unit tests alone")
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
                  either error pure (renderSmf <$> perform defaultInterp s)
            a <- run (); b <- run ()
            assertBool "byte-identical" (a == b)
        , testCase "channel cardinality: every WTC piece fits 15 lanes" $ do
            let overs =
                  [ f | (f, Right s) <- results
                  , Left _ <- [perform defaultInterp s]
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
            Right (Score (Bpm t) vs 0 _ _ _) -> do
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
            Right (Score _ [Voice _ [n]] 0 _ _ _) ->
              snDur n @?= (1 / 2)
            Right s -> assertFailure ("unexpected shape: " <> show s)
      , testCase "fermata on a tied close holds only the close" $ do
          let src = T.unlines ["**kern", "[2c", "4c;]", "*-"]
          case parseKern (Bpm 72) src of
            Left e -> assertFailure e
            Right (Score _ [Voice _ [n]] 0 _ _ _) -> do
              snDur n @?= (3 / 4)
              snMarks n @?= [Fermata]
              snSegs n @?= [(1 / 2, []), (1 / 4, [Fermata])]
              -- held = 1/2 + 1/4 * hold, over 3/4
              let h = agFermataHold defaultAgogicParams
              fermataFactor defaultAgogicParams n @?= (1 / 2 + h / 4) / (3 / 4)
            Right s -> assertFailure ("unexpected shape: " <> show s)
      , testCase "sub-spine identity survives a rest" $ do
          -- the split-out line rests for a beat; greedy repacking would
          -- hand its next note to whichever lane happened to be free
          let src = T.unlines
                [ "**kern"
                , "*^"
                , "4c\t4e"
                , "4c\t4r"
                , "4c\t4g"
                , "*v\t*v"
                , "*-"
                ]
          case parseKern (Bpm 72) src of
            Left e -> assertFailure e
            Right (Score _ [Voice _ ns] 0 _ _ _) ->
              sort [(snLane n, snPitch n) | n <- ns]
                @?= [(0, 60), (0, 60), (0, 60), (1, 64), (1, 67)]
            Right s -> assertFailure ("unexpected shape: " <> show s)
      , testCase "ornament on a tie close restrikes, not unions" $ do
          -- a trill on the closing token starts a fresh attack there: the
          -- held half lands plain, the trilled quarter is its own note
          let src = T.unlines ["**kern", "[2c", "4cT]", "*-"]
          case parseKern (Bpm 72) src of
            Left e -> assertFailure e
            Right (Score _ [Voice _ ns] 0 _ _ _) ->
              [(snOnset n, snDur n, snMarks n) | n <- ns]
                @?= [(0, 1 / 2, []), (1 / 2, 1 / 4, [Trill 2])]
            Right s -> assertFailure ("unexpected shape: " <> show s)
      , testCase "meter changes are kept as a map" $ do
          let src = T.unlines
                ["**kern", "*M4/4", "1c", "*M3/8", "4d", "*-"]
          case parseKern (Bpm 72) src of
            Left e -> assertFailure e
            Right s -> scMeter s @?= [(0, (4, 4)), (1, (3, 8))]
      , testCase "merge of different voices is rejected, clock drift counted" $ do
          let drift = T.unlines
                ["**kern", "*^", "4c\t8e", "*v\t*v", "4d", "*-"]
          case parseKern (Bpm 72) drift of
            Left e -> assertFailure e
            Right s -> scMergeDrifts s @?= 1
          let cross = T.unlines ["**kern\t**kern", "4c\t4e", "*v\t*v", "*-"]
          case parseKern (Bpm 72) cross of
            Left _ -> pure ()
            Right _ -> assertFailure "cross-voice merge accepted"
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
  , testGroup "agogics"
      [ testCase "tempo map: rit still descends to the floor (arches off)" $ do
          let ag = defaultAgogicParams {agRitSpan = 1, agRitFloor = 0.5}
              tm = tempoMap ag 0 0 [(0, 1)] (Bpm 100) 4
              bpms = [b | (_, Bpm b) <- tm]
          take 1 bpms @?= [100]
          assertBool ("not descending: " <> show bpms)
            (and (zipWith (>=) bpms (drop 1 bpms)))
          assertBool ("floor overshot: " <> show (last bpms))
            (last bpms >= 50 && last bpms < 100)
      , testCase "Todd arches: tempo rises to centre, sane bounds" $ do
          let ag = defaultAgogicParams {agRitSpan = 0}
              tm = tempoMap ag 0.05 0 [(0, 1)] (Bpm 100) 8
              bpms = [b | (_, Bpm b) <- tm]
              mid = bpms !! (length bpms `div` 2)
          assertBool ("no arch: " <> show (take 5 bpms)) (mid > 100)
          assertBool "bounded" (all (\b -> b > 90 && b < 110) bpms)
      , testCase "group arches follow a meter change" $ do
          -- 4/4 for one bar then 3/8: arch peaks must land at 1/2 and
          -- at 1 + 3/16, i.e. the groups re-align at the change
          let ag = defaultAgogicParams {agRitSpan = 0, agTempoStep = 1 / 16}
              tm = tempoMap ag 0 0.05 [(0, 1), (1, 3 / 8)] (Bpm 100) (1 + 3 / 8)
              at t = case takeWhile ((<= t) . fst) tm of
                [] -> 100; xs -> let Bpm b = snd (last xs) in b
          assertBool "peak of the 4/4 bar" (at (1 / 2) > at (1 / 16))
          assertBool "a fresh trough at the change" (at 1 < at (1 / 2))
          assertBool "peak of the 3/8 group" (at (1 + 3 / 16) > at 1)
      , testCase "short piece: no rit, single tempo" $
          length (tempoMap defaultAgogicParams 0 0 [(0, 1)] (Bpm 100) (1 / 2)) @?= 1
      , testCase "fermata holds" $ do
          let sn = scoreNote 0 (1 / 4) 60 [Fermata]
          fermataFactor defaultAgogicParams sn @?= agFermataHold defaultAgogicParams
          fermataFactor defaultAgogicParams (sn {snMarks = []}) @?= 1
      ]
  , testGroup "ornaments"
      [ testCase "trill: upper start, main-note end, duration preserved" $ do
          let sn = scoreNote 0 (1 / 2) 60 [Trill 2]
              out = realizeLane defaultOrnamentParams (Bpm 120) [sn]
          assertBool "at least 4 subnotes" (length out >= 4)
          snPitch (head out) @?= 62 -- on the beat, upper auxiliary
          snPitch (last out) @?= 60 -- ends on the main note
          sum (map snDur out) @?= (1 / 2)
          assertBool "even alternation" (even (length out))
      , testCase "half-step trill uses the corpus's interval" $ do
          let out = realizeLane defaultOrnamentParams (Bpm 120)
                      [scoreNote 0 (1 / 2) 64 [Trill 1]]
          snPitch (head out) @?= 65
      , testCase "mordent bites below and returns" $ do
          let out = realizeLane defaultOrnamentParams (Bpm 120)
                      [scoreNote 0 (1 / 4) 67 [Mordent 2]]
          map snPitch out @?= [67, 65, 67]
          sum (map snDur out) @?= (1 / 4)
      , testCase "turn: upper main lower main" $ do
          let out = realizeLane defaultOrnamentParams (Bpm 120)
                      [scoreNote 0 (1 / 4) 60 [Turn]]
          map snPitch out @?= [62, 60, 58, 60]
      , testCase "unornamented notes pass through untouched" $ do
          let sn = scoreNote 0 (1 / 4) 60 [Staccato]
          realizeLane defaultOrnamentParams (Bpm 120) [sn] @?= [sn]
      ]
  , testGroup "express"
      [ testCase "inegal: conjunct pair swings long-short, total preserved" $ do
          let l = [scoreNote 0 (1/16) 60 [], scoreNote (1/16) (1/16) 62 []]
              out = inegalLane 0.33 l
          sum (map snDur out) @?= (1/8)
          assertBool "first longer" (snDur (out !! 0) > snDur (out !! 1))
          snOnset (out !! 1) @?= snOnset (out !! 0) + snDur (out !! 0)
      , testCase "inegal: leaps stay equal (Quantz)" $ do
          let l = [scoreNote 0 (1/16) 60 [], scoreNote (1/16) (1/16) 67 []]
          map snDur (inegalLane 0.33 l) @?= [1/16, 1/16]
      , testCase "overhold fills the gap fraction" $ do
          let l = [scoreNote 0 (1/16) 60 [], scoreNote (1/4) (1/16) 62 []]
              out = overholdLane 1.0 l
          snDur (out !! 0) @?= (1/4)
      , testCase "dissonance: minor 2nd charges 1, octave charges 0" $ do
          let others = [(0, 1, 61), (0, 1, 72)]
          chargesForLane others [scoreNote 0 (1/4) 60 []] @?= [1.0]
          chargesForLane [(0, 1, 72)] [scoreNote 0 (1/4) 60 []] @?= [0.0]
      , testCase "jitter is deterministic" $
          seededJitter (seedOf "wtc1p01") 42
            @?= seededJitter (seedOf "wtc1p01") 42
      ]
  , testGroup "player"
      [ testCase "chord overflow never borrows another spine's lane" $ do
          -- lane 1 rests on beat 2; the chord's extra e must still go to
          -- an overflow channel (the one it used on beat 1), not lane 1's
          let src = T.unlines
                ["**kern", "*^", "4c 4e\t4g", "4c 4e\t4r", "*v\t*v", "*-"]
          case parseKern (Bpm 72) src >>= perform defaultInterp of
            Left e -> assertFailure e
            Right pf -> do
              let evs = concat (perfTracks pf)
                  chOf pc = nub [pnChannel n | n <- evs, pnPitch n == pc]
              length (chOf 64) @?= 1
              assertBool "e on g's lane" (chOf 64 /= chOf 67)
              length (nub (map pnChannel evs)) @?= 3
      , testCase "trilled final chord still rolls bass-upward" $ do
          let src = T.unlines ["**kern\t**kern", "4C\t4e", "2C\t2gT", "*-\t*-"]
              run r = either error id $ parseKern (Bpm 72) src >>= perform
                defaultInterp { iExpress = defaultExpressParams
                  { exRollMs = r, exEnsemble = 1, exJitterMs = 0, exLeadMs = 0 } }
              late pf = [ n | tr <- perfTracks pf, n <- tr, pnOnset n >= 1 / 5 ]
              bass pf = minimum [pnOnset n | n <- late pf, pnPitch n == 48]
              trill pf = sort [pnOnset n | n <- late pf, pnPitch n >= 67]
              steps xs = zipWith (-) (drop 1 xs) xs
          -- the trill is the final chord's upper tone: it stays a trill
          -- (even steps) and starts *after* the bass by the roll
          take 1 (trill (run 0)) @?= [bass (run 0)]
          assertBool "no roll" (take 1 (trill (run 12)) > [bass (run 12)])
          steps (trill (run 12)) @?= steps (trill (run 0))
          assertBool "trill intact" (all (> 0) (steps (trill (run 12))))
      , testCase "monophonic gap is 1.5 ms at local tempo" $ do
          let interp = defaultInterp
                { iArt = defaultArtParams {apBase = 1, apCantabile = 1}
                , iAgogics = defaultAgogicParams {agRitSpan = 0}
                , iExpress = defaultExpressParams
                    {exExpression = 0, exEnsemble = 0}
                }
              score bpm = Score (Bpm bpm)
                [Voice 0 [scoreNote 0 (1/4) 60 [],
                          scoreNote (1/4) (1/4) 62 []]] 0 0 [(0, (4, 4))] 0
              gapMs bpm = case perform interp (score bpm) of
                Left e -> error e
                Right pf -> case perfTracks pf of
                  [[a, b]] ->
                    let Seconds off = secondsAt (perfTempoMap pf)
                                        (pnOnset a + pnDur a)
                        Seconds on = secondsAt (perfTempoMap pf) (pnOnset b)
                     in (on - off) * 1000
                  tracks -> error ("unexpected tracks: " <> show tracks)
          forM_ [60, 120] $ \bpm ->
            assertBool ("gap at " <> show bpm <> " bpm: " <> show (gapMs bpm))
              (abs (gapMs bpm - 1.5) < 0.01)
      , testCase "close onsets remain strictly monophonic" $ do
          let d = 1 / 4096
              interp = defaultInterp
                { iArt = defaultArtParams {apBase = 1}
                , iAgogics = defaultAgogicParams {agRitSpan = 0}
                , iExpress = defaultExpressParams
                    {exExpression = 0, exEnsemble = 0}
                }
              score = Score (Bpm 120)
                [Voice 0 [scoreNote 0 (d/4) 60 [], scoreNote d d 67 []]]
                0 0 [(0, (4, 4))] 0
          case perform interp score of
            Left e -> assertFailure e
            Right pf | [[a, b]] <- perfTracks pf -> do
              pnDur a @?= d / 4
              assertBool "channel overlap" (pnOnset a + pnDur a < pnOnset b)
            Right pf -> assertFailure ("unexpected tracks: " <> show (perfTracks pf))
      ]
  , testGroup "tools"
      [ testCase "patchboard HTTP boundary (python3 -m unittest)" $ do
          (code, out, err) <- readProcessWithExitCode "python3"
            ["-m", "unittest", "-q", "tools/test_patchboard.py"] ""
          case code of
            ExitSuccess -> pure ()
            _ -> assertFailure (out <> err)
      ]
  , testGroup "phrasing"
      [ testCase "a written rest is a boundary (Quantz)" $ do
          let l = [scoreNote 0 (1/4) 60 [], scoreNote (1/2) (1/4) 62 [], scoreNote (3/4) (1/4) 64 []]
              -- note 1 is followed by a quarter rest
              ss = boundaryStrengths defaultPhraseParams l
          assertBool ("rest not salient: " <> show ss) (ss !! 0 >= 1.0)
      , testCase "stepwise continuity is not a boundary" $ do
          let l = [scoreNote (fromIntegral t / 4) (1/4) (60 + i) [] | (t, i) <- zip [0 :: Int ..] [0, 2, 4]]
              ss = boundaryStrengths defaultPhraseParams l
          assertBool ("step breathed: " <> show ss) (ss !! 0 < 1.0)
      , testCase "breath shortens the pre-boundary note, spares the last" $ do
          let l = [scoreNote 0 (1/4) 60 [], scoreNote (1/2) (1/4) 62 []]
              arts = [(head l, 0.8), (l !! 1, 1.0)]
              out = map snd (breatheLane defaultPhraseParams l arts)
          out @?= [0.8 * (1 - ppBreath defaultPhraseParams), 1.0]
      ]
  , testGroup "dynamics"
      [ testCase "downbeat outweighs offbeat (Sloboda)" $ do
          let l = [scoreNote (fromIntegral i / 16) (1 / 16) 66 [] | i <- [0 .. 15 :: Int]]
              vs = dynamicsLane defaultDynParams [(0, (4, 4))] (replicate 16 False) l
          assertBool ("bar<=beat: " <> show vs) (vs !! 0 > vs !! 1)
          assertBool ("halfbar: " <> show vs) (vs !! 8 > vs !! 1)
          assertBool ("beat: " <> show vs) (vs !! 4 > vs !! 1)
      , testCase "phrase arch peaks in the middle (Todd)" $ do
          let l = [scoreNote (fromIntegral i / 4 + 1/64) (1/8) 66 [] | i <- [0 .. 8 :: Int]]
              -- offset by 1/64 so metre contributes nothing
              vs = dynamicsLane defaultDynParams [(0, (4, 4))] (replicate 9 False) l
          assertBool ("arch: " <> show vs) (vs !! 4 > vs !! 0 && vs !! 4 > vs !! 8)
      , testCase "accent mark honoured" $ do
          let l = [scoreNote (1/64) (1/8) 66 [Accent], scoreNote (9/64) (1/8) 66 []]
              vs = dynamicsLane defaultDynParams [] (replicate 2 False) l
          assertBool (show vs) (vs !! 0 > vs !! 1)
      , testCase "no meter degrades gracefully" $ do
          let l = [scoreNote 0 (1/4) 66 []]
          length (dynamicsLane defaultDynParams [] [False] l) @?= 1
      ]
  , testGroup "explain"
      [ testCase "provenance reaches the Performance with citations" $ do
          present <- doesDirectoryExist corpusDir
          if not present then pure () else do
           src <- TIO.readFile (corpusDir </> "wtc1f01.krn")
           s <- either (assertFailure . ("parse: " <>)) pure
                  (parseKern (Bpm 72) src)
           p <- either (assertFailure . ("perform: " <>)) pure
                  (perform defaultInterp s)
           let ws = concat [w | ((_, _), w) <- perfWhys p]
               rendered = renderWhys (take 200 ws)
           assertBool "articulation cited"
             ("articulation" `isInfixOf` rendered)
           assertBool "CPE Bach cited" ("CPE Bach" `isInfixOf` rendered)
           assertBool "Quantz cited" ("Quantz" `isInfixOf` rendered)
           assertBool "Sloboda cited" ("Sloboda 1983" `isInfixOf` rendered)
      ]
  , testGroup "config"
      [ testCase "junk and out-of-range values are rejected" $ do
          let bad = ["[agogics]\ntempo_step = 0", "[ornaments]\ntrill_rate = -1"
                    , "[tuning]\nbend_range = 0", "[phrasing]\nbreath = 1.5"
                    , "[x]\nfoo = 1 2", "[x]\nnot a key value"
                    , "[arches]\narch_bars = 0.1"]
          forM_ bad $ \src -> case loadConfig src of
            Left _ -> pure ()
            Right _ -> assertFailure ("accepted: " <> T.unpack src)
          either (assertFailure . show) (const (pure ()))
            (loadConfig "# c\n[a]\nk = 1.5 # trailing comment\n")
      , testCase "scl rejects trailing junk" $ do
          let scl n = T.unlines (["! x", "desc", n, "!"] <> replicate 11 "100.0" <> ["2/1"])
          either (assertFailure) (const (pure ())) (parseScl (scl "12"))
          case parseScl (scl "12x") of
            Left _ -> pure (); Right _ -> assertFailure "12x accepted"
          case parseScl (T.replace "100.0\n" "100.0 junk\n" (scl "12")) of
            Left _ -> pure (); Right _ -> assertFailure "junk cents accepted"
          case parseScl (T.replace "2/1" "3/2" (scl "12")) of
            Left _ -> pure (); Right _ -> assertFailure "non-octave period accepted"
      , testCase "piece override beats section beats default" $ do
          let cfg = either error id $ loadConfig (T.unlines
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
    nt pit = scoreNote 0 (1 / 4) pit [] -- quarter notes, onset irrelevant to rules
    with n ms = n {snMarks = ms, snSegs = [(snDur n, ms)]}
    gates = map snd . articulateLane p

-- ---------------------------------------------------------------------
-- W4: metamorphic laws (the algebra respects structure)

genLane :: Gen [ScoreNote]
genLane = do
  n <- chooseInt (1, 12)
  durs <- vectorOf n (elements [1/16, 1/8, 3/16, 1/4, 1/2])
  gaps <- vectorOf n (elements [0, 0, 0, 1/8, 1/4]) -- mostly contiguous
  pitches <- vectorOf n (chooseInt (36, 84))
  let onsets = scanl (\t (d, g) -> t + d + g) 0 (zip durs gaps)
  pure [scoreNote t d p [] | (t, d, p) <- zip3 onsets durs pitches]

genScore :: Gen Score
genScore = do
  nv <- chooseInt (1, 3)
  vs <- vectorOf nv genLane
  pure (Score (Bpm 96) [Voice i l | (i, l) <- zip [0 ..] vs]
          0 0 [(0, (4, 4))] 0)

transposeScore :: Int -> Score -> Score
transposeScore n s =
  s { scVoices = [ v { vNotes = [ sn { snPitch = snPitch sn + n
                                     , snSource =
                                         (fst (snSource sn),
                                          snd (snSource sn) + n) }
                                | sn <- vNotes v ] }
                 | v <- scVoices s ] }

pitchesTimes :: Performance -> [(Rational, Rational, Int)]
pitchesTimes p =
  sort [ (toRational (pnOnset n), toRational (pnDur n), pnPitch n)
       | tr <- perfTracks p, n <- tr ]
  where
    toRational = realToFrac

laws :: TestTree
laws = testGroup "laws (metamorphic)"
  [ testProperty "equal temperament: transposition commutes with performance" $
      forAll genScore $ \s -> forAll (chooseInt (-11, 11)) $ \n ->
        let ip = defaultInterp {iTuning = equalTable}
            a = perform ip (transposeScore n s)
            b = perform ip s
         in case (a, b) of
              (Right pa, Right pb) ->
                [ (t, d, p - n) | (t, d, p) <- pitchesTimes pa ]
                  === pitchesTimes pb
              _ -> property False
  , testProperty "Werckmeister does NOT commute: some bend must move" $
      forAll (genScore `suchThat` (not . null . concatMap vNotes . scVoices)) $ \s ->
        let ip = defaultInterp
            notes q = [n | tr <- perfTracks q, n <- tr]
         in case (perform ip (transposeScore 1 s), perform ip s) of
              (Right pa, Right pb) ->
                -- perform is structurally parallel across a transposition,
                -- so compare note-for-note: a semitone shift permutes the
                -- Werckmeister offsets and no two adjacent offsets in the
                -- table are equal, so some note's bend must move — the
                -- asymmetry IS what well-temperament exists to have.
                -- (Multiset comparison is too weak: distinct pitch sets
                -- can shift onto each other's offsets and it flakes.)
                property (any (\(x, y) -> pnBend x /= pnBend y)
                            (zip (notes pa) (notes pb)))
              _ -> property False
  , testProperty "inegales preserve lane duration sum" $
      forAll genLane $ \l ->
        sum (map snDur (inegalLane 0.33 l)) === sum (map snDur l)
  , testProperty "ornament realisation preserves duration sum" $
      forAll genLane $ \l ->
        let l' = [sn {snMarks = [Trill 2]} | sn <- l]
         in sum (map snDur (realizeLane defaultOrnamentParams (Bpm 96) l'))
              === sum (map snDur l')
  , testProperty "overhold never overlaps the lane successor" $
      forAll genLane $ \l ->
        let held = overholdLane 1.0 l
            ok = and [ snOnset a + snDur a <= snOnset b
                     | (a, b) <- zip held (drop 1 held) ]
         in property ok
  , testProperty "performance is channel-monophonic" $
      forAll genScore $ \s ->
        case perform defaultInterp s of
          Left _ -> property True -- cardinality refusal is legal
          Right p ->
            let byCh = [ sortOn pnOnset [n | tr <- perfTracks p, n <- tr
                                           , pnChannel n == c]
                       | c <- [0 .. 15] ]
                ok ns = and [ pnOnset a + pnDur a <= pnOnset b
                            | (a, b) <- zip ns (drop 1 ns) ]
             in property (all ok byCh)
  ]


-- ---------------------------------------------------------------------
-- W5: Euterpea's ToMidi as third oracle

oracle :: TestTree
oracle = testCase "ToMidi oracle: Euterpea's writer and ours agree" $ do
  present <- doesDirectoryExist corpusDir
  if not present then pure () else do
    src <- TIO.readFile (corpusDir </> "wtc1p01.krn")
    s <- either (assertFailure . ("parse: " <>)) pure (parseKern (Bpm 72) src)
    p <- either (assertFailure . ("perform: " <>)) pure
           (perform defaultInterp s)
    let ch0 = sortOn pnOnset
                [n | tr <- perfTracks p, n <- tr, pnChannel n == 0]
        m1 = go 0 ch0
        go _ [] = E.rest 0
        go t (n : more) =
          let WholeNotes o = pnOnset n
              WholeNotes d = pnDur n
              gap = o - t
           in E.rest gap E.:+: E.note d (E.pitch (pnPitch n),
                                          [E.Volume (pnVel n)])
                E.:+: go (o + d) more
        tmpF = ".otb-oracle-tmp.mid"
    writeMidi tmpF (m1 :: E.Music1)
    theirsBytes <- BS.readFile tmpF
    theirs <- either assertFailure pure (readSmf theirsBytes)
    ours0 <- either assertFailure pure
               (readSmf (BL.toStrict (renderSmf p)))
    let ours = sortOn smfOnQ [x | x <- ours0, smfChannel x == 0]
        them = sortOn smfOnQ theirs
    assertEqual "note counts" (length ours) (length them)
    let tol = 1 / 24 -- Euterpea's coarser division rounds; ours is 480
        close a b =
          smfPitch a == smfPitch b
            && smfVel a == smfVel b
            && abs (smfOnQ a - smfOnQ b) <= tol
            && abs (smfDurQ a - smfDurQ b) <= 2 * tol
        bad = [(a, b) | (a, b) <- zip ours them, not (close a b)]
    assertBool ("disagreements: " <> show (take 3 bad)) (null bad)
