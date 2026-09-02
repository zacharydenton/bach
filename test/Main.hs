-- | Unit tests plus the M1 corpus sweep.
--
-- The sweep needs corpus/bach-wtc (gitignored); when absent it *fails*
-- with the clone command, unless OTB_NO_CORPUS=1 asks for units only. The
-- tools' Python tests run from here too, so one `stack test` covers all.
--
-- License: GPL-2.0-or-later.
module Main (main) where

import Control.Monad (forM, forM_)
import Data.List (groupBy, isInfixOf, isSuffixOf, nub, sort, sortOn)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import OTB.Analysis.Counterpoint (parallelPerfects)
import OTB.Config (ArtParams (..), artParamsFor, defaultArtParams, loadConfig)
import OTB.Interp.Articulation (articulateLane)
import OTB.Kern.Lexer (lexNoteTok)
import OTB.Kern.Parser (parseKern)
import OTB.Kern.Token (Mark (..), NoteTok (..), Tie (..))
import OTB.Emit.Json (renderJson)
import OTB.Emit.Midi (renderSmf)
import OTB.Edition (kernTitle, readKernSource)
import OTB.Interp.Agogics
import OTB.Interp.Dynamics
import OTB.Interp.Express
import OTB.Interp.Ornament
import OTB.Interp.Phrasing
import OTB.Explain (Why (..), renderWhys)
import OTB.BakeSite qualified as Bake
import OTB.Bridge qualified as B
import OTB.Fit qualified as F
import OTB.Json qualified as J
import OTB.Maestro qualified as M
import OTB.MaestroAlign qualified as MA
import OTB.PieceFit qualified as PF
import OTB.Smf qualified as Smf
import Data.Vector qualified as BV
import Data.Word (Word8)
import OTB.Analysis.Harmony (Harmony (..), analyzeHarmony, melodicCharge)
import OTB.Analysis.Imitation (Imitation (..), findImitation)
import OTB.Analysis.Parallelism (Sequences (..), findSequences)
import OTB.Analysis.Subject (subjectEntries)
import OTB.Instrument (hardwareTracks)
import OTB.Annotate (annEvents)
import OTB.Pitch (Spelled (..), spMidi, spName)
import OTB.TempoGiusto (tempoGiusto)
import OTB.Player (Interp (..), PerfNote (..), Performance (..), defaultInterp, perform)
import OTB.Score (Score (..), ScoreNote (..), Voice (..), scoreNote)
import OTB.Tuning
import OTB.Units (Bpm (..), Cents (..), Seconds (..), WholeNotes (..), secondsAt, toTicks)
import System.Directory
  (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory)
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
  chorales <- choraleSweep
  offering <- offeringSweep
  defaultMain $ testGroup "all"
    [units, laws, sweep, chorales, offering, oracle, review, sota]

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
-- | The chorale corpus (craigsapp/bach-370-chorales): 370 files, all
-- parse, ZERO tie leftovers (a cleaner encoding than the WTC), all
-- perform within 15 lanes. Baselines pinned 2026-09-02 at repo HEAD;
-- max observed parallel perfects is 4 (Bach's documented ones) — the
-- ceiling of 10 hunts parser blowups, not musicology.
choraleSweep :: IO TestTree
choraleSweep = do
  let dir = "corpus/bach-chorales/kern"
  present <- doesDirectoryExist dir
  allowSkip <- lookupEnv "OTB_NO_CORPUS"
  if not present
    then pure $ testCase "chorale sweep" $
      if allowSkip == Just "1"
        then pure ()
        else assertFailure
          ("chorales not cloned — run\n  git clone --depth 1 "
             <> "https://github.com/craigsapp/bach-370-chorales "
             <> "corpus/bach-chorales\nor set OTB_NO_CORPUS=1")
    else do
      files <- sort . filter (".krn" `isSuffixOf`) <$> listDirectory dir
      results <- forM files $ \f -> do
        src <- TIO.readFile (dir </> f)
        pure (f, parseKern (Bpm 72) src)
      pure $ testGroup "chorale sweep"
        [ testCase "all 370 chorales parse" $ do
            let failures = [f <> ": " <> e | (f, Left e) <- results]
            assertEqual (unlines failures) 370
              (length [() | (_, Right _) <- results])
        , testCase "no tie leftovers (chorale encoding is clean)" $
            assertEqual "leftover notes" 0
              (sum [scTieLeftovers s | (_, Right s) <- results])
        , testCase "every chorale fits 15 lanes" $ do
            let overs = [ f | (f, Right s) <- results
                        , Left _ <- [perform defaultInterp s] ]
            assertEqual (unlines overs) [] overs
        , testCase "counterpoint oracle: chorales under threshold" $ do
            let bad = [ (f, n) | (f, Right s) <- results
                      , let n = parallelPerfects s, n > 10 ]
            assertEqual (show bad) [] bad
        , testCase "titles come out of the kern" $ do
            src <- TIO.readFile (dir </> "chor001.krn")
            let (title, sct) = kernTitle src
            title @?= Just "Aus meines Herzens Grunde"
            sct @?= Just "BWV 269"
        ]

-- | The Musical Offering (craigsapp/bach-musical-offering): 6 kern
-- files; offering-013b carries a real mid-piece *MM (the trio
-- sonata's closing adagio) which the parser deliberately refuses —
-- 5 of 6 parse until mid-piece tempo lands. Pinned 2026-09-02.
offeringSweep :: IO TestTree
offeringSweep = do
  let dir = "corpus/bach-musical-offering/kern"
  present <- doesDirectoryExist dir
  allowSkip <- lookupEnv "OTB_NO_CORPUS"
  if not present
    then pure $ testCase "offering sweep" $
      if allowSkip == Just "1"
        then pure ()
        else assertFailure
          ("musical offering not cloned — run\n  git clone --depth 1 "
             <> "https://github.com/craigsapp/bach-musical-offering "
             <> "corpus/bach-musical-offering\nor set OTB_NO_CORPUS=1")
    else do
      files <- sort . filter (".krn" `isSuffixOf`) <$> listDirectory dir
      results <- forM files $ \f -> do
        src <- TIO.readFile (dir </> f)
        pure (f, parseKern (Bpm 72) src)
      pure $ testGroup "offering sweep"
        [ testCase "5 of 6 parse (013b: mid-piece tempo, known out)" $ do
            let ok = [f | (f, Right _) <- results]
                failed = [f | (f, Left _) <- results]
            assertEqual (show failed) ["offering-013b.krn"] failed
            assertEqual "parsed" 5 (length ok)
        , testCase "every parsed movement fits its lanes" $ do
            let overs = [ f | (f, Right s) <- results
                        , Left _ <- [perform defaultInterp s] ]
            assertEqual (unlines overs) [] overs
        ]

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
            Right (Score (Bpm t) vs 0 _ _ _ _ _) -> do
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
            Right (Score _ [Voice _ [n]] 0 _ _ _ _ _) ->
              snDur n @?= (1 / 2)
            Right s -> assertFailure ("unexpected shape: " <> show s)
      , testCase "fermata on a tied close holds only the close" $ do
          let src = T.unlines ["**kern", "[2c", "4c;]", "*-"]
          case parseKern (Bpm 72) src of
            Left e -> assertFailure e
            Right (Score _ [Voice _ [n]] 0 _ _ _ _ _) -> do
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
            Right (Score _ [Voice _ ns] 0 _ _ _ _ _) ->
              sort [(snLane n, snPitch n) | n <- ns]
                @?= [(0, 60), (0, 60), (0, 60), (1, 64), (1, 67)]
            Right s -> assertFailure ("unexpected shape: " <> show s)
      , testCase "ornament on a tie close restrikes, not unions" $ do
          -- a trill on the closing token starts a fresh attack there: the
          -- held half lands plain, the trilled quarter is its own note
          let src = T.unlines ["**kern", "[2c", "4cT]", "*-"]
          case parseKern (Bpm 72) src of
            Left e -> assertFailure e
            Right (Score _ [Voice _ ns] 0 _ _ _ _ _) ->
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
          let ag = defaultAgogicParams
                     {agRitSpan = 1, agRitFloor = 0.5, agOpenPush = 0}
              tm = tempoMap ag [] [] [] [] [] (Bpm 100) 4
              bpms = [b | (_, Bpm b) <- tm]
          take 1 bpms @?= [100]
          assertBool ("not descending: " <> show bpms)
            (and (zipWith (>=) bpms (drop 1 bpms)))
          assertBool ("floor overshot: " <> show (last bpms))
            (last bpms >= 50 && last bpms < 100)
      , testCase "Todd arches: tempo rises to centre, sane bounds" $ do
          let ag = defaultAgogicParams {agRitSpan = 0, agOpenPush = 0}
              tm = tempoMap ag [(0, 8, 0.05)] [] [] [] [] (Bpm 100) 8
              bpms = [b | (_, Bpm b) <- tm]
              mid = bpms !! (length bpms `div` 2)
          assertBool ("no arch: " <> show (take 5 bpms)) (mid > 100)
          assertBool "bounded" (all (\b -> b > 90 && b < 110) bpms)
      , testCase "group arches follow a meter change" $ do
          -- 4/4 for one bar then 3/8: the caller lays arches per group,
          -- re-aligned at the change; peaks at 1/2 and 1 + 3/16
          let ag = defaultAgogicParams
                     {agRitSpan = 0, agTempoStep = 1 / 16, agOpenPush = 0}
              tm = tempoMap ag [(0, 1, 0.05), (1, 1 + 3 / 8, 0.05)] [] [] [] []
                     (Bpm 100) (1 + 3 / 8)
              at t = case takeWhile ((<= t) . fst) tm of
                [] -> 100; xs -> let Bpm b = snd (last xs) in b
          assertBool "peak of the 4/4 bar" (at (1 / 2) > at (1 / 16))
          assertBool "a fresh trough at the change" (at 1 < at (1 / 2))
          assertBool "peak of the 3/8 group" (at (1 + 3 / 16) > at 1)
      , testCase "opening push decays to base over its span" $ do
          let ag = defaultAgogicParams
                     {agRitSpan = 0, agOpenPush = 0.05, agOpenSpan = 2}
              tm = tempoMap ag [] [] [] [] [] (Bpm 100) 8
              at x = case takeWhile ((<= x) . fst) tm of
                [] -> 100; xs -> let Bpm b = snd (last xs) in b
          assertBool "opens above base" (at 0 > 103)
          assertBool "settled at base after the span"
            (abs (at 3 - 100) < 0.5)
      , testCase "boundary easing slows into the arrival, recovers after" $ do
          let ag = defaultAgogicParams {agRitSpan = 0, agOpenPush = 0}
              tm = tempoMap ag [] [(4, 0.08, 3 / 4)] [] [] [] (Bpm 100) 8
              at x = case takeWhile ((<= x) . fst) tm of
                [] -> 100; xs -> let Bpm b = snd (last xs) in b
          assertBool "eases before" (at 3.9 < 96)
          assertBool "a tempo after" (abs (at 4.5 - 100) < 0.5)
      , testCase "subject spans push forward" $ do
          let ag = defaultAgogicParams
                     {agRitSpan = 0, agOpenPush = 0, agSubjectPush = 0.02}
              tm = tempoMap ag [] [] [(1, 2)] [] [] (Bpm 100) 8
              at x = case takeWhile ((<= x) . fst) tm of
                [] -> 100; xs -> let Bpm b = snd (last xs) in b
          assertBool "pushes inside" (at 1.5 > 101)
          assertBool "base outside" (abs (at 3 - 100) < 0.5)
      , testCase "hostile arch depths never take tempo non-positive" $ do
          let ag = defaultAgogicParams {agRitSpan = 0}
              tm = tempoMap ag [(0, 8, 5.0), (0, 4, 2.0)] [(2, 5.0, 3 / 4), (4, 0.2, 3 / 4)] [] [] [] (Bpm 100) 8
          assertBool "all positive" (all (\(_, Bpm b) -> b > 0) tm)
      , testCase "short piece: no rit, single tempo" $
          length (tempoMap (defaultAgogicParams {agOpenPush = 0})
                    [] [] [] [] [] (Bpm 100) (1 / 2)) @?= 1
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
                      [scoreNote 0 (1 / 4) 60 [Turn 2 2]]
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
                          scoreNote (1/4) (1/4) 62 []]] 0 0 [(0, (4, 4))] 0 [] True
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
                0 0 [(0, (4, 4))] 0 [] True
          case perform interp score of
            Left e -> assertFailure e
            Right pf | [[a, b]] <- perfTracks pf -> do
              pnDur a @?= d / 4
              assertBool "channel overlap" (pnOnset a + pnDur a < pnOnset b)
            Right pf -> assertFailure ("unexpected tracks: " <> show (perfTracks pf))
      , testCase "a fermata on one chord member holds the whole sonority" $ do
          -- engraving puts one sign above the system, so kern often marks
          -- only the outer voices (wtc1p08's final chord: ; on the two
          -- E-flats, nothing on the four inner notes) — the hold belongs
          -- to the sonority, not to whoever got the glyph
          let src marks = T.unlines
                [ "**kern\t**kern"
                , "*MM96\t*"
                , "2C" <> marks <> "\t2e"
                , "*-\t*-"
                ]
              durs s = case parseKern (Bpm 72) s >>= perform defaultInterp of
                Left e -> error e
                Right pf -> [pnDur n | tr <- perfTracks pf, n <- tr]
          case (durs (src ";"), durs (src "")) of
            ([dm, dp], [um, up]) -> do
              -- each keeps its own articulation gate; the HOLD (1.75x)
              -- must reach both
              assertBool ("marked not held: " <> show (dm, um))
                (dm > 1.5 * um)
              assertBool ("partner not held: " <> show (dp, up))
                (dp > 1.5 * up)
            shape -> assertFailure ("unexpected notes: " <> show shape)
      , testCase "melody-lead belongs to the top lane, not the whole voice" $ do
          -- one voice, split spine (wtc1p08's texture): only the highest
          -- LANE anticipates — the old voice-level melody shifted every
          -- lane identically and produced no asynchrony at all
          let src = T.unlines
                [ "**kern"
                , "*MM96"
                , "*^"
                , "4c\t4e"
                , "4c\t4e"
                , "*v\t*v"
                , "*-"
                ]
              interp = defaultInterp
                { iExpress = defaultExpressParams
                    -- no jitter, and no final-chord roll: the second
                    -- beat IS the final chord here, and the bass-up
                    -- roll would reorder exactly the pair under test
                    { exJitterMs = 0, exJitterVel = 0, exRollMs = 0 } }
          case parseKern (Bpm 72) src >>= perform interp of
            Left e -> assertFailure e
            Right pf -> do
              let ns = [n | tr <- perfTracks pf, n <- tr]
                  -- the second beat, clear of any clamp at zero
                  later p = minimum
                    [pnOnset n | n <- ns, pnPitch n == p, pnOnset n > 1 / 8]
              assertBool "top lane does not lead its chord partner"
                (later 64 < later 60)
      ]
  , testGroup "editions"
      [ testCase "edition substitutes only its named corpus source" $ do
          let corpusF = "corpus/bach-wtc/kern/wtc1p08.krn"
          have <- doesFileExist corpusF
          if not have then pure () else do
            raw <- TIO.readFile corpusF
            ed <- readKernSource "config/editions" corpusF
            assertBool "edition not applied to its corpus source" (ed /= raw)
            assertBool "m36 appoggiatura absent from the edition"
              ("4.g-p" `T.isInfixOf` ed)
            -- a stranger's file that merely shares the basename (no
            -- matching !!!KEY) must pass through untouched
            let dir = "/tmp/otb-edition-test"
                stranger = dir <> "/wtc1p08.krn"
            createDirectoryIfMissing True dir
            TIO.writeFile stranger
              (T.unlines ["**kern", "4c", "*-", "!!!KEY: 12345"])
            kept <- readKernSource "config/editions" stranger
            assertBool "stranger file was replaced by the edition"
              ("!!!KEY: 12345" `T.isInfixOf` kept)
      ]
  , testGroup "bake"
      [ testCase "book order: prelude before fugue, extras last" $ do
          let paths = [ "wtc2f01.json", "wtc1f01.json", "wtc1p02.json"
                      , "wtc1p01.json", "xtra-ground-folia.json"
                      , "wtc2p01.json" ]
          sortOn Bake.albumKey paths @?=
            [ "wtc1p01.json", "wtc1f01.json", "wtc1p02.json"
            , "wtc2p01.json", "wtc2f01.json"
            , "xtra-ground-folia.json" ]
      , testCase "patch url is the bank tail" $
          Bake.patchUrl
            "/usr/share/surge-xt/patches_factory/Leads/DNA.fxp"
            @?= "data/patches/Leads/DNA.fxp"
      , testCase "windows separators normalised" $
          Bake.patchUrl "C:\\lib\\Pads\\Warm.fxp"
            @?= "data/patches/Pads/Warm.fxp"
      , testCase "piece_instances mirrors JS half-up rounding" $
          -- seven ranked channels: ranks 1 and 5 sit exactly on .5 —
          -- JS Math.round goes UP; per-slot [1,2,2,2] -> 4
          Bake.pieceInstances [(c, 40 + c) | c <- [0 .. 6]] @?= 4
      , testCase "casting + calibration rewritten to bank urls" $ do
          let tmp = "/tmp/otb-bake-test"
              castdir = tmp </> "casting"
              dat = tmp </> "data"
          forM_ [castdir, dat] (createDirectoryIfMissing True)
          writeFile (castdir </> "default.json")
            ("{\"_\": \"a comment, not a channel\", "
               <> "\"0\": \"/mac/Leads/Deep.fxp\", "
               <> "\"soprano\": \"/mac/Leads/Lera.fxp\"}")
          writeFile (castdir </> "wtc1f01.json")
            "{\"0\": \"/other/Pads/Warm.fxp\"}"
          writeFile (tmp </> "calibration.json")
            "{\"/mac/Leads/Deep.fxp\": {\"releaseS\": 0.4}}"
          Bake.bakeCasting dat castdir (tmp </> "calibration.json")
          cast <- either error id . J.parseJson
                    <$> TIO.readFile (dat </> "casting.json")
          (J.jLookup "0" =<< J.jLookup "default" cast) @?=
            Just (J.JStr "data/patches/Leads/Deep.fxp")
          (J.jLookup "soprano" =<< J.jLookup "default" cast) @?=
            Just (J.JStr "data/patches/Leads/Lera.fxp")
          (J.jLookup "0" =<< J.jLookup "wtc1f01" cast) @?=
            Just (J.JStr "data/patches/Pads/Warm.fxp")
          cal <- either error id . J.parseJson
                   <$> TIO.readFile (dat </> "calibration.json")
          (J.jNum =<< J.jLookup "releaseS"
             =<< J.jLookup "data/patches/Leads/Deep.fxp" cal) @?=
            Just 0.4
      , testCase "manifest orders, measures, counts instances" $ do
          let tmp = "/tmp/otb-manifest-test"
              perf = tmp </> "perf"
          createDirectoryIfMissing True perf
          writeFile (perf </> "wtc1p01.json")
            ("{\"piece\": \"wtc1p01\", \"endS\": 9.0, \"tracks\": [["
               <> "{\"onS\": 0.0, \"durS\": 1.0, \"ch\": 0, \"pitch\": 40},"
               <> "{\"onS\": 1.0, \"durS\": 1.0, \"ch\": 2, \"pitch\": 70}"
               <> "]]}")
          writeFile (perf </> "wtc1f01.json")
            ("{\"piece\": \"wtc1f01\", \"tracks\": [["
               <> "{\"onS\": 0.0, \"durS\": 4.0, \"ch\": 0, \"pitch\": 60}"
               <> "]]}")
          Bake.bakeManifest tmp
          man <- either error id . J.parseJson
                   <$> TIO.readFile (tmp </> "manifest.json")
          let pieces = J.jArrOf (maybe (J.JArr []) id
                (J.jLookup "pieces" man))
          map (J.jLookup "name") pieces @?=
            [Just (J.JStr "wtc1p01"), Just (J.JStr "wtc1f01")]
          (J.jNum =<< J.jLookup "endS" (head pieces)) @?= Just 9.0
          (J.jNum =<< J.jLookup "maxCh" (head pieces)) @?= Just 3
          (J.jNum =<< J.jLookup "endS" (pieces !! 1)) @?= Just 4.0
          -- chans 0,2 -> bass+soprano -> 2 instances; one chan -> 1
          (J.jNum =<< J.jLookup "nInstances" man) @?= Just 2
          (J.jLookup "scl" man) @?= Just (J.JStr "data/w3.scl")
      , testCase "baked site honours client contracts (if baked)" $ do
          let dat = "site" </> "data"
          baked <- doesFileExist (dat </> "manifest.json")
          if not baked then pure () else do
            man <- either error id . J.parseJson
                     <$> TIO.readFile (dat </> "manifest.json")
            let pieces = J.jArrOf (maybe (J.JArr []) id
                  (J.jLookup "pieces" man))
            assertBool "no pieces" (not (null pieces))
            forM_ (take 3 pieces <> drop (length pieces - 3) pieces)
              $ \p -> forM_ (J.jStr =<< J.jLookup "url" p) $ \u -> do
                ok <- doesFileExist ("site" </> T.unpack u)
                assertBool ("missing " <> T.unpack u) ok
            ok <- doesFileExist (dat </> "init.fxp")
            assertBool "init.fxp missing" ok
            cast <- either error id . J.parseJson
                      <$> TIO.readFile (dat </> "casting.json")
            case cast of
              J.JObj kvs -> forM_ kvs $ \(_, entry) ->
                case entry of
                  J.JObj es -> forM_ es $ \(_, v) ->
                    forM_ (J.jStr v) $ \u -> do
                      exists <- doesFileExist ("site" </> T.unpack u)
                      assertBool ("casting url gone: " <> T.unpack u)
                        exists
                  _ -> pure ()
              _ -> assertFailure "casting.json not an object"
      ]
  , testGroup "piece fit"
      [ testCase "shrink pulls toward the global (n=5, k=2)" $
          PF.shrinkKnobs [("cadence_depth", 0.07)] (const 0) 5 2
            @?= [("cadence_depth", 0.05)]
      , testCase "n=1 is strongly shrunk toward the base" $
          PF.shrinkKnobs [("open_push", 0.12)] (const 0.06) 1 2
            @?= [("open_push", 0.08)]
      , testCase "sections carry only kept fits, all marked" $ do
          let secs = PF.buildSections "2026-01-01" [fitRec]
          case lookup "wtc1p03" secs of
            Nothing -> assertFailure "no section"
            Just lns -> do
              let keys = [k | (k, _, _) <- lns]
              assertBool "tempo missing" ("tempo" `elem` keys)
              assertBool "kept timing missing"
                ("cadence_depth" `elem` keys)
              assertBool "rejected velocity leaked"
                ("vel_highloud" `notElem` keys)
              forM_ lns $ \(_, _, c) ->
                assertBool "unmarked" ("# PIECE-FIT" `isInfixOf` c)
      , testCase "hand keys survive and win" $ do
          let toml = T.unlines
                [ "[agogics]", "rit_span = 2.0", ""
                , "[piece.wtc1p03]"
                , "cadence_depth = 0.09 # by ear, veto" ]
              out = T.unpack (PF.applyFits "2026-01-01" [fitRec] toml)
          assertBool "hand veto gone"
            ("cadence_depth = 0.09 # by ear, veto" `isInfixOf` out)
          countOf "cadence_depth" out @?= 1
          assertBool "tempo not added"
            ("tempo = 84.0 # PIECE-FIT" `isInfixOf` out)
      , testCase "new piece lands under the banner" $ do
          let out = T.unpack (PF.applyFits "2026-01-01" [fitRec]
                "[agogics]\nrit_span = 2.0\n")
          assertBool "no section" ("[piece.wtc1p03]" `isInfixOf` out)
          assertBool "no banner" ("fitted per piece" `isInfixOf` out)
          assertBool "no shrunk value"
            ("cadence_depth = 0.04 # PIECE-FIT" `isInfixOf` out)
      , testCase "idempotent regeneration (byte-level, mixed)" $ do
          -- one piece with an existing section, one new piece: the
          -- second application must not relocate anything around the
          -- banner (once /= twice was a real writer bug)
          let newRec = PF.PieceRec "wtc1p10" 2
                (Just (60.0, 70.0, 63.3)) Nothing Nothing
              recs = [fitRec, newRec]
              toml = T.unlines
                [ "[agogics]", "rit_span = 2.0", ""
                , "[piece.wtc1p03]"
                , "cadence_depth = 0.09 # by ear, veto" ]
              once = PF.applyFits "2026-01-01" recs toml
              twice = PF.applyFits "2026-01-01" recs once
              thrice = PF.applyFits "2026-01-01" recs twice
          twice @?= once
          thrice @?= once
          countOf "tempo = 84.0" (T.unpack twice) @?= 1
          countOf "tempo = 63.3" (T.unpack twice) @?= 1
          countOf "[piece.wtc1p03]" (T.unpack twice) @?= 1
          countOf "[piece.wtc1p10]" (T.unpack twice) @?= 1
      , testCase "edits are positional, not lexical" $ do
          let recFor p f = PF.PieceRec p 3
                (Just (fromIntegral (10 * f), 100.0
                      , fromIntegral (10 * f) + 1))
                Nothing Nothing
              recs = [ recFor "wtc1f04" 4, recFor "wtc1f03" 3
                     , recFor "wtc1f05" 5 ]
              toml = T.unlines
                [ "[agogics]", "rit_span = 2.0", ""
                , "[piece.wtc1f05]", "overhold = 0.5 # hand", ""
                , "[piece.wtc1f03]", "inegal = 0.1 # hand", ""
                , "[piece.wtc1f04]", "base = 0.8 # hand" ]
              out = PF.applyFits "2026-01-01" recs toml
          forM_ [ ("wtc1f03", "31.0"), ("wtc1f04", "41.0")
                , ("wtc1f05", "51.0") ] $ \(p, want) -> do
            let body = sectionBody p (T.unpack out)
            assertBool (p <> " got: " <> body)
              (("tempo = " <> want) `isInfixOf` body)
      , testCase "prefit strips PIECE-FIT, keeps hand + global priors" $ do
          cfg <- TIO.readFile "config/default.toml"
          let stripped = T.unpack (F.prefitStrip cfg)
          assertBool "PIECE-FIT survived strip"
            (not ("# PIECE-FIT" `isInfixOf` stripped))
          assertBool "banner survived strip"
            (not ("fitted per piece" `isInfixOf` stripped))
          assertBool "global prior comment stripped"
            ("FITTED (was" `isInfixOf` stripped)
          assertBool "global rit_span gone"
            ("rit_span = 2.0" `isInfixOf` stripped)
          assertBool "global vel_highloud gone"
            ("vel_highloud = 0.8" `isInfixOf` stripped)
          assertBool "hand section gone"
            ("[piece.wtc1p01]" `isInfixOf` stripped)
          assertBool "hand overhold gone"
            ("overhold" `isInfixOf` stripped)
      ]
  , testGroup "maestro"
      [ testCase "smf: tempo map + running status" $ do
          case Smf.parseSmf synthSmf of
            Left e -> assertFailure e
            Right notes -> do
              length notes @?= 2
              let [c4, e4] = notes
              (Smf.snPitch c4, Smf.snVel c4) @?= (0x3C, 0x50)
              assertClose "c4 on" (Smf.snOnS c4) 0.0
              assertClose "c4 off" (Smf.snOffS c4) 0.5
              -- 2 qn at 120 + 1 qn at 240 = 1.0 + 0.25
              assertClose "e4 on" (Smf.snOnS e4) 1.25
              assertClose "e4 off" (Smf.snOffS e4) 1.5
      , testCase "piece names across the books" $ do
          M.pieceNames 846 @?= ("wtc1p01", "wtc1f01")
          M.pieceNames 853 @?= ("wtc1p08", "wtc1f08")
          M.pieceNames 870 @?= ("wtc2p01", "wtc2f01")
          M.pieceNames 893 @?= ("wtc2p24", "wtc2f24")
      , testCase "subsequence DTW recovers a warped performance" $ do
          let (score, notes) = synthPerf (\i -> 0.4 + 0.15 * (i / 64))
              notesV = BV.fromList notes
              pc = MA.perfChords notesV
              path = MA.dtwPath score pc notesV
              (pairs, _dels, _ins) = MA.pairNotes score pc notesV path
              total = sum (map (length . snd) score)
          assertBool ("rate " <> show (length pairs) <> "/"
                        <> show total)
            (fromIntegral (length pairs) / fromIntegral total
               >= (0.97 :: Double))
          let (grid, Just times) =
                MA.beatGridTimes pairs notesV (fst (last score))
              Just mid = lookup (32 * 480) (zip grid [0 :: Int ..])
              expect = 3.0 + sum [ 0.4 + 0.15 * (i / 64)
                                 | i <- [0 .. 31] ]
          assertBool ("beat grid off: " <> show (times !! mid))
            (abs (times !! mid - expect) < 0.05)
      , testCase "emitted .match round-trips through parseMatch" $ do
          let (score, notes) = synthPerf (const 0.5)
              notesV = BV.fromList notes
              pc = MA.perfChords notesV
              path = MA.dtwPath score pc notesV
              (pairs, dels, ins) = MA.pairNotes score pc notesV path
              ao = MA.AlignOutcome False pairs dels ins [] [] score
                     (MA.AlignStats 0 0 0 0 0 Nothing Nothing)
              mp = B.parseMatch (MA.matchText "t" "m" ao notesV)
          length (B.mpRows mp) @?= length pairs
          sort [ (B.mrKey r, B.mrPitch r) | r <- B.mpRows mp ] @?=
            sort [ (t, p) | (t, p, _c, _x) <- pairs ]
          B.mpDeletions mp @?= length dels
          B.mpInsertions mp @?= length ins
      , testCase "rolled chords still match after recovery" $ do
          let score =
                [ ( i * 960
                  , if even i
                      then [(40 + i + j * 3, 0) | j <- [0 .. 5]]
                      else [(80 + (i * 7) `mod` 12, 0)] )
                | i <- [0 .. 23] ]
              notes =
                [ Smf.SmfNote p 70 (t + 0.06 * fromIntegral k) (t + 0.8)
                    0 0
                | ((_, ps), t) <- zip score [2.0, 3.1 ..]
                , (k, (p, _c)) <- zip [0 :: Int ..] ps ]
              notesV = BV.fromList notes
              pc = MA.perfChords notesV
              path = MA.dtwPath score pc notesV
              (pairs0, dels0, ins0) =
                MA.pairNotes score pc notesV path
              (grid, Just times) =
                MA.beatGridTimes pairs0 notesV (fst (last score))
              (pairs, _d, _i) =
                MA.recover notesV pairs0 dels0 ins0 grid times
              total = sum (map (length . snd) score)
          assertBool ("rate " <> show (length pairs) <> "/"
                        <> show total)
            (fromIntegral (length pairs) / fromIntegral total
               >= (0.95 :: Double))
      , testCase "smf reader reproduces .match ground truth (if corpus)"
          $ do
          let mid = "corpus/asap/Bach/Fugue/bwv_846/Shi05M.mid"
              mtch = "corpus/asap/Bach/Fugue/bwv_846/Shi05M.match"
          have <- doesFileExist mid
          if not have then pure () else do
            notes <- Smf.readSmf mid
            mp <- B.parseMatch <$> TIO.readFile mtch
            let byPitch = foldr
                  (\n m -> insertWithList (Smf.snPitch n) n m) []
                  notes
                lookupP p = maybe [] id (lookup p byPitch)
                oks =
                  [ ( abs (Smf.snOnS best - B.mrOnS r) < 0.002
                    , Smf.snVel best == B.mrVel r )
                  | r <- B.mpRows mp
                  , let cands = lookupP (B.mrPitch r)
                  , not (null cands)
                  , let best = minimumOn
                          (\n -> abs (Smf.snOnS n - B.mrOnS r)) cands ]
            length [() | (True, _) <- oks] @?= length (B.mpRows mp)
            length [() | (_, True) <- oks] @?= length (B.mpRows mp)
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
          0 0 [(0, (4, 4))] 0 [] True)

transposeScore :: Int -> Score -> Score
transposeScore n s =
  s { scVoices = [ v { vNotes = [ sn { snPitch = snPitch sn + n
                                     , snSpell =
                                         if n == 0 then snSpell sn
                                         else Nothing
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


-- ---------------------------------------------------------------------
-- Review regressions (2026-08-29 findings)

review :: TestTree
review = testGroup "review regressions"
  [ testCase "conductor: no base-tempo reset after the closing rit" $
      withCorpus "wtc1p01.krn" $ \p _ -> do
        let tm = perfTempoMap p
            Bpm lastB = snd (last tm)
            Bpm firstB = snd (head tm)
        assertBool "tempo map non-empty" (not (null tm))
        -- the final rit must stand: last point well below base, and no
        -- trailing entry restoring the opening tempo
        assertBool ("closing rit cancelled: ends at " <> show lastB
                      <> " vs opening " <> show firstB)
          (lastB < firstB * 0.9)
  , testCase "hardware: every note survives tick rounding (no stuck notes)" $
      withCorpus "wtc1f01.krn" $ \p0 _ -> do
        (p, clipped) <- either assertFailure pure (hardwareTracks p0)
        let bad = [ n | tr <- perfTracks p, n <- tr
                  , toTicks (pnOnset n) >= toTicks (pnOnset n + pnDur n) ]
        assertBool "mono-reduction clipped something (sanity)" (clipped > 0)
        assertEqual "zero-tick notes (would stick on hardware)" 0 (length bad)
  , testCase "hardware: provenance rekeyed to surviving identities" $
      withCorpus "wtc1f01.krn" $ \p0 _ -> do
        (p, _) <- either assertFailure pure (hardwareTracks p0)
        let keys = [ (pnChannel n, pnIndex n) | tr <- perfTracks p, n <- tr ]
            stale = [ k | (k, _) <- perfWhys p, k `notElem` keys ]
        assertBool "whys survive the remap" (not (null (perfWhys p)))
        assertEqual "stale/colliding provenance keys" 0 (length stale)
        assertEqual "no duplicate note identities"
          (length keys) (length (nubOrd keys))

  -- 2026-08-31 findings
  , testCase "emitter: sub-tick note is floored to one tick, not stuck" $ do
      -- enforceMono can clamp a duration below one tick; at the same tick
      -- the off<on sort order would emit the release BEFORE its own
      -- attack — a stuck note on the default (non-hardware) path
      let tiny = PerfNote 0 (1 / 16384) 60 96 8192 0 0 0 60 0
          p = Performance [(0, Bpm 120)] [[tiny]] [] [] (1 / 4)
      notes <- either assertFailure pure (readSmf (BL.toStrict (renderSmf p)))
      [(smfPitch n, smfDurQ n > 0) | n <- notes] @?= [(60, True)]
  , testCase "parser: cross-spine tie continuation keeps the sound" $ do
      -- wtc1p04:240 continues a tie opened in the neighbouring spine; the
      -- (voice, pitch) key cannot match, but the note must not vanish
      let src = T.unlines
            [ "**kern\t**kern"
            , "4c\t[4c"
            , "2c_\t4r"
            , "*-\t*-"
            ]
      case parseKern (Bpm 72) src of
        Left e -> assertFailure e
        Right s -> do
          scTieLeftovers s @?= 1 -- the abandoned open still flushes
          sort [ (snOnset n, snDur n, snPitch n)
               | v <- scVoices s, n <- vNotes v ]
            @?= [(0, 1 / 4, 60), (0, 1 / 4, 60), (1 / 4, 1 / 2, 60)]
  , testCase "parser: *x (spine exchange) is a hard error, not silence" $ do
      let src = T.unlines ["**kern\t**kern", "*x\t*x", "4c\t4e", "*-\t*-"]
      case parseKern (Bpm 72) src of
        Left e -> assertBool ("error names *x: " <> e) ("*x" `isInfixOf` e)
        Right _ -> assertFailure "*x absorbed silently"
  , testCase "parser: fermata on a rest is recorded as a span" $ do
      let src = T.unlines ["**kern", "4c", "4r;", "4c", "*-"]
      case parseKern (Bpm 72) src of
        Left e -> assertFailure e
        Right s -> scRestHolds s @?= [(1 / 4, 1 / 4)]
  , testCase "rest fermata: the silence breathes through the tempo map" $ do
      -- every other agogic layer zeroed: the curve must be base tempo
      -- everywhere except the rest's span, where it divides by the hold
      let src = T.unlines ["**kern", "4c", "4r;", "4c", "*-"]
      case parseKern (Bpm 120) src >>= perform calmInterp of
        Left e -> assertFailure e
        Right p -> do
          let tm = perfTempoMap p
              Bpm held = tempoAt tm (3 / 8) -- mid-rest
              Bpm before = tempoAt tm (1 / 8)
              Bpm after = tempoAt tm (5 / 8)
              hold = fromRational (agFermataHold defaultAgogicParams)
          assertBool ("held " <> show held) (abs (held - 120 / hold) < 0.01)
          assertBool ("before " <> show before) (abs (before - 120) < 0.01)
          assertBool ("after " <> show after) (abs (after - 120) < 0.01)
  , testCase "rest fermata: one hold, however many spines notate it" $ do
      -- wtc2p07's final bar carries ; in BOTH resting spines: covered-or-
      -- not semantics, never a product of spans
      let src = T.unlines
            [ "**kern\t**kern", "4c\t4e", "4r;\t4r;", "4c\t4e", "*-\t*-" ]
      case parseKern (Bpm 120) src >>= perform (calmInterp) of
        Left e -> assertFailure e
        Right p -> do
          let Bpm held = tempoAt (perfTempoMap p) (3 / 8)
              hold = fromRational (agFermataHold defaultAgogicParams)
          assertBool ("held once, not squared: " <> show held)
            (abs (held - 120 / hold) < 0.01)
  , testCase "rest fermata: a concurrent note fermata is the same event" $ do
      -- wtc1p21: 8bb-; against 8r; — ONE pause. The global hold owns it
      -- (only the tempo map pushes every voice's successors); the
      -- covered note must not stretch its duration on top
      let src = T.unlines
            [ "**kern\t**kern", "4c\t4e", "4c;\t4r;", "4c\t4e", "*-\t*-" ]
          plain = T.unlines
            [ "**kern\t**kern", "4c\t4e", "4c\t4r", "4c\t4e", "*-\t*-" ]
      case (,) <$> (parseKern (Bpm 120) src >>= perform calmInterp)
               <*> (parseKern (Bpm 120) plain >>= perform calmInterp) of
        Left e -> assertFailure e
        Right (p, q) -> do
          let Bpm held = tempoAt (perfTempoMap p) (3 / 8)
              hold = fromRational (agFermataHold defaultAgogicParams)
          assertBool ("global hold applies: " <> show held)
            (abs (held - 120 / hold) < 0.01)
          -- notated durations identical to the fermata-free variant:
          -- the pause lives in the tempo map, not in the note
          map pnDur (concat (perfTracks p))
            @?= map pnDur (concat (perfTracks q))
  , testCase "rest fermata: a trailing held silence stays in the piece" $ do
      -- wtc2p07 ends on an all-rest held bar AFTER the last sounding
      -- note: the curve's domain must extend through it
      let src = T.unlines ["**kern", "4c", "4r;", "*-"]
      case parseKern (Bpm 120) src >>= perform (calmInterp) of
        Left e -> assertFailure e
        Right p -> do
          let tm = perfTempoMap p
              Bpm held = tempoAt tm (3 / 8)
              hold = fromRational (agFermataHold defaultAgogicParams)
          assertBool ("held in the silence: " <> show held)
            (abs (held - 120 / hold) < 0.01)
          -- the piece's extent includes the held silence, and the SMF
          -- End-of-Track lands there (the file does not stop at the
          -- last note-off)
          perfEnd p @?= 1 / 2
  , testCase "lexer: longa and rational reciprocals" $ do
      ntDur (lexNoteTok "0c") @?= 2 -- breve
      ntDur (lexNoteTok "00c") @?= 4 -- longa, not a second breve
      ntDur (lexNoteTok "2%3c") @?= 3 / 2 -- triplet breve, not recip 23
      ntDots (lexNoteTok "8.e") @?= 1 -- notated dots retained
      ntDots (lexNoteTok "2%3c") @?= 0 -- same 3/2 shape, not dotted
  , testCase "lexer: O and z are retained marks, not dropped" $
      ntMarks (lexNoteTok "4cOz") @?= [GenericOrn, Sforzando]
  , testCase "trill: acceleration is a spin-up window, not a runaway" $ do
      -- per-subnote compounding made first/last = accel^(n-1): a tied
      -- whole-note trill (wtc1p16) opened at a third of nominal rate and
      -- closed in a sub-tick buzz. The window caps the ratio at accel^8.
      let sn = scoreNote 0 4 60 [Trill 2]
          out = realizeNote defaultOrnamentParams (Bpm 96) sn
          ds = map snDur out
      sum ds @?= 4
      assertBool "still accelerates" (head ds > last ds)
      assertBool ("first/last ratio: " <> show (maximum ds / minimum ds))
        (maximum ds / minimum ds < 2.5)
  , testCase "IR carries score-level identity (srcWn/srcPitch)" $ do
      -- the ASAP note bridge joins on the notated onset and pitch;
      -- both must survive into the JSON seam
      let n = PerfNote (1 / 8) (1 / 4) 74 96 8192 0 0 (1 / 8) 72 0
          p = Performance [(0, Bpm 120)] [[n]] [] [] (1 / 2)
          js = renderJson "t" p
      assertBool "srcWn present" ("\"srcWn\":0.125" `isInfixOf` js)
      assertBool "srcPitch present" ("\"srcPitch\":72" `isInfixOf` js)
  , testCase "grace: lexed as a zero-duration Grace-marked note" $ do
      let t' = lexNoteTok "cc#q/"
      ntDur t' @?= 0
      assertBool "carries Grace" (Grace `elem` ntMarks t')
  , testCase "grace: parser retains it; nothing dropped" $ do
      let src = T.unlines ["**kern", "ccq", "4c", "*-"]
      case parseKern (Bpm 72) src of
        Left e -> assertFailure e
        Right s -> do
          scGraceDropped s @?= 0
          [ (snDur n, snPitch n, Grace `elem` snMarks n)
            | v <- scVoices s, n <- vNotes v ]
            @?= [(0, 72, True), (1 / 4, 60, False)]
  , testCase "grace: realised on the beat, stealing from the main note" $ do
      -- C.P.E. Bach: the Vorschlag falls ON the beat; 70 ms at 96 bpm
      -- is 7/250 wn, well under half the quarter
      let g = scoreNote (1 / 4) 0 74 [Grace]
          m = scoreNote (1 / 4) (1 / 4) 72 []
          out = realizeGraceLane defaultOrnamentParams (Bpm 96) [g, m]
      [(snOnset n, snDur n, snPitch n) | n <- out]
        @?= [ (1 / 4, 7 / 250, 74)
            , (1 / 4 + 7 / 250, 1 / 4 - 7 / 250, 72) ]
      sum (map snDur out) @?= 1 / 4 -- stolen, not added
      assertBool "Grace mark consumed"
        (all (notElem Grace . snMarks) out)
  , testCase "grace: never takes more than half a short main note" $ do
      let g = scoreNote 0 0 74 [Grace]
          m = scoreNote 0 (1 / 64) 72 []
          out = realizeGraceLane defaultOrnamentParams (Bpm 96) [g, m]
      map snDur out @?= [1 / 128, 1 / 128]
  , testCase "grace: long appoggiatura takes half, 2/3 of a dotted note" $ do
      let long = defaultOrnamentParams {opGraceLong = True}
          g t = scoreNote t 0 74 [Grace]
          run m = [(snOnset n, snDur n) | n <- realizeGraceLane long (Bpm 96) m]
      -- plain main: half
      run [g 0, scoreNote 0 (1 / 4) 72 []]
        @?= [(0, 1 / 8), (1 / 8, 1 / 8)]
      -- dotted main: two thirds — dottedness read from snDots (notation)
      run [g 0, (scoreNote 0 (3 / 8) 72 []) {snDots = 1}]
        @?= [(0, 1 / 4), (1 / 4, 1 / 8)]
      -- same 3/8 duration UNdotted (a triplet value): plain half — the
      -- rational alone cannot distinguish, the notation must
      run [g 0, scoreNote 0 (3 / 8) 72 []]
        @?= [(0, 3 / 16), (3 / 16, 3 / 16)]
      -- a run of graces (slide) stays short even in long mode
      let out = run [g 0, g 0, scoreNote 0 (1 / 4) 72 []]
      sum (map snd out) @?= 1 / 4
      assertBool "slide subnotes stay short"
        (all (< 1 / 8) (init (map snd out)))
  , testCase "grace: dangling at lane end still speaks" $ do
      let g = scoreNote (1 / 2) 0 74 [Grace]
          out = realizeGraceLane defaultOrnamentParams (Bpm 96) [g]
      [(snOnset n, snDur n) | n <- out] @?= [(1 / 2, 7 / 250)]
  , testCase "provenance: preparation-stage decisions reach the whys" $ do
      -- the reshapers and the grace steal happen before annotation; their
      -- whys must ride through to the performance all the same
      let src = T.unlines ["**kern", "ccq", "4c", "8d", "8e", "8f", "8g", "*-"]
          interp = defaultInterp
            {iExpress = defaultExpressParams {exInegal = 0.33}}
      case parseKern (Bpm 96) src >>= perform interp of
        Left e -> assertFailure e
        Right p -> do
          let rules = [whyRule w | (_, ws) <- perfWhys p, w <- ws]
          assertBool ("grace why present in " <> show (nub rules))
            ("grace" `elem` rules)
          assertBool ("inegales why present in " <> show (nub rules))
            ("inegales" `elem` rules)
  , testCase "grace: wtc2p13's four graces survive parse and perform" $ do
      present <- doesDirectoryExist corpusDir
      if not present then pure () else do
        src <- TIO.readFile (corpusDir </> "wtc2p13.krn")
        s <- either (assertFailure . ("parse: " <>)) pure
               (parseKern (Bpm 72) src)
        scGraceDropped s @?= 0
        length [ n | v <- scVoices s, n <- vNotes v
               , Grace `elem` snMarks n ] @?= 4
        p <- either (assertFailure . ("perform: " <>)) pure
               (perform defaultInterp s)
        -- realised: every performed note has width
        assertBool "no zero-duration notes reach the performance"
          (all (\n -> pnDur n > 0) (concat (perfTracks p)))
  , testCase "spelling: retained from the lexer, MIDI derived from it" $ do
      ntSpell (lexNoteTok "cc#") @?= Just (Spelled 0 1 5)
      ntPitch (lexNoteTok "cc#") @?= Just 73
      ntSpell (lexNoteTok "DD--") @?= Just (Spelled 1 (-2) 2)
      ntPitch (lexNoteTok "DD--") @?= Just 36
      spName (Spelled 0 1 5) @?= "C#5"
      spMidi (Spelled 5 (-2) 4) @?= 67 -- Abb4 sounds as G4, spelled apart
  , testCase "tie: dropped accidental on an adjacent close still merges" $ do
      -- kern law respells the accidental on every token; an encoder lapse
      -- ([4c# … 4c]) is the same staff position starting exactly where
      -- the open ends — one held note, not a stray close + EOF leftover
      let src = T.unlines ["**kern", "[4c#", "4c]", "*-"]
      case parseKern (Bpm 72) src of
        Left e -> assertFailure e
        Right s -> do
          scTieLeftovers s @?= 0
          [ (snDur n, snPitch n) | v <- scVoices s, n <- vNotes v ]
            @?= [(1 / 2, 61)] -- the open's pitch is authoritative
  , testCase "tie: a stray ] on a passing tone steals nothing" $ do
      -- wtc1p07:150 in miniature: 32an] mid-run while an a-flat tie is
      -- open from another beat. Same staff position, but NOT temporally
      -- adjacent — the passing tone must survive and the open must not
      -- be consumed by it
      let src = T.unlines ["**kern", "[2c#", "4d", "4c]", "*-"]
      case parseKern (Bpm 72) src of
        Left e -> assertFailure e
        Right s -> do
          scTieLeftovers s @?= 1 -- the c# open still flushes at EOF
          sort [ (snOnset n, snPitch n) | v <- scVoices s, n <- vNotes v ]
            @?= [(0, 61), (1 / 2, 62), (3 / 4, 60)]
  , testCase "counterpoint: a diminished sixth is not a fifth" $ do
      let sp l a o m t = (scoreNote t (1 / 4) m []) {snSpell = Just (Spelled l a o)}
          mkS v1 v2 =
            Score (Bpm 96) [Voice 0 v1, Voice 1 v2] 0 0 [(0, (4, 4))] 0 [] True
          -- C4->D4 under Abb4->Bbb4: seven semitones both times, but a
          -- sixth by letter — similar motion, and not parallel fifths
          dim6 = mkS [sp 0 0 4 60 0, sp 1 0 4 62 (1 / 4)]
                     [sp 5 (-2) 4 67 0, sp 6 (-2) 4 69 (1 / 4)]
          -- the genuine article still counts
          p5 = mkS [sp 0 0 4 60 0, sp 1 0 4 62 (1 / 4)]
                   [sp 4 0 4 67 0, sp 5 0 4 69 (1 / 4)]
      parallelPerfects dim6 @?= 0
      parallelPerfects p5 @?= 1
  , testCase "counterpoint: mixed spelling still pairs up" $ do
      -- a spelled P5 slice followed by the same P5 with spelling absent
      -- (generated notes, ornament auxiliaries) must still count: the
      -- chromatic class pairs the slices; spelling only vetoes quality
      let sp l a o m t = (scoreNote t (1 / 4) m []) {snSpell = Just (Spelled l a o)}
          bare m t = scoreNote t (1 / 4) m []
          mixed =
            Score (Bpm 96)
              [ Voice 0 [sp 0 0 4 60 0, bare 62 (1 / 4)]
              , Voice 1 [sp 4 0 4 67 0, bare 69 (1 / 4)] ]
              0 0 [(0, (4, 4))] 0 [] True
      parallelPerfects mixed @?= 1
  , testCase "transpose clears spelling (it cannot keep the notation)" $ do
      let sn = (scoreNote 0 (1 / 4) 60 []) {snSpell = Just (Spelled 0 0 4)}
          out k = [ s | (_, _, (s, _)) <-
                          annEvents (E.Modify (E.Transpose k)
                                       (E.Prim (E.Note (1 / 4) (sn, [])))) ]
      map snPitch (out 3) @?= [63]
      map snSpell (out 3) @?= [Nothing]
      map snSpell (out 0) @?= [Just (Spelled 0 0 4)] -- identity keeps it
  , testCase "turn: realised from its mark's intervals" $ do
      map snPitch
        (realizeNote defaultOrnamentParams (Bpm 96)
           (scoreNote 0 (1 / 4) 60 [Turn 1 2]))
        @?= [61, 60, 58, 60]
      map snPitch
        (realizeNote defaultOrnamentParams (Bpm 96)
           (scoreNote 0 (1 / 4) 60 [InvTurn 1 2]))
        @?= [58, 60, 61, 60]
  , testCase "turn: auxiliaries are the key's diatonic neighbours" $ do
      -- a C major context; the turn sits on E, whose diatonic neighbours
      -- are F (+1) and D (-2) — the old whole-tone default said F#
      let src = T.unlines
            [ "**kern", "*MM96", "8c", "8d", "8e", "8f"
            , "8g", "8a", "8b", "8cc", "2eS", "4c", "*-" ]
      case parseKern (Bpm 96) src >>= perform calmInterp of
        Left e -> assertFailure e
        Right p -> do
          let subs = [ pnPitch n
                     | n <- concat (perfTracks p), pnSrcOn n == 1 ]
          subs @?= [65, 64, 62, 64]
  , testCase "turn: the minor tonic closes from its leading tone" $ do
      -- G# minor, turn on the tonic: the lower neighbour is F## — one
      -- semitone below, a double accidental no single-accidental letter
      -- search could reach; the upper is A# (+2)
      let src = T.unlines
            [ "**kern", "*MM96", "8g#", "8b", "8d#", "8g#"
            , "8g#", "8b", "8d#", "8g#", "2g#S", "4g#", "*-" ]
      case parseKern (Bpm 96) src >>= perform calmInterp of
        Left e -> assertFailure e
        Right p -> do
          let subs = [ pnPitch n
                     | n <- concat (perfTracks p), pnSrcOn n == 1 ]
          subs @?= [70, 68, 67, 68]
  , testCase "turn: neighbours are letters, not accidentals (wtc1p04)" $ do
      -- the C#-minor A# turn (wtc1p04:179): the scale's pc set contains
      -- A-natural one semitone below A#, but a same-letter accidental is
      -- not a neighbour — the spelled lower neighbour is G#, two below
      present <- doesDirectoryExist corpusDir
      if not present then pure () else do
        src <- TIO.readFile (corpusDir </> "wtc1p04.krn")
        p <- either assertFailure pure
               (parseKern (Bpm 72) src >>= perform calmInterp)
        s <- either assertFailure pure (parseKern (Bpm 72) src)
        let turns = [ (snSource n, snPitch n)
                    | v <- scVoices s, n <- vNotes v
                    , any (\m -> case m of Turn _ _ -> True; _ -> False)
                        (snMarks n) ]
        assertBool "the piece has its turns" (not (null turns))
        forM_ turns $ \((o, _), tp) -> do
          let subs = [ pnPitch n | n <- concat (perfTracks p)
                     , pnSrcOn n == o, abs (pnPitch n - tp) <= 2 ]
          assertBool ("lower aux is the letter below: " <> show subs)
            ((tp - 2) `elem` subs && (tp - 1) `notElem` subs)
  , testCase "anacrusis: the bar grid anchors at the first full bar" $ do
      -- an eighth-note pickup: the first meter entry moves to the
      -- pickup's end so every metrical consumer (Sloboda accents, bar
      -- arches, explain --bar) starts bar 1 where the edition prints it
      let pickup = T.unlines
            ["**kern", "*M4/4", "8c", "=1", "2d", "2e", "=2", "4f", "*-"]
          leadingBar = T.unlines ["**kern", "*M4/4", "=1-", "4c", "*-"]
      case parseKern (Bpm 72) pickup of
        Left e -> assertFailure e
        Right s -> map fst (scMeter s) @?= [1 / 8]
      case parseKern (Bpm 72) leadingBar of
        Left e -> assertFailure e
        Right s -> map fst (scMeter s) @?= [0]
  , testCase "parser: mid-piece *MM is a loud error, not a silent latch" $ do
      let src = T.unlines ["**kern", "*MM96", "4c", "*MM120", "4d", "*-"]
      case parseKern (Bpm 72) src of
        Left e -> assertBool e ("*MM" `isInfixOf` e)
        Right _ -> assertFailure "mid-piece tempo change absorbed silently"
      -- restating the SAME tempo is harmless
      let same = T.unlines ["**kern", "*MM96", "4c", "*MM96", "4d", "*-"]
      case parseKern (Bpm 72) same of
        Left e -> assertFailure e
        Right s -> scTempo s @?= Bpm 96
      -- a late FIRST *MM would retroactively re-tempo the music already
      -- heard at the fallback: same guard, even with no earlier *MM
      let late = T.unlines ["**kern", "4c", "*MM120", "4d", "*-"]
      case parseKern (Bpm 72) late of
        Left e -> assertBool e ("*MM" `isInfixOf` e)
        Right _ -> assertFailure "late first *MM absorbed silently"
      -- unless it merely states the effective (fallback) tempo
      case parseKern (Bpm 120) late of
        Left e -> assertFailure e
        Right s -> scTempo s @?= Bpm 120
  , testCase "parser: conflicting *MM in one record is an error" $ do
      let src = T.unlines ["**kern\t**kern", "*MM96\t*MM120", "4c\t4e", "*-\t*-"]
      case parseKern (Bpm 72) src of
        Left e -> assertBool e ("conflicting" `isInfixOf` e)
        Right _ -> assertFailure "conflicting record tempos accepted"
  , testCase "parser: additive meters are not quietly truncated" $ do
      -- *M3+2/8 must not read as 3/8; with no valid meter record the
      -- map stays empty (metrical dynamics degrade gracefully)
      let src = T.unlines ["**kern", "*M3+2/8", "4c", "*-"]
      case parseKern (Bpm 72) src of
        Left e -> assertFailure e
        Right s -> scMeter s @?= []
  , testCase "parser: CRLF input parses identically to LF" $ do
      let lf = T.unlines ["**kern", "*M4/4", "4c", ".", "4d", "*-"]
          crlf = T.replace "\n" "\r\n" lf
      show (parseKern (Bpm 72) crlf) @?= show (parseKern (Bpm 72) lf)
  , testCase "suspension: the prepared dissonance is seen and spoken" $ do
      -- v0 holds C4 for a whole note; v1 strikes G4 (consonant with it)
      -- then D4 beneath the held C — a 2nd forms MID-NOTE. The held
      -- note goes fully legato, the next lane note resolves gently,
      -- and the tempo leans into the dissonant moment.
      let s = Score (Bpm 120)
                [ Voice 0 [ scoreNote 0 1 60 []
                          , scoreNote 1 (1 / 2) 59 [] ]
                , Voice 1 [ scoreNote 0 (1 / 2) 67 []
                          , scoreNote (1 / 2) 1 62 [] ] ]
                0 0 [(0, (4, 4))] 0 [] True
          interp = calmInterp
            { iExpress = defaultExpressParams
                { exEnsemble = 0, exArchPiece = 0, exArchGroup = 0
                , exDisVel = 0, exMelCharge = 0, exHarmCharge = 0
                , exSubjectVel = 0
                , exSusLean = 0.02 } } -- fit-vetoed default; on for the test
      case perform interp s of
        Left e -> assertFailure e
        Right p -> do
          let rules = [whyRule w | (_, ws) <- perfWhys p, w <- ws]
          assertBool ("suspension why in " <> show (nub rules))
            ("suspension" `elem` rules)
          assertBool "resolution why" ("resolution" `elem` rules)
          -- the agogic lean: tempo dips just before the mid-note
          -- dissonance at 1/2, and only there
          let Bpm before = tempoAt (perfTempoMap p) (3 / 8)
              Bpm clear = tempoAt (perfTempoMap p) (5 / 4)
          assertBool ("leans in: " <> show before) (before < 119.9)
          assertBool ("a tempo elsewhere: " <> show clear)
            (abs (clear - 120) < 0.01)
  , testCase "dialogue: the listeners yield while a voice speaks" $ do
      let mk vi ons ps =
            Voice vi [scoreNote o (1 / 8) p [] | (o, p) <- zip ons ps]
          motif = [60, 64, 62, 67, 65, 69]
          a = mk 0 [fromIntegral i / 8 | i <- [0 :: Int ..]] motif
          b = mk 1 [1/2 + fromIntegral i / 8 | i <- [0 :: Int ..]]
                (map (+ 5) motif)
          s = Score (Bpm 96) [a, b] 0 0 [(0, (4, 4))] 0 [] True
      case perform defaultInterp s of
        Left e -> assertFailure e
        Right p -> do
          let deltas = [whyDelta w | (_, ws) <- perfWhys p, w <- ws
                       , whyRule w == "dialogue" ]
          assertBool ("takes the floor: " <> show deltas)
            (any ("takes the floor" `isInfixOf`) deltas)
          assertBool ("yields: " <> show deltas)
            (any ("yields" `isInfixOf`) deltas)
  , testCase "echo: sequence repetitions terrace down" $ do
      -- the sequence fixture from the detector's own test: a figure
      -- restated at a consistent step — later statements step down
      let line' fig step k =
            [ scoreNote (fromIntegral (it * 4 + j) / 8) (1 / 8)
                (p + it * step) []
            | it <- [0 .. k - 1], (j, p) <- zip [0 ..] fig ]
          s = Score (Bpm 96) [Voice 0 (line' [60, 64, 62, 65] (-2) 4)]
                0 0 [(0, (4, 4))] 0 [] True
      case perform defaultInterp s of
        Left e -> assertFailure e
        Right p -> do
          let rules = [whyRule w | (_, ws) <- perfWhys p, w <- ws]
          assertBool ("echo why in " <> show (nub rules))
            ("echo" `elem` rules)
  , testCase "easing recovery is instant even off the tempo grid" $ do
      -- arrival at 4 + 1/32 falls between eighth-note grid points; the
      -- boundary must join the grid so the a tempo lands exactly there
      let ag = defaultAgogicParams {agRitSpan = 0, agOpenPush = 0}
          tm = tempoMap ag [] [(4 + 1 / 32, 0.2, 1 / 2)] [] [] []
                 (Bpm 100) 8
          at x = case takeWhile ((<= x) . fst) tm of
            [] -> 100; xs -> let Bpm b = snd (last xs) in b
      assertBool "eased just before" (at 4 < 99)
      assertBool ("a tempo AT the arrival: " <> show (at (4 + 1 / 32)))
        (abs (at (4 + 1 / 32) - 100) < 0.01)
  , testCase "suspension: a rest or a leap is not a resolution" $ do
      let mkS v0notes =
            Score (Bpm 120)
              [ Voice 0 v0notes
              , Voice 1 [ scoreNote 0 (1 / 4) 67 []
                        , scoreNote (1 / 4) (3 / 4) 62 [] ] ]
              0 0 [(0, (4, 4))] 0 [] True
          interp = calmInterp
            { iExpress = defaultExpressParams
                {exEnsemble = 0, exArchPiece = 0, exArchGroup = 0} }
          rulesOf s = case perform interp s of
            Left e -> error e
            Right p -> [whyRule w | (_, ws) <- perfWhys p, w <- ws]
          -- suspension on the held C; the next lane note after a REST
          gapped = mkS [scoreNote 0 (1 / 2) 60 [], scoreNote 1 (1 / 4) 59 []]
          -- adjacent but a LEAP down a seventh
          leapt = mkS [scoreNote 0 (1 / 2) 60 [], scoreNote (1 / 2) (1 / 4) 50 []]
      assertBool "gapped: suspension seen" ("suspension" `elem` rulesOf gapped)
      assertBool "gapped: no resolution label"
        ("resolution" `notElem` rulesOf gapped)
      assertBool "leapt: no resolution label"
        ("resolution" `notElem` rulesOf leapt)
  , testCase "echo: voices repeating together are one seam, not two" $ do
      let line' fig step k =
            [ scoreNote (fromIntegral (it * 4 + j) / 8) (1 / 8)
                (p + it * step) []
            | it <- [0 .. k - 1], (j, p) <- zip [0 ..] fig ]
          oneV = Score (Bpm 96) [Voice 0 (line' [60, 64, 62, 65] (-2) 4)]
                   0 0 [(0, (4, 4))] 0 [] True
          twoV = Score (Bpm 96)
                   [ Voice 0 (line' [60, 64, 62, 65] (-2) 4)
                   , Voice 1 (line' [72, 76, 74, 77] (-2) 4) ]
                   0 0 [(0, (4, 4))] 0 [] True
      sqSeams (findSequences twoV) @?= sqSeams (findSequences oneV)
  , testCase "a vetoed (zero-depth) easing is a true no-op" $ do
      -- with an arch active, an off-grid easing boundary would change
      -- where the arch is sampled even at depth 0 — the vetoed cadence
      -- and boundary rules must leave the map byte-identical
      let ag = defaultAgogicParams {agRitSpan = 0, agOpenPush = 0}
          with0 = tempoMap ag [(0, 8, 0.05)] [(4 + 1 / 32, 0, 1 / 2)]
                    [] [] [] (Bpm 100) 8
          without = tempoMap ag [(0, 8, 0.05)] [] [] [] [] (Bpm 100) 8
      with0 @?= without
  , testCase "suspension: a re-attack or a still-dissonant step is no resolution" $ do
      let mkS second =
            Score (Bpm 120)
              [ Voice 0 [scoreNote 0 (1 / 2) 60 [], second]
              , Voice 1 [ scoreNote 0 (1 / 4) 67 []
                        , scoreNote (1 / 4) (3 / 4) 62 [] ] ]
              0 0 [(0, (4, 4))] 0 [] True
          interp = calmInterp
            { iExpress = defaultExpressParams
                {exEnsemble = 0, exArchPiece = 0, exArchGroup = 0} }
          rulesOf s = case perform interp s of
            Left e -> error e
            Right p -> [whyRule w | (_, ws) <- perfWhys p, w <- ws]
          -- adjacent, but the same pitch again: a re-attack
          reattack = mkS (scoreNote (1 / 2) (1 / 4) 60 [])
          -- adjacent and a step, but C#4 against the held D is MORE
          -- dissonant than the suspension's peak — nothing resolved
          stillDiss = mkS (scoreNote (1 / 2) (1 / 4) 61 [])
      assertBool "re-attack: no resolution label"
        ("resolution" `notElem` rulesOf reattack)
      assertBool "still dissonant: no resolution label"
        ("resolution" `notElem` rulesOf stillDiss)
  , testCase "metrical residuals reproduce the historical accents" $ do
      -- the 2026-09-01 migration: absolute 12/6/3 under select-one
      -- semantics == residuals 6/3/3 under additive semantics, in BOTH
      -- meter parities (downbeats belong to the half-bar level in every
      -- meter, which is what makes the odd-meter case exact)
      let dp = defaultDynParams {dyArch = 0, dyHighLoud = 0}
          note t = scoreNote t (1 / 16) 66 []
          velsIn meter ts =
            [ v - round (dyBase dp)
            | (v, _) <- dynamicsLane' dp [(0, meter)]
                          (replicate (length ts) False) (map note ts) ]
      -- 4/4: downbeat, mid-bar, beat, subdivision
      velsIn (4, 4) [0, 1 / 2, 1 / 4, 1 / 8] @?= [12, 6, 3, 0]
      -- 3/4: downbeat still 12; no mid-bar; beats 3
      velsIn (3, 4) [0, 1 / 4, 1 / 8] @?= [12, 3, 0]
  , testCase "easing shape: kinematic braking, not the vetoed line" $ do
      let ag = defaultAgogicParams {agRitSpan = 0, agOpenPush = 0}
          tm = tempoMap ag [] [(4, 0.3, 1)] [] [] [] (Bpm 100) 8
          at x = case takeWhile ((<= x) . fst) tm of
            [] -> 100; xs -> let Bpm b = snd (last xs) in b
          -- Friberg–Sundberg at x = 0.5, w = 0.7, q = 2:
          -- 100 * sqrt(1 + (0.49 - 1) * 0.5) ≈ 86.31; linear says 85
          fs = 100 * sqrt (1 + (0.7 ** 2 - 1) * 0.5)
      assertBool ("kinematic at midpoint: " <> show (at 3.5))
        (abs (at 3.5 - fs) < 0.1)
  , testCase "tempo giusto: the notation implies the tempo" $ do
      let mkS m ds =
            Score (Bpm 72)
              [Voice 0 [ scoreNote (fromIntegral i * d) d 60 []
                       | (i, d) <- zip [0 :: Int ..] ds ]]
              0 0 [(0, m)] 0 [] False
          quarters = mkS (4, 4) (replicate 16 (1 / 4))
          sixteenths = mkS (4, 4) (replicate 64 (1 / 16))
          gigue = mkS (6, 8) (replicate 16 (1 / 8))
          breve = mkS (2, 2) (replicate 16 (1 / 4))
      -- FITTED directions (ASAP, 2026-09-01) — the data overturned the
      -- "fast notes ask for room" prior: dense figuration STRIDES
      assertBool "sixteenth figuration strides"
        (tempoGiusto sixteenths > tempoGiusto quarters)
      assertBool "compound eighths dance"
        (tempoGiusto gigue > tempoGiusto quarters)
      assertBool "alla breve flows fastest (Kirnberger vindicated)"
        (tempoGiusto breve > tempoGiusto gigue)
      -- and the parser records whether *MM spoke at all
      case parseKern (Bpm 72) (T.unlines ["**kern", "4c", "*-"]) of
        Right s -> scTempoDeclared s @?= False
        Left e -> assertFailure e
      case parseKern (Bpm 72) (T.unlines ["**kern", "*MM96", "4c", "*-"]) of
        Right s -> scTempoDeclared s @?= True
        Left e -> assertFailure e
  , testCase "scl: a bare integer is a ratio (2 = the octave)" $ do
      let scl = T.unlines $
            ["! t", "t", "12", "!"]
              <> map (T.pack . show) ([100.0, 200 .. 1100] :: [Double])
              <> ["2"]
      case parseScl scl of
        Left e -> assertFailure e
        Right t -> t @?= equalTable
  ]
  where
    -- every non-fermata agogic layer zeroed, expression off: tempo maps
    -- in these tests are base everywhere except what the test creates
    calmInterp = defaultInterp
      { iAgogics = defaultAgogicParams
          { agRitSpan = 0, agOpenPush = 0, agCadenceDepth = 0
          , agBoundaryEase = 0, agSubjectPush = 0 }
      , iExpress = defaultExpressParams {exExpression = 0, exEnsemble = 0}
      }
    tempoAt tm t = snd (last (takeWhile ((<= t) . fst) tm))
    nubOrd = map head' . groupBy (==) . sort
    head' (x : _) = x
    head' [] = error "impossible"
    withCorpus f k = do
      present <- doesDirectoryExist corpusDir
      if not present then pure () else do
        src <- TIO.readFile (corpusDir </> f)
        s <- either (assertFailure . ("parse: " <>)) pure
               (parseKern (Bpm 72) src)
        p <- either (assertFailure . ("perform: " <>)) pure
               (perform defaultInterp s)
        k p s


-- ---------------------------------------------------------------------
-- SOTA layer: harmony, subject, KTH reshapers

sota :: TestTree
sota = testGroup "sota"
  [ testCase "harmony: WTC keys found (KK profiles + Viterbi)" $ do
      present <- doesDirectoryExist corpusDir
      if not present then pure () else do
        let expect = [ ("wtc1p01", 0, True), ("wtc1f01", 0, True)
                     , ("wtc1f04", 1, False), ("wtc2f01", 0, True) ]
        mapM_ (\(f, k, mj) -> do
                 h <- harmOf f
                 (hKeyAt h 0, hMajorAt h 0) @?= (k, mj))
          expect
  , testCase "harmony: opening keys vs WTC ground truth (>= 95/96)" $ do
      present <- doesDirectoryExist corpusDir
      if not present then pure () else do
        results <- mapM
          (\(bk, nn, kind) -> do
             let name = "wtc" <> show (bk :: Int) <> [kind]
                          <> (if nn < 10 then '0' : show nn else show nn)
                 expPc = [0,0,1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10,11,11]
                           !! (nn - 1)
             h <- harmOf name
             pure (name, (hKeyAt h 0, hMajorAt h 0) == (expPc, odd nn)))
          [(bk, nn, k) | bk <- [1, 2], nn <- [1 .. 24], k <- "pf"]
        let wrong = [n | (n, False) <- results]
        assertBool ("wrong keys: " <> show wrong) (length wrong <= 1)
  , testCase "adaptive temperament: settled major third goes just" $ do
      -- root C (Werckmeister offset 0): at full stability the E sits at
      -- the just major third, -13.7 cents — flatter than Werckmeister's
      -- -9.775, which is the audible point of the whole feature
      let Cents atRest = adaptiveCents werckmeister3 0 1 64
          Cents moving = adaptiveCents werckmeister3 0 0 64
          Cents just3 = justOffsetFor 4
      assertBool "settled = just third" (abs (atRest - just3) < 0.01)
      assertBool "moving = Werckmeister" (abs (moving - (-9.775)) < 0.01)
  , testCase "harmony: stability rises while the root holds" $ do
      -- a held C major triad: beat 1 unstable, beat 3+ settled
      let notes = [(0, 2, 48), (0, 2, 64), (0, 2, 67)]
          h = analyzeHarmony [(0, (4, 4))] notes 2
      hStabilityAt h 0 @?= 0
      assertBool "settled by the third beat" (hStabilityAt h (3 / 4) >= 1)
  , testCase "harmony: melodic charge table (Friberg 1991)" $ do
      melodicCharge 60 0 @?= 0 -- root
      melodicCharge 67 0 @?= 1 -- fifth
      melodicCharge 61 0 @?= 6.5 -- flat second, heaviest
  , testCase "subject: the C major fugue states, the prelude does not" $ do
      present <- doesDirectoryExist corpusDir
      if not present then pure () else do
        f <- scoreOf "wtc1f01"
        p <- scoreOf "wtc1p01"
        let entries = subjectEntries f
        assertBool "fugue entries found" (length entries >= 40)
        subjectEntries p @?= []
  , testProperty "reshapers never produce non-positive durations, any k" $
      forAll ((,) <$> choose (0, 3.0) <*> genLane) $ \(k, l) ->
        all ((> 0) . snDur) (uphillLane k (doubleDurLane k l))
  , testCase "sequences: a real sequence detected, invariants enforced" $ do
      let line ps step k =
            -- figure of 4 notes restated k times, transposed by step
            [ scoreNote (fromIntegral (it * 4 + j) / 8) (1 / 8)
                (p + it * step) []
            | it <- [0 .. k - 1], (j, p) <- zip [0 ..] ps ]
          mkS ns = Score (Bpm 96) [Voice 0 ns] 0 0 [(0, (4, 4))] 0 [] True
          figure = [60, 64, 62, 65] -- direction changes, distinctive
          seqS = mkS (line figure (-2) 4)
      assertBool "descending sequence detected"
        (not (null (sqSpans (findSequences seqS))))
      -- inconsistent transposition (+3 then +6) is two coincidences
      let jumpy = mkS (line figure 3 2
                        <> [ scoreNote ((8 + fromIntegral j) / 8) (1 / 8)
                               (p + 9) []
                           | (j, p) <- zip [0 ..] figure ])
      sqSpans (findSequences jumpy) @?= []
      -- stretched onset gaps break attack rhythm even with equal durs
      let stretched = mkS
            (line figure (-2) 1
              <> [ scoreNote (1 / 2 + fromIntegral j / 4) (1 / 8)
                     (p - 2) []
                 | (j, p) <- zip [0 ..] figure ]
              <> [ scoreNote (3 / 2 + fromIntegral j / 8) (1 / 8)
                     (p - 4) []
                 | (j, p) <- zip [0 ..] figure ])
      sqSpans (findSequences stretched) @?= []
  , testCase "imitation: an echoed distinctive motif is a take" $ do
      let mk vi ons ps =
            Voice vi [scoreNote o (1 / 8) p [] | (o, p) <- zip ons ps]
          motif = [60, 64, 62, 67, 65, 69] -- direction changes galore
          a = mk 0 [fromIntegral i / 8 | i <- [0 :: Int ..]] motif
          b = mk 1 [1/2 + fromIntegral i / 8 | i <- [0 :: Int ..]] (map (+ 5) motif)
          s = Score (Bpm 96) [a, b] 0 0 [(0, (4, 4))] 0 [] True
          im = findImitation s
      map (\(t', v, _) -> (t', v)) (imTakes im) @?= [(1 / 2, 1)]
  , testCase "imitation: a scale run is figuration, not speech" $ do
      let mk vi ons ps =
            Voice vi [scoreNote o (1 / 8) p [] | (o, p) <- zip ons ps]
          run = [60, 62, 64, 65, 67, 69]
          s = Score (Bpm 96)
                [mk 0 [fromIntegral i / 8 | i <- [0 :: Int ..]] run, mk 1 [1/2 + fromIntegral i / 8 | i <- [0 :: Int ..]] run]
                0 0 [(0, (4, 4))] 0 [] True
      imTakes (findImitation s) @?= []
  , testCase "dialogue: the fugue converses, the prelude does not" $ do
      present <- doesDirectoryExist corpusDir
      if not present then pure () else do
        pf <- performOf "wtc1f01"
        pp <- performOf "wtc1p01"
        let dialogues q =
              length [ () | (_, ws) <- perfWhys q, w <- ws
                     , "dialogue" `isInfixOf` whyRule w ]
        assertBool "fugue has dialogue whys" (dialogues pf > 20)
        dialogues pp @?= 0
  , testProperty "uphill reshaper preserves lane duration sum" $
      forAll genLane $ \l ->
        sum (map snDur (uphillLane 0.05 l)) === sum (map snDur l)
  , testProperty "double-duration reshaper preserves lane duration sum" $
      forAll genLane $ \l ->
        sum (map snDur (doubleDurLane 0.07 l)) === sum (map snDur l)
  , testCase "accelerating trill still sums to the note" $ do
      let sn = scoreNote 0 (1 / 2) 60 [Trill 2]
          out = realizeNote defaultOrnamentParams (Bpm 96) sn
      sum (map snDur out) @?= 1 / 2
      assertBool "accelerates: first subnote longest"
        (snDur (head out) > snDur (last out))
  , testCase "long trill closes with the Nachschlag" $ do
      let sn = scoreNote 0 1 60 [Trill 2]
          out = realizeNote defaultOrnamentParams (Bpm 96) sn
          ps = map snPitch out
      assertBool "enough subnotes" (length ps >= 8)
      drop (length ps - 2) ps @?= [58, 60] -- lower turn into the main
  ]
  where
    harmOf f = do
      s <- scoreOf f
      let notes = [ (snOnset n, snDur n, snPitch n)
                  | v <- scVoices s, n <- vNotes v ]
          end' = maximum (0 : [o + d | (o, d, _) <- notes])
      pure (analyzeHarmony (scMeter s) notes end')
    scoreOf f = do
      src <- TIO.readFile (corpusDir </> f <> ".krn")
      either (assertFailure . ("parse: " <>)) pure (parseKern (Bpm 72) src)
    performOf f = do
      s <- scoreOf f
      either (assertFailure . ("perform: " <>)) pure
        (perform defaultInterp s)


-- ---------------------------------------------------------------------
-- ported tool-test helpers (bake / piece fit / maestro)

fitRec :: PF.PieceRec
fitRec = PF.PieceRec "wtc1p03" 4 (Just (80.0, 100.0, 84.0))
  (Just (PF.FitLayer [("cadence_depth", 0.04)] 0.1 0.3 (Just 0.05)
           True))
  (Just (PF.FitLayer [("vel_highloud", 1.0)] 0.3 0.35 (Just (-0.02))
           False))

countOf :: String -> String -> Int
countOf needle hay = length
  [ () | s <- suffixesS hay, needle `isPrefixOfS` s ]
  where
    isPrefixOfS a b = take (length a) b == a
    suffixesS s = s : case s of
      [] -> []
      (_ : rest) -> suffixesS rest

sectionBody :: String -> String -> String
sectionBody piece out =
  let ls = lines out
      after = drop 1 (dropWhile (/= ("[piece." <> piece <> "]")) ls)
   in unlines (takeWhile (not . ("[" `isPrefixOfL`)) after)
  where isPrefixOfL a b = take (length a) b == a

assertClose :: String -> Double -> Double -> Assertion
assertClose lbl got want =
  assertBool (lbl <> ": " <> show got <> " /= " <> show want)
    (abs (got - want) < 1e-9)

vlq :: Int -> [Word8]
vlq n0 = go (n0 `div` 128) [fromIntegral (n0 `mod` 128)]
  where
    go 0 acc = acc
    go n acc = go (n `div` 128)
      (fromIntegral (n `mod` 128 + 128) : acc)

-- | Format-1 file: a tempo change mid-way, running status, two notes.
synthSmf :: BS.ByteString
synthSmf = BS.pack (mthd <> chunk trk0 <> chunk trk1)
  where
    division = 480
    be32 n = [ fromIntegral (n `div` 16777216)
             , fromIntegral (n `div` 65536 `mod` 256)
             , fromIntegral (n `div` 256 `mod` 256)
             , fromIntegral (n `mod` 256) ]
    be16 n = [fromIntegral (n `div` 256), fromIntegral (n `mod` 256)]
    be24 n = drop 1 (be32 n)
    mthd = map (fromIntegral . fromEnum) ("MThd" :: String)
             <> be32 (6 :: Int) <> be16 (1 :: Int) <> be16 (2 :: Int)
             <> be16 division
    chunk body = map (fromIntegral . fromEnum) ("MTrk" :: String)
                   <> be32 (length body) <> body
    trk0 = vlq 0 <> [0xFF, 0x51, 0x03] <> be24 (500000 :: Int)
             <> vlq (division * 2) <> [0xFF, 0x51, 0x03]
             <> be24 (250000 :: Int)
             <> vlq 0 <> [0xFF, 0x2F, 0x00]
    trk1 = vlq 0 <> [0x90, 0x3C, 0x50]
             <> vlq division <> [0x3C, 0x00] -- running status off
             <> vlq (division * 2) <> [0x90, 0x40, 0x40]
             <> vlq division <> [0x80, 0x40, 0x00]
             <> vlq 0 <> [0xFF, 0x2F, 0x00]

-- | A synthetic score + a warped "performance" of it, starting
-- mid-file at t=3.
synthPerf :: (Double -> Double)
          -> ([(Int, [(Int, Int)])], [Smf.SmfNote])
synthPerf curve = (score, notes)
  where
    score =
      [ ( i * 480
        , (60 + i `mod` 12, 0)
            : [(48 + i `mod` 12, 1) | i `mod` 4 == 0] )
      | i <- [0 .. 63] ]
    starts = scanl (\t i -> t + curve (fromIntegral i)) 3.0 [0 .. 62]
    notes =
      [ Smf.SmfNote p (60 + i `mod` 20) (t + 0.004 * fromIntegral k)
          (t + 0.3) 0 0
      | ((i, (_, ps)), t) <- zip (zip [0 :: Int ..] score) starts
      , (k, (p, _c)) <- zip [0 :: Int ..] ps ]

insertWithList :: Eq k => k -> v -> [(k, [v])] -> [(k, [v])]
insertWithList k v [] = [(k, [v])]
insertWithList k v ((k2, vs) : rest)
  | k == k2 = (k2, vs <> [v]) : rest
  | otherwise = (k2, vs) : insertWithList k v rest

minimumOn :: Ord b => (a -> b) -> [a] -> a
minimumOn f = foldr1 (\a b -> if f a <= f b then a else b)
