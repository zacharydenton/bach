-- | Align MAESTRO WTC performances to otb scores, no ASAP required.
-- Ports tools/maestro_align.py; the IO orchestration lives in Main
-- (@otb maestro-align@), this module is the pure machinery.
--
-- A MAESTRO competition file may hold a prelude, its fugue, or both,
-- with no segmentation and no alignment. The aligner finds each
-- candidate piece inside the file by SUBSEQUENCE DTW over chordified
-- pitch sets (open begin/end, so segmentation falls out of the
-- alignment), pairs notes inside matched chords, derives a beat grid
-- by local-linear fits over the aligned onsets, and emits ASAP's own
-- file formats into a mirror tree — corpus/maestro-wtc/Bach/\<Kind\>/
-- bwv_NNN/ — so the entire downstream stack (OTB.Bridge, OTB.Eval,
-- otb fit) consumes the new performances unchanged.
--
-- The DTW arithmetic deliberately runs in 32-bit 'Float' — the
-- reference used numpy float32, and the backtracking's epsilon
-- comparisons only reproduce the same paths if the rounding does too.
-- The beat grid's local line fits are closed-form least squares where
-- the reference called np.polyfit (SVD); they agree to ~1e-12
-- relative, far below the 1 ms the grid is read at, and the emitted
-- .match rows are integer-rounded so they agree exactly.
--
-- Derived beat grids use a uniform QUARTER-NOTE grid (score
-- annotations carry @db,4/4@ every fourth beat regardless of notated
-- meter): the eval stack only needs the two sides of a source dir to
-- agree, and a uniform grid avoids exporting meter maps.
--
-- Quality gates (rejects logged, never emitted): score-note match
-- rate >= 85%, monotone beat grid, |median (aligned onset - beat
-- prediction)| < 30 ms.
--
-- VALIDATED against ASAP ground truth exactly like the Python was
-- (the catalog files ASAP also aligned; see @otb maestro-align
-- --validate@): note agreement over the common score, derived-beat vs
-- hand-annotated beat |delta|, acceptance gate agree >= 0.95 with
-- beat <= 20 ms or agree >= 0.97 with beat <= 60 ms.
--
-- License: GPL-2.0-or-later.
module OTB.MaestroAlign
  ( Pair
  , AlignStats (..)
  , AlignOutcome (..)
  , scoreChords
  , perfChords
  , dtwPath
  , pairNotes
  , beatGridTimes
  , recover
  , alignOne
  , interpTime
  , pyRepr
  , matchText
  , annotationsText
  , provJson
  , Verdict (..)
  , validateOne
  ) where

import Data.IntSet qualified as IntSet
import Data.List (foldl', sort, sortOn)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as BV
import Data.Vector.Unboxed qualified as U
import GHC.Float (double2Float, float2Double)
import Numeric (floatToDigits, showFFloat)
import OTB.Bridge (MatchPerf (..), MatchRow (..), wnTicks)
import OTB.Eval (perfBeatTimes, scoreBeatPositions)
import OTB.Player (PerfNote (..), Performance (..))
import OTB.Smf (SmfNote (..))
import OTB.Units (WholeNotes (..))

chordWindowS :: Double
chordWindowS = 0.030 -- perf notes within 30 ms strike as one chord

gapScore :: Float
gapScore = 0.6 -- cost of skipping a score chord (performer omission)

gapPerf :: Float
gapPerf = 0.35 -- cost of skipping a perf chord (ornament, extra strike)

secPerTick :: Double
secPerTick = 500000 / 1e6 / 480 -- the .match clock we emit

spellTable :: [(String, String)]
spellTable =
  [ ("C", "n"), ("C", "#"), ("D", "n"), ("E", "b"), ("E", "n")
  , ("F", "n"), ("F", "#"), ("G", "n"), ("A", "b"), ("A", "n")
  , ("B", "b"), ("B", "n") ]

-- | (ticks, pitch, ch, perf-note index).
type Pair = (Int, Int, Int, Int)

data AlignStats = AlignStats
  { stScoreNotes :: !Int
  , stMatched :: !Int
  , stRate :: !Double
  , stDeleted :: !Int
  , stInserted :: !Int
  , stSpan :: Maybe (Double, Double)
  , stMedianDevMs :: Maybe Double
  }

data AlignOutcome = AlignOutcome
  { aoReject :: !Bool
  , aoPairs :: [Pair]
  , aoDeleted :: [(Int, Int, Int)]
  , aoInserted :: [Int]
  , aoGrid :: [Int]
  , aoTimes :: [Double]
  , aoScore :: [(Int, [(Int, Int)])]
  , aoStats :: AlignStats
  }

-- ---------------------------------------------------------------------
-- chordification

-- | [(ticks, [(pitch, ch)])] from the performance's notated
-- coordinates. (pitch, ch) multiplicity is PRESERVED: two voices
-- striking the same pitch at the same moment are two notes (wtc1p12
-- has 45 such unisons), collapsed only across a single note's ornament
-- subnotes (which share all three coordinates).
scoreChords :: Performance -> [(Int, [(Int, Int)])]
scoreChords (Performance _ tracks _ _ _) =
  sortOn fst
    [ (t, sort ps)
    | (t, ps) <- Map.toList (foldl' add Map.empty triples) ]
  where
    triples = Set.toList . Set.fromList $
      [ ( round (fromRational w * fromIntegral wnTicks :: Double)
        , pnSrcPitch pn, pnChannel pn )
      | pn <- concat tracks
      , let WholeNotes w = pnSrcOn pn ]
    add m (t, p, c) = Map.insertWith (<>) t [(p, c)] m

-- | [(on_s, [note index])] — note-ons grouped within 'chordWindowS'.
-- The on_s is the FIRST note's (the group's anchor), as the reference.
perfChords :: BV.Vector SmfNote -> [(Double, [Int])]
perfChords notes = reverse (map (fmap reverse) grouped)
  where
    idxs = sortOn (\i -> snOnS (notes BV.! i))
             [0 .. BV.length notes - 1]
    grouped = foldl' step [] idxs
    step [] i = [(snOnS (notes BV.! i), [i])]
    step out@((t0, is) : rest) i
      | snOnS (notes BV.! i) - t0 <= chordWindowS = (t0, i : is) : rest
      | otherwise = (snOnS (notes BV.! i), [i]) : out

-- ---------------------------------------------------------------------
-- subsequence DTW (open begin and end on the performance side)

-- | Chord-level alignment path [(i, j)] of score chords into perf
-- chords. Float32 throughout, matching the reference's numpy dtype.
dtwPath :: [(Int, [(Int, Int)])] -> [(Double, [Int])]
        -> BV.Vector SmfNote -> [(Int, Int)]
dtwPath sc pc notes = backtrack
  where
    m = length sc
    n = length pc
    sSets = BV.fromList
      [ U.fromList (dedupSort [p | (p, _) <- ps]) | (_, ps) <- sc ]
    pSets = BV.fromList
      [ U.fromList (dedupSort [snPitch (notes BV.! i) | i <- is])
      | (_, is) <- pc ]
    dedupSort = uniqAsc . sort
    uniqAsc (a : b : rest) | a == (b :: Int) = uniqAsc (b : rest)
                           | otherwise = a : uniqAsc (b : rest)
    uniqAsc xs = xs

    interCount :: U.Vector Int -> U.Vector Int -> Int
    interCount a b = go 0 0 0
      where
        go !acc !i !j
          | i >= U.length a || j >= U.length b = acc
          | av == bv = go (acc + 1) (i + 1) (j + 1)
          | av < bv = go acc (i + 1) j
          | otherwise = go acc i (j + 1)
          where
            av = U.unsafeIndex a i
            bv = U.unsafeIndex b j

    -- two local costs: the DIAGONAL cost prefers full-chord coverage
    -- (or the surface goes flat and the path wanders), while the
    -- HORIZONTAL gap weight uses containment only — a skipped perf
    -- chord wholly inside the current score chord is a roll splinter
    -- and must be nearly free to step over, where skipping an alien
    -- chord costs the full gap
    containRow :: Int -> U.Vector Float
    containRow i =
      let sa = sSets BV.! i
       in U.generate n $ \j ->
            let pa = pSets BV.! j
                inter = interCount sa pa
                mn = min (U.length sa) (U.length pa)
             in double2Float
                  (1.0 - fromIntegral inter / fromIntegral mn)
    costRow :: Int -> U.Vector Float -> U.Vector Float
    costRow i cont =
      let sa = sSets BV.! i
       in U.generate n $ \j ->
            let pa = pSets BV.! j
                inter = interCount sa pa
                mx = max (U.length sa) (U.length pa)
             in double2Float
                  (float2Double (U.unsafeIndex cont j)
                     + 0.25 * (1.0 - fromIntegral inter
                                 / fromIntegral mx))

    neg :: Float
    neg = 1e9

    contains :: BV.Vector (U.Vector Float)
    contains = BV.generate m containRow

    costs :: BV.Vector (U.Vector Float)
    costs = BV.generate m (\i -> costRow i (contains BV.! i))

    dRows :: BV.Vector (U.Vector Float)
    dRows = BV.fromList (go 0 (U.replicate n 0)) -- open begin
      where
        go i prev
          | i >= m = []
          | otherwise =
              let c = costs BV.! i
                  cont = contains BV.! i
                  diag = U.generate n $ \j ->
                    if j == 0
                      then if i == 0 then 0 else neg
                      else U.unsafeIndex prev (j - 1)
                  a = U.zipWith (+) c
                        (U.zipWith min diag (U.map (+ gapScore) prev))
                  step = U.map (\x -> 0.05 + gapPerf * x) cont
                  pref = U.scanl1' (+) step
                  run = U.scanl1' min (U.zipWith (-) a pref)
                  d = U.zipWith (+) run pref
               in d : go (i + 1) d
    dAt i j = U.unsafeIndex (dRows BV.! i) j

    backtrack =
      let j0 = U.minIndex (dRows BV.! (m - 1))
          eps = 1e-4 :: Double
          f2d = float2Double
          walk i j acc
            | i < 0 || j < 0 = acc
            | otherwise =
                let c = U.unsafeIndex (costs BV.! i) j
                    diag = if i > 0 && j > 0
                      then dAt (i - 1) (j - 1)
                      else if i == 0 then 0 else neg
                    -- numpy scalar promotion: f32 + python float
                    -- runs in float64, f32 + f32 stays float32
                    upD = f2d (if i > 0 then dAt (i - 1) j else neg)
                            + 0.6
                    here = dAt i j
                    leftD = if j > 0
                      then Just (f2d (dAt i (j - 1)) + 0.05
                             + 0.35 * f2d (U.unsafeIndex
                                             (contains BV.! i) j))
                      else Nothing
                 in if abs (f2d (here - (c + diag))) < eps
                      then if i == 0 then (i, j) : acc
                           else walk (i - 1) (j - 1) ((i, j) : acc)
                      else if abs (f2d here - (f2d c + upD)) < eps
                        then walk (i - 1) j acc -- score chord skipped
                        else case leftD of
                          Just lv
                            | abs (f2d here - lv) < eps ->
                                walk i (j - 1) acc -- perf chord skipped
                          _ -> -- numeric corner: treat as diagonal
                            if i == 0 then (i, j) : acc
                            else walk (i - 1) (j - 1) ((i, j) : acc)
       in if m == 0 || n == 0 then [] else walk (m - 1) j0 []

-- ---------------------------------------------------------------------
-- note pairing inside the path

rollAbsorbS :: Double
rollAbsorbS = 1.0 -- a rolled chord's splinters within this window

-- | (pairs, deleted, inserted). A rolled chord reaches the DTW as
-- splinters, and the path can only land on ONE of them — the rest are
-- skipped perf chords in the gaps between path steps. So each score
-- chord may also absorb its remaining pitches from the surrounding gap
-- (previous matched chord exclusive to next matched chord exclusive),
-- time-bounded.
pairNotes :: [(Int, [(Int, Int)])] -> [(Double, [Int])]
          -> BV.Vector SmfNote -> [(Int, Int)]
          -> ([Pair], [(Int, Int, Int)], [Int])
pairNotes sc pc notes path = (sortOn key pairs, deleted, inserted)
  where
    scV = BV.fromList sc
    pcV = BV.fromList pc
    key (t, p, c, _) = (t, p, c)
    ext = map snd path <> [BV.length pcV]
    (pairs, matchedPerf, matchedScore) =
      foldl' step ([], IntSet.empty, Set.empty)
        (zip3 path (drop 1 ext) (-1 : map snd path))
    step (ps, mp, ms) ((i, j), jNext, prevJ) =
      let (ticks, spitches) = scV BV.! i
          t0 = fst (pcV BV.! j)
          poolNotes =
            [ x
            | jj <- [prevJ + 1 .. jNext - 1]
            , abs (fst (pcV BV.! jj) - t0) <= rollAbsorbS
            , x <- snd (pcV BV.! jj) ]
          pool = Map.map (sortOn (\x -> abs (snOnS (notes BV.! x)
                                               - t0)))
                   (foldl' (\mm x -> Map.insertWith (flip (<>))
                              (snPitch (notes BV.! x)) [x] mm)
                      Map.empty poolNotes)
          claim (ps', mp', ms') (pitch, ch) =
            case [ x | x <- Map.findWithDefault [] pitch pool
                     , not (IntSet.member x mp') ] of
              (x : _) ->
                ( (ticks, pitch, ch, x) : ps'
                , IntSet.insert x mp'
                , Set.insert (ticks, pitch, ch) ms' )
              [] -> (ps', mp', ms')
       in foldl' claim (ps, mp, ms) spitches
    deleted =
      [ (t, p, ch)
      | (t, ps) <- sc, (p, ch) <- ps
      , not (Set.member (t, p, ch) matchedScore) ]
    inserted =
      [ x | (_, xs) <- pc, x <- xs
          , not (IntSet.member x matchedPerf) ]

-- ---------------------------------------------------------------------
-- beat grid: local-linear time at each quarter-note position

-- | (grid_wn, times) over quarter notes 0..last, monotone.
beatGridTimes :: [Pair] -> BV.Vector SmfNote -> Int
              -> ([Int], Maybe [Double])
beatGridTimes pairs notes lastTick = (grid, times)
  where
    q = wnTicks `div` 4
    grid = takeWhile (< lastTick + q) [0, q ..]
    sorted = sortOn fst
      [ (fromIntegral t :: Double, snOnS (notes BV.! x))
      | (t, _, _, x) <- pairs ]
    xs = U.fromList (map fst sorted)
    ys = U.fromList (map snd sorted)
    fitAt g =
      let sel half = U.filter
            (\k -> abs (U.unsafeIndex xs k - fromIntegral g)
                     <= fromIntegral half)
            (U.enumFromN 0 (U.length xs))
          ptp w = if U.null w then 0
            else U.maximum (U.map (U.unsafeIndex xs) w)
                   - U.minimum (U.map (U.unsafeIndex xs) w)
          -- adaptive context: sparse textures (slow arpeggiated
          -- preludes) need a wider window before the line is
          -- determined
          pickWin (h : hs) =
            let w = sel h
             in if U.length w >= 6 && ptp w > 0 || null hs
                  then w else pickWin hs
          pickWin [] = U.empty
          w = pickWin [2 * q, 4 * q, 8 * q]
       in if U.length w >= 4 && ptp w > 0
            then
              let wx = U.map (U.unsafeIndex xs) w
                  wy = U.map (U.unsafeIndex ys) w
                  nn = fromIntegral (U.length w)
                  mx = U.sum wx / nn
                  my = U.sum wy / nn
                  sxx = U.sum (U.map (\x -> (x - mx) * (x - mx)) wx)
                  sxy = U.sum (U.zipWith
                          (\x y -> (x - mx) * (y - my)) wx wy)
                  k = sxy / sxx
                  b = my - k * mx
               in Just (k * fromIntegral g + b)
            else Nothing
    fitted = [(g, fitAt g) | g <- grid]
    known = [(fromIntegral g, t) | (g, Just t) <- fitted]
    times
      | length known < 2 = Nothing
      | otherwise =
          let kx = U.fromList (map fst known)
              ky = U.fromList (map snd known)
              filled = [ interpTime kx ky (fromIntegral g) | g <- grid ]
           in Just (scanl1 max filled)

-- | Piecewise-linear ys(x) with edge-SLOPE extrapolation: clamping the
-- time outside the annotated span would fabricate huge deviations.
-- xs strictly increasing, length >= 2.
interpTime :: U.Vector Double -> U.Vector Double -> Double -> Double
interpTime xs ys x =
  let i0 = bisectRight xs x
      i = max 1 (min i0 (U.length xs - 1))
      x0 = U.unsafeIndex xs (i - 1)
      x1 = U.unsafeIndex xs i
      y0 = U.unsafeIndex ys (i - 1)
      y1 = U.unsafeIndex ys i
   in y0 + (y1 - y0) * (x - x0) / (x1 - x0)

bisectRight :: U.Vector Double -> Double -> Int
bisectRight v x = go 0 (U.length v)
  where
    go lo hi
      | lo >= hi = lo
      | U.unsafeIndex v mid <= x = go (mid + 1) hi
      | otherwise = go lo mid
      where mid = (lo + hi) `div` 2

-- ---------------------------------------------------------------------
-- recovery pass

-- | Second chance for score notes the chord DTW missed: a trill's
-- alternation hides the parent pitch from exact chord matching, but
-- once the beat map exists the note's TIME is predictable — claim an
-- unused performance note of the same pitch within the tempo-scaled
-- window.
recover :: BV.Vector SmfNote -> [Pair] -> [(Int, Int, Int)] -> [Int]
        -> [Int] -> [Double]
        -> ([Pair], [(Int, Int, Int)], [Int])
recover notes pairs deleted inserted grid times =
  (sortOn (\(t, p, c, _) -> (t, p, c)) (pairs <> newPairs)
  , still, remaining)
  where
    gx = U.fromList (map fromIntegral grid)
    gy = U.fromList times
    q = wnTicks `div` 4
    byPitch = foldl'
      (\m x -> Map.insertWith (flip (<>))
                 (snPitch (notes BV.! x)) [x] m)
      Map.empty inserted
    (newPairs, still, used) = foldl' step ([], [], IntSet.empty) deleted
    step (nps, st, us) (t, p, ch) =
      let pred' = interpTime gx gy (fromIntegral t)
          -- window scales with local tempo: in a lament a beat is a
          -- second and a rolled bass note trails far behind the grid
          qdur = (interpTime gx gy (fromIntegral (t + q))
                    - interpTime gx gy (fromIntegral (max 0 (t - q))))
                   / 2
          win = min 0.6 (max 0.12 (0.5 * qdur))
          cands = [ x | x <- Map.findWithDefault [] p byPitch
                      , not (IntSet.member x us) ]
          best = foldl'
            (\b x ->
               let d = abs (snOnS (notes BV.! x) - pred')
                in if d <= win
                     && maybe True
                          (\bx -> d < abs (snOnS (notes BV.! bx)
                                             - pred')) b
                     then Just x else b)
            Nothing cands
       in case best of
            Just x -> ((t, p, ch, x) : nps, st, IntSet.insert x us)
            Nothing -> (nps, (t, p, ch) : st, us)
    remaining = [x | x <- inserted, not (IntSet.member x used)]

-- ---------------------------------------------------------------------
-- per-(file, piece) alignment

alignOne :: [(Int, [(Int, Int)])] -> BV.Vector SmfNote
         -> Maybe AlignOutcome
alignOne sc notes
  | length sc < 8 || length pc < 8 = Nothing
  | otherwise = Just outcome
  where
    pc = perfChords notes
    path = dtwPath sc pc notes
    (pairs0, deleted0, inserted0) = pairNotes sc pc notes path
    totalScore = sum (map (length . snd) sc)
    lastTick = fst (last sc)
    (grid0, times0) = beatGridTimes pairs0 notes lastTick
    -- second pass rides the improved grid
    refine 0 st = st
    refine k (ps, dels, ins, g, Just ts) =
      let (ps', dels', ins') = recover notes ps dels ins g ts
          (g', ts') = beatGridTimes ps' notes lastTick
       in case ts' of
            Nothing -> (ps', dels', ins', g', Nothing)
            _ -> refine (k - 1 :: Int) (ps', dels', ins', g', ts')
    refine _ st = st
    (pairs, deleted, inserted, grid, timesM) =
      refine 2 (pairs0, deleted0, inserted0, grid0, times0)
    rate = if totalScore == 0 then 0
           else fromIntegral (length pairs) / fromIntegral totalScore
    onsets = [snOnS (notes BV.! x) | (_, _, _, x) <- pairs]
    baseStats = AlignStats
      { stScoreNotes = totalScore
      , stMatched = length pairs
      , stRate = roundD 4 rate
      , stDeleted = length deleted
      , stInserted = length inserted
      , stSpan = if null onsets then Nothing
          else Just (roundD 2 (minimum onsets), roundD 2 (maximum onsets))
      , stMedianDevMs = Nothing
      }
    outcome = case timesM of
      Nothing -> rejected baseStats
      Just times
        | rate < 0.85 -> rejected baseStats
        | otherwise ->
            -- self-check: aligned onsets vs the beat-grid prediction
            let gx = U.fromList (map fromIntegral grid)
                gy = U.fromList times
                devs = [ snOnS (notes BV.! x)
                           - interpTime gx gy (fromIntegral t)
                       | (t, _, _, x) <- pairs ]
                med = medianD devs
                stats = baseStats
                  { stMedianDevMs = Just (roundD 2 (med * 1000)) }
             in if abs med > 0.030
                  then rejected stats
                  else AlignOutcome False pairs deleted inserted grid
                         times sc stats
    rejected st = AlignOutcome True [] [] [] [] [] sc st

medianD :: [Double] -> Double
medianD xs =
  let s = sort xs
      n = length s
   in if odd n
        then s !! (n `div` 2)
        else (s !! (n `div` 2 - 1) + s !! (n `div` 2)) / 2

roundD :: Int -> Double -> Double
roundD d x =
  let m = 10 ^^ d in fromIntegral (round (x * m) :: Integer) / m

-- ---------------------------------------------------------------------
-- python float repr (annotation files must not depend on the emitter's
-- host language)

-- | The shortest-roundtrip decimal, formatted exactly as python's
-- repr: fixed notation while the leading digit's exponent is in
-- [-4, 16), scientific with a signed two-digit exponent outside.
pyRepr :: Double -> String
pyRepr x
  | isNaN x = "nan"
  | isInfinite x = if x < 0 then "-inf" else "inf"
  | x < 0 = '-' : pyRepr (negate x)
  | x == 0 = "0.0"
  | e10 >= -4 && e10 < 16 = fixed
  | otherwise = sci
  where
    (ds, e) = floatToDigits10 x
    e10 = e - 1
    digits = concatMap show ds
    nd = length digits
    fixed
      | e <= 0 = "0." <> replicate (negate e) '0' <> digits
      | e >= nd = digits <> replicate (e - nd) '0' <> ".0"
      | otherwise = take e digits <> "." <> drop e digits
    sci =
      let mant = case digits of
            [d1] -> [d1]
            (d1 : rest) -> d1 : '.' : rest
            [] -> "0"
          esign = if e10 < 0 then "-" else "+"
          eabs = abs e10
          epad = (if eabs < 10 then "0" else "") <> show eabs
       in mant <> "e" <> esign <> epad

floatToDigits10 :: Double -> ([Int], Int)
floatToDigits10 = floatToDigits 10

-- ---------------------------------------------------------------------
-- emission: ASAP's own file formats

spell :: Int -> (String, String, Int)
spell pitch =
  let (letter, acc) = spellTable !! (pitch `mod` 12)
   in (letter, acc, pitch `div` 12 - 1)

-- | The Vienna .match body for one aligned performance.
matchText :: String -> Text -> AlignOutcome -> BV.Vector SmfNote -> Text
matchText piece maestro ao notes =
  T.unlines (header <> body)
  where
    header = map T.pack
      [ "info(matchFileVersion,1.0.0)."
      , "info(piece," <> piece <> ")."
      , "info(midiFileName," <> T.unpack maestro <> ")."
      , "info(performer,maestro)."
      , "info(midiClockUnits,480)."
      , "info(midiClockRate,500000)."
      , "scoreprop(keySignature,C,1:1,0,0.0000)."
      , "scoreprop(timeSignature,4/4,1:1,0,0.0000)."
      ]
    q = wnTicks `div` 4
    snote idx ticks pitch ch =
      let beats = fromIntegral ticks / fromIntegral wnTicks * 4
            :: Double
          (letter, acc, octv) = spell pitch
          (meas, beat) = ticks `divMod` wnTicks
       in "snote(n" <> show idx <> ",[" <> letter <> "," <> acc <> "],"
            <> show octv <> "," <> show (meas + 1) <> ":"
            <> show (beat `div` q + 1) <> ",0,1/4,"
            <> showFFloat (Just 4) beats ","
            <> showFFloat (Just 4) (beats + 1) ",[v" <> show (ch + 1)
            <> ",staff1])"
    noteTimes x =
      let onT = round (snOnS (notes BV.! x) / secPerTick) :: Int
          offT = max (onT + 1)
                   (round (snOffS (notes BV.! x) / secPerTick))
       in (onT, offT)
    numbered = zip [0 :: Int ..]
      (map PairL (sortOn (\(t, p, c, _) -> (t, p, c)) (aoPairs ao))
         <> map DelL (sort (aoDeleted ao))
         <> map InsL (aoInserted ao))
    body =
      [ T.pack $ case ln of
          PairL (ticks, pitch, ch, x) ->
            let (onT, offT) = noteTimes x
             in snote idx ticks pitch ch
                  <> "-note(n" <> show idx <> "," <> show pitch <> ","
                  <> show onT <> "," <> show offT <> ","
                  <> show (snVel (notes BV.! x)) <> ",0,0)."
          DelL (ticks, pitch, ch) ->
            snote idx ticks pitch ch <> "-deletion."
          InsL x ->
            let (onT, offT) = noteTimes x
             in "insertion-note(i" <> show idx <> ","
                  <> show (snPitch (notes BV.! x)) <> "," <> show onT
                  <> "," <> show offT <> ","
                  <> show (snVel (notes BV.! x)) <> ",0,0)."
      | (idx, ln) <- numbered ]

data MatchLine
  = PairL (Int, Int, Int, Int)
  | DelL (Int, Int, Int)
  | InsL Int

-- | 3-column ASAP format; db,4/4 on the uniform quarter grid.
annotationsText :: [Int] -> [Double] -> Text
annotationsText grid times =
  T.unlines
    [ T.pack (pyRepr t <> "\t" <> pyRepr t <> "\t" <> mark)
    | (g, t) <- zip grid times
    , let mark = if g `mod` wnTicks == 0 then "db,4/4" else "b" ]

-- | Provenance sidecar, json.dump-indent-1 compatible.
provJson :: [(String, String)] -> Text
provJson kvs =
  T.pack ("{\n"
    <> intercalate' ",\n" [" \"" <> k <> "\": " <> v | (k, v) <- kvs]
    <> "\n}")
  where
    intercalate' sep = foldr1 (\a b -> a <> sep <> b)

-- ---------------------------------------------------------------------
-- validation against ASAP ground truth

data Verdict = Verdict
  { vAgree :: !Double
  , vBeatMs :: Maybe Double
  , vNBeats :: !Int
  }

-- | Compare our alignment against ASAP's for one performance:
-- their .match text, their beat annotations, their score grid.
validateOne :: AlignOutcome -> BV.Vector SmfNote -> MatchPerf
            -> Text -> Text -> Maybe Verdict
validateOne ao notes mp scoreAnnT perfAnnT = do
  offset <- findOffset
  let corr = correspondences offset
  if length corr < 20
    then pure (Verdict 0.0 Nothing 0)
    else do
      let (slope, inter) = theilSen
            (map (\(k, _, _) -> fromIntegral k) corr)
            (map (\(_, t, _) -> fromIntegral t) corr)
          agree = agreement offset corr
          theirWn = [ fromRational w :: Double
                    | WholeNotes w <- scoreBeatPositions scoreAnnT ]
          theirT = perfBeatTimes perfAnnT
          n = min (length theirWn) (length theirT)
          (wa, wb) = wnToKey (take n theirWn) (take n theirT)
          gx = U.fromList (map fromIntegral (aoGrid ao))
          gy = U.fromList (aoTimes ao)
          deltas =
            [ abs (predT - (t + offset))
            | (w, t) <- zip (take n theirWn) (take n theirT)
            , let ourTick = slope * (wa * w + wb) + inter
                  predT = interpTime gx gy ourTick ]
      pure (Verdict agree
              (Just (roundD 2 (medianD deltas * 1000))) n)
  where
    rows = mpRows mp

    -- ASAP's performance is a CUT of the maestro file: recover the
    -- cut offset against OUR ALIGNED SPAN only — mode of the pairwise
    -- differences at 50 ms resolution, then a median refinement
    -- inside the mode bin.
    byPitch = foldl'
      (\m (_, p, _, x) -> Map.insertWith (flip (<>)) p
                            [snOnS (notes BV.! x)] m)
      Map.empty (aoPairs ao)
    diffs =
      [ d
      | r <- take 300 rows
      , t <- Map.findWithDefault [] (mrPitch r) byPitch
      , let d = t - mrOnS r
      , d > -1.0 ]
    findOffset = do
      _ <- if null diffs then Nothing else Just ()
      let binned = zip (map (\d -> round (d / 0.05) :: Int) diffs)
                     [0 :: Int ..]
          counts = foldl'
            (\m (b, i) -> Map.insertWith
               (\(c1, i1) (c2, i2) -> (c1 + c2, min i1 i2))
               b (1 :: Int, i) m)
            Map.empty binned
          modeBin = fst (Map.foldrWithKey
            (\b (c, i) best@(_, (bc, bi)) ->
               if c > bc || (c == bc && i < bi)
                 then (b, (c, i)) else best)
            (0, (0, maxBound)) counts)
          offs = [ d | d <- diffs
                     , abs (d - fromIntegral modeBin * 0.05) < 0.2 ]
      if null offs then Nothing else Just (medianD offs)

    -- correspondences CONSUME: one of our aligned notes may satisfy
    -- at most one reference row, or a unison loss hides behind its
    -- twin
    oursByPerf0 = foldl'
      (\m (t, p, _, x) ->
         Map.insertWith (flip (<>))
           (p, round (snOnS (notes BV.! x) * 200) :: Int) [t] m)
      Map.empty (aoPairs ao)
    correspondences offset = go oursByPerf0 (zip [0 ..] rows) []
      where
        go _ [] acc = reverse acc
        go m ((ri, r) : rest) acc =
          let t = mrOnS r + offset
              tryKeys =
                [ (mrPitch r, (round (t * 200) :: Int) + d)
                | d <- [-1, 0, 1] ]
              hit = foldl'
                (\found k -> case found of
                   Just _ -> found
                   Nothing -> case Map.lookup k m of
                     Just ts@(_ : _) -> Just (k, ts)
                     _ -> Nothing)
                Nothing tryKeys
           in case hit of
                Just (k, ts) ->
                  go (Map.insert k (init ts) m) rest
                    ((mrKey r, last ts, ri) : acc)
                Nothing -> go m rest acc

    -- Theil-Sen: a handful of wrong correspondences must not bend the
    -- key-space mapping
    theilSen ks ts =
      let len = length ks
          step = max 1 (len `div` 60)
          kv = BV.fromList ks
          tv = BV.fromList ts
          ii = [0, step .. len - 1]
          slopes =
            [ (tv BV.! b - tv BV.! a) / (kv BV.! b - kv BV.! a)
            | a <- ii, b <- ii, b > a, kv BV.! b /= kv BV.! a ]
          slope = if null slopes then 0 / 0 else medianD slopes
          inter = medianD (zipWith (\t k -> t - slope * k) ts ks)
       in (slope, inter)

    -- agree over the COMMON score: their MusicXML expands ornaments
    -- into snotes our notated chords deliberately lack, so their
    -- trill strikes must not count against us — the denominator is
    -- their rows whose (mapped key, pitch) our score also contains,
    -- within an eighth note
    ticksOf = foldl'
      (\m (t, ps) -> foldl'
         (\mm (p, _) -> Map.insertWith (flip (<>)) p [t] mm) m ps)
      Map.empty (aoScore ao)
    agreement _offset corr =
      let hits = IntSet.fromList [ri | (_, _, ri) <- corr]
          (slope, inter) = theilSen
            (map (fromIntegral . (\(k, _, _) -> k)) corr)
            (map (fromIntegral . (\(_, t, _) -> t)) corr)
          q8 = fromIntegral (wnTicks `div` 8) :: Double
          counts = foldl'
            (\(dn, ht) (ri, r) ->
               let myTick = slope * fromIntegral (mrKey r) + inter
                   ok = any (\t -> abs (fromIntegral t - myTick) <= q8)
                          (Map.findWithDefault [] (mrPitch r) ticksOf)
                in if ok
                     then ( dn + 1
                          , ht + if IntSet.member ri hits
                                   then 1 else 0 )
                     else (dn, ht))
            (0 :: Int, 0 :: Int) (zip [0 ..] rows)
       in case counts of
            (0, _) -> 0.0
            (dn, ht) -> fromIntegral ht / fromIntegral dn

    -- their beat timeline and their key space have different origins
    -- (anacrusis pieces put the pickup at NEGATIVE keys while the
    -- annotation clock starts at zero): anchor the two through the
    -- shared PERFORMANCE timeline — a beat's key is the key of their
    -- own matched note sounding at that annotated moment
    wnToKey theirWn theirT =
      let onByTime = sortOn id
            [ (mrOnS r, fromIntegral (mrKey r) :: Double) | r <- rows ]
          ons = U.fromList (map fst onByTime)
          kv = BV.fromList onByTime
          anchor (w, t) =
            let i0 = bisectLeft ons (t - 0.04)
                pickB i best
                  | i >= U.length ons
                      || U.unsafeIndex ons i > t + 0.04 = best
                  | otherwise =
                      let cand = kv BV.! i
                          better = case best of
                            Nothing -> True
                            Just (bt, _) ->
                              abs (fst cand - t) < abs (bt - t)
                       in pickB (i + 1)
                            (if better then Just cand else best)
              in (\r -> (w, snd r)) <$> pickB i0 Nothing
          anchors = [a | Just a <- map anchor (zip theirWn theirT)]
       in if length anchors >= 10
            then
              let aw = map fst anchors
                  ak = map snd anchors
                  (wa, wb) = theilSen aw ak
               in (wa, wb)
            else ( fromIntegral wnTicks
                 , minimum (map (fromIntegral . mrKey) rows) )

bisectLeft :: U.Vector Double -> Double -> Int
bisectLeft v x = go 0 (U.length v)
  where
    go lo hi
      | lo >= hi = lo
      | U.unsafeIndex v mid < x = go (mid + 1) hi
      | otherwise = go lo mid
      where mid = (lo + hi) `div` 2
