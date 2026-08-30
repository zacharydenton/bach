-- | The harmony model: key, chord roots, charge, cadences.
--
-- Sources:
--
--   * Krumhansl–Kessler key profiles (Krumhansl, /Cognitive Foundations
--     of Musical Pitch/, 1990): a windowed pitch-class vector correlated
--     against all 24 rotated profiles.
--   * Temperley's preference-rule framing (/Music and Probability/,
--     2007; the Melisma analyzers): key is tracked with a change
--     penalty — a Viterbi pass over the per-bar profile scores — and
--     chord roots score by root-support weights (root, fifth, third,
--     seventh, with the bass note favoured).
--   * Sundberg\/Friberg melodic charge (Friberg 1991, Director Musices):
--     the perceptual weight of a tone by its circle-of-fifths distance
--     from the prevailing chord root. Harmonic charge here is the
--     charge of the chord's tones against the *key* root, root doubled —
--     an honest approximation of DM's table, documented as such.
--   * Cadence: root motion a fifth down onto the tonic, arriving on a
--     barline — the V–I clause the phrasing rules want to know about.
--
-- License: GPL-2.0-or-later.
module OTB.Analysis.Harmony
  ( Harmony (..)
  , analyzeHarmony
  , melodicCharge
  ) where

import Data.List (maximumBy)
import Data.Ord (comparing)
import OTB.Units (WholeNotes (..))

data Harmony = Harmony
  { hKeyAt :: WholeNotes -> Int -- ^ tonic pitch class at a score position
  , hMajorAt :: WholeNotes -> Bool
  , hRootAt :: WholeNotes -> Maybe Int -- ^ chord root pc for the beat
  , hChargeAt :: WholeNotes -> Double -- ^ harmonic charge, 0..~6.5
  , hStabilityAt :: WholeNotes -> Double
    -- ^ 0 = the root just moved, 1 = settled (held three beats or more);
    -- the adaptive temperament's blend factor
  , hCadences :: [WholeNotes] -- ^ onsets of V–I arrivals on barlines
  }

-- | Melodic charge of a pitch class relative to a root pitch class
-- (Friberg 1991, Table 1): distance on the circle of fifths, the flat
-- side weighted heavier. Indexed by (pc - root) mod 12.
melodicCharge :: Int -> Int -> Double
melodicCharge pc root =
  chargeTable !! ((pc - root) `mod` 12)
  where
    chargeTable = [0, 6.5, 2, 4.5, 4, 2.5, 6, 1, 5.5, 3, 3.5, 5]

-- Krumhansl–Kessler probe-tone profiles
kkMajor, kkMinor :: [Double]
kkMajor = [6.35, 2.23, 3.48, 2.33, 4.38, 4.09, 2.52, 5.19, 2.39, 3.66, 2.29, 2.88]
kkMinor = [6.33, 2.68, 3.52, 5.38, 2.60, 3.53, 2.54, 4.75, 3.98, 2.69, 3.34, 3.17]

-- | Analyze once per piece; every accessor is total (fallbacks to the
-- piece-level result when a window is empty).
analyzeHarmony
  :: [(WholeNotes, (Int, Int))] -- ^ meter map
  -> [(WholeNotes, WholeNotes, Int)] -- ^ all sounding notes (on, dur, pitch)
  -> WholeNotes -- ^ piece end
  -> Harmony
analyzeHarmony meter notes end = Harmony
  { hKeyAt = \t -> fst (keyAtT t)
  , hMajorAt = \t -> snd (keyAtT t)
  , hRootAt = rootAtT
  , hChargeAt = chargeAtT
  , hStabilityAt = stabilityAtT
  , hCadences = cadences
  }
  where
    meter' = case meter of
      [] -> [(0, (4, 4))]
      ms -> ms
    wn n d = WholeNotes (fromIntegral n / fromIntegral (d :: Int))

    -- meter segments with their stops; every grid below is laid PER
    -- SEGMENT and re-anchored at each meter change, so a 4/4 -> 3/8
    -- piece never has a beat straddling the barline of the new meter
    segs = zip meter' (map fst (drop 1 meter') <> [max end 1])
    grid stepOf =
      [ (a, min stop (a + sl))
      | ((t, (n, d)), stop) <- segs
      , let sl = stepOf n d
      , sl > 0
      , a <- takeWhile (< stop) (iterate (+ sl) t) ]
    beatGrid = grid (\_ d -> wn 1 d)
    barGrid = grid wn
    keyGrid = grid (\n d -> 2 * wn n d) -- two-bar key windows

    -- index of the grid cell containing t (grids are onset-ascending)
    idxOf g t = max 0 (length (takeWhile ((<= t) . fst) g) - 1)

    -- duration-weighted pitch-class vector for a window
    pcVector a b =
      [ sum [ w | (o, d, p) <- notes
            , p `mod` 12 == pc
            , let lo = max o a; hi = min (o + d) b
            , let WholeNotes wr = max 0 (hi - lo)
            , let w = fromRational wr :: Double
            , w > 0 ]
      | pc <- [0 .. 11] ]

    -- Pearson correlation of a pc vector with a profile rotated to tonic k
    corr v prof k =
      let p = [prof !! ((i - k) `mod` 12) | i <- [0 .. 11]]
          mv = sum v / 12; mp = sum p / 12
          num = sum (zipWith (\x y -> (x - mv) * (y - mp)) v p)
          den = sqrt (sum (map (\x -> (x - mv) ^ (2 :: Int)) v))
                  * sqrt (sum (map (\y -> (y - mp) ^ (2 :: Int)) p))
       in if den <= 0 then 0 else num / den

    -- one key candidate score list per two-bar window (a single bar of
    -- a fugue subject alone is too thin to name a key)
    windowScores =
      [ [ (corr v prof k, (k, isMaj))
        | (prof, isMaj) <- [(kkMajor, True), (kkMinor, False)]
        , k <- [0 .. 11] ]
      | (a, b) <- keyGrid
      , let v = pcVector a b ]

    -- Viterbi over 24 key states with a switch penalty: keys are sticky
    -- (Temperley's change penalty), so a chromatic bar does not flicker
    switchPenalty = 1.0
    keyTrack = case windowScores of
      [] -> [pieceKey]
      (w0 : ws) ->
        let states = map snd w0
            start = [(s, sc, [s]) | (sc, s) <- w0]
            step acc w =
              [ let cands =
                      [ (sc0 + here + (if s0 == s then 0 else -switchPenalty), path)
                      | (s0, sc0, path) <- acc
                      , let here = sum [c | (c, s') <- w, s' == s] ]
                    (best, path) = maximumBy (comparing fst) cands
                 in (s, best, s : path)
              | s <- states ]
            final = maximumBy (comparing (\(_, sc, _) -> sc)) (foldl step start ws)
         in reverse (let (_, _, path) = final in path)
    -- whole-piece K-S drifts a fifth sharp (the dominant is simply
    -- emphasised in tonal music); the corpus supplies the corrective:
    -- Bach ends on the tonic in the bass, so the final bass pitch
    -- class earns its candidate a bonus. Mode still comes from the
    -- profiles (a picardy third must not flip a minor piece major).
    finalBassPc =
      let sounding = [p | (o, d, p) <- notes, o + d >= end - 1 / 16]
       in case sounding of
            [] -> Nothing
            ps -> Just (minimum ps `mod` 12)
    pieceKey =
      let v = pcVector 0 end
       in snd (maximumBy (comparing fst)
                [ (corr v prof k + bassBonus k, (k, isMaj))
                | (prof, isMaj) <- [(kkMajor, True), (kkMinor, False)]
                , k <- [0 .. 11] ])
      where
        bassBonus k = if Just k == finalBassPc then 0.25 else 0
    -- WTC pieces declare their tonic before modulating; the opening
    -- window is too thin for the profiles (a solo subject reads as its
    -- dominant), so it is pinned to the whole-piece key
    keyTrack' = case keyTrack of
      (_ : more) -> pieceKey : more
      [] -> [pieceKey]
    keyAtT t =
      let i = idxOf keyGrid t
       in if i < length keyTrack' then keyTrack' !! i else pieceKey

    -- chord root per beat: Temperley-style root support, bass favoured
    rootOf a b =
      let sounding =
            [ (p, w) | (o, d, p) <- notes
            , let lo = max o a; hi = min (o + d) b
            , let WholeNotes wr = max 0 (hi - lo)
            , let w = fromRational wr :: Double
            , w > 0 ]
       in if null sounding
            then Nothing
            else
              let bass = minimum (map fst sounding)
                  support r =
                    sum [ w * iw ((p - r) `mod` 12) | (p, w) <- sounding ]
                      + (if bass `mod` 12 == r then 0.5 else 0)
                  iw iv = case iv of
                    0 -> 1.0 -- root
                    7 -> 0.5 -- fifth
                    4 -> 0.4; 3 -> 0.4 -- third
                    10 -> 0.25 -- seventh
                    _ -> 0.0
               in Just (fst (maximumBy (comparing snd)
                              [(r, support r) | r <- [0 .. 11]]))
    rootTrack = [rootOf a b | (a, b) <- beatGrid]
    rootAtT t =
      let i = idxOf beatGrid t
       in if i < length rootTrack then rootTrack !! i else Nothing

    -- how many consecutive beats (up to and including this one) the
    -- current root has been held; blend saturates at three
    heldTrack = go 0 Nothing rootTrack
      where
        go _ _ [] = []
        go n prev (r : more) =
          let n' = if r == prev && r /= Nothing then n + 1 else 1
           in n' : go n' r more
    stabilityAtT t =
      let i = idxOf beatGrid t
       in if i < length heldTrack
            then min 1 ((fromIntegral (heldTrack !! i) - 1) / 2)
            else 0

    -- harmonic charge: the prevailing chord root's melodic charge
    -- against the key root — the leading term of DM's harmonic charge
    chargeAtT t = case rootAtT t of
      Nothing -> 0
      Just r ->
        let (key, _) = keyAtT t
         in melodicCharge r key

    -- cadence: beat roots (r1, r2) a falling fifth apart, r2 the tonic,
    -- arriving on a barline
    beatStarts = map fst beatGrid
    cadences =
      [ b2 | ((_, Just r1), (b2, Just r2)) <-
                zip (zip beatStarts rootTrack)
                    (drop 1 (zip beatStarts rootTrack))
      , (r1 - r2) `mod` 12 == 7
      , let (key, _) = keyAtT b2 in r2 == key
      , b2 `elem` map fst barGrid ]
