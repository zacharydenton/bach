-- | Temperament: the quietly radical feature.
--
-- A tuning table is twelve cent-offsets from equal temperament, one per
-- pitch class (smart-constructed — the twelve-ness is enforced, the
-- vector-sized dependency is not worth twelve elements). One table serves
-- two outputs:
--
--   * per-note **pitch bend** in the MIDI artifact — for the hardware,
--     which reads no tuning files. Bend is per *channel*, which is why
--     the emitter gives every monophonic lane its own channel.
--   * a rendered **.scl file** — for Surge, which does microtuning
--     natively and serves as ground truth. Same table, two carriers;
--     the round-trip (table -> .scl -> table) is asserted in tests.
--
-- Werckmeister III ships built in: the well-temperament the corpus was
-- written to demonstrate. Carlos re-recorded for want of this; we get it
-- from a lookup table.
--
-- License: GPL-2.0-or-later.
module OTB.Tuning
  ( TuningTable
  , mkTuningTable
  , offsetFor
  , equalTable
  , werckmeister3
  , bendValue
  , justOffsetFor
  , adaptiveCents
  , parseScl
  , renderScl
  ) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as TR
import OTB.Units (Cents (..))

-- | Offsets from 12TET per pitch class, C first. Opaque; always length 12.
newtype TuningTable = TuningTable [Cents]
  deriving (Eq, Show)

mkTuningTable :: [Cents] -> Either String TuningTable
mkTuningTable cs
  | length cs == 12 = Right (TuningTable cs)
  | otherwise = Left ("tuning table needs 12 offsets, got " <> show (length cs))

offsetFor :: TuningTable -> Int -> Cents
offsetFor (TuningTable cs) pitch = cs !! (pitch `mod` 12)

equalTable :: TuningTable
equalTable = TuningTable (replicate 12 0)

-- | Werckmeister III, C-based. Scale degrees in cents from C:
-- 0, 90.225, 192.18, 294.135, 390.225, 498.045, 588.27, 696.09, 792.18,
-- 888.27, 996.09, 1092.18 — stored as deviation from equal temperament.
werckmeister3 :: TuningTable
werckmeister3 =
  TuningTable
    (zipWith (\w e -> Cents (w - e)) wIII [0, 100 ..])
  where
    wIII =
      [ 0, 90.225, 192.18, 294.135, 390.225, 498.045
      , 588.27, 696.09, 792.18, 888.27, 996.09, 1092.18
      ]

-- | 5-limit just-intonation deviation from ET for an interval above a
-- root, in cents (Duffin, /How Equal Temperament Ruined Harmony/, for
-- the argument; the ratios are the classical ones — 5/4 thirds, 3/2
-- fifths). The tritone is left tempered.
justOffsetFor :: Int -> Cents
justOffsetFor iv =
  Cents ([ 0, 11.7, 3.9, 15.6, -13.7, -2.0
         , 0, 2.0, 13.7, -15.64, 17.6, -11.7 ] !! (iv `mod` 12))

-- | The adaptive policy's arithmetic: blend a note's tempered offset
-- toward "just above the tempered root" by s in [0,1]. At s=0 this is
-- Werckmeister exactly; at s=1 the chord is a just sonority anchored on
-- the root's Werckmeister position — the temperament breathes with the
-- harmonic rhythm.
adaptiveCents :: TuningTable -> Int -> Double -> Int -> Cents
adaptiveCents table root s pitch =
  let Cents w = offsetFor table pitch
      Cents wr = offsetFor table root
      Cents j = justOffsetFor ((pitch - root) `mod` 12)
      s' = max 0 (min 1 s)
   in Cents ((1 - s') * w + s' * (wr + j))

-- | 14-bit pitch-bend value for a cent offset, given the receiver's bend
-- range in semitones (±). Center 8192; clamped to the legal range.
bendValue :: Double -> Cents -> Int
bendValue rangeSemis (Cents c) =
  max 0 (min 16383 (8192 + round (c / (rangeSemis * 100) * 8192)))

-- | Scala .scl subset: '!' comments, description line, count line, then one
-- interval per line — cents ("90.225") or ratio ("256/243") — for degrees
-- 1..n where the last is the octave. Only 12-tone octave scales are
-- accepted; degree k maps to pitch class k, tonic gets 0.
parseScl :: Text -> Either String TuningTable
parseScl src = do
  let content = filter (not . T.isPrefixOf "!") (map T.strip (T.lines src))
  case content of
    (_desc : countLine : rest) -> do
      n <- case TR.decimal countLine of
        Right (v, rest') | T.null (T.strip rest') -> Right (v :: Int)
        _ -> Left ("bad note count: " <> T.unpack countLine)
      if n /= 12
        then Left ("only 12-tone scales supported, got " <> show n)
        else do
          vals <- traverse parseInterval (take 12 (filter (not . T.null) rest))
          case vals of
            degrees
              | length degrees /= 12 -> Left "fewer than 12 interval lines"
              -- the last degree is the period; it folds to pitch class 0,
              -- which only works if it is an octave
              | abs (last degrees - 1200) > 0.01 ->
                  Left ("non-octave scale: period is "
                          <> show (last degrees) <> " cents, not 1200")
              | otherwise ->
                  let pcCents = 0 : take 11 degrees
                   in mkTuningTable
                        (zipWith (\c e -> Cents (c - e)) pcCents [0, 100 ..])
    _ -> Left "truncated .scl"
  where
    parseInterval t
      | T.any (== '/') t
      , (a, b) <- T.breakOn "/" t
      , Right (num, "") <- TR.decimal (T.strip a)
      , Right (den, "") <- TR.decimal (T.strip (T.drop 1 b))
      , (den :: Integer) > 0, (num :: Integer) > 0 =
          Right (1200 * logBase 2 (fromIntegral num / fromIntegral den))
      -- Scala spec: a value without a period is a ratio ("2" means 2/1);
      -- cents always carry one
      | not (T.any (== '.') t)
      , Right (num, "") <- TR.decimal (T.strip t)
      , (num :: Integer) > 0 =
          Right (1200 * logBase 2 (fromIntegral num))
      | otherwise = case TR.double t of
          Right (v, rest') | T.null (T.strip rest'), not (isNaN v) -> Right v
          _ -> Left ("bad interval: " <> T.unpack t)

-- | Render for Surge (and anything else Scala-literate).
renderScl :: Text -> TuningTable -> Text
renderScl name (TuningTable cs) =
  T.unlines $
    [ "! " <> name <> ".scl"
    , "! generated by otb — offsets are exact table values"
    , name
    , "12"
    , "!"
    ]
      <> [ T.pack (show (fromIntegral e + c))
         | (Cents c, e) <- drop 1 (zip cs [0 :: Int, 100 ..])
         ]
      <> ["1200.0"]
