-- | Tempo giusto: the tempo the notation itself implies.
--
-- Kirnberger (Kunst des reinen Satzes II) and Quantz (Versuch XVII)
-- teach that meter sign and prevailing note values carry the tempo: a
-- larger denominator moves lighter and quicker per written value;
-- dense fast figuration asks the beat to yield room; long sustained
-- values let it flow. This module is that doctrine as a small formula —
-- a scholarly DEFAULT for scores with no @*MM@ (and for the encoder's
-- 72-BPM placeholders half of Book II carries), never an override of a
-- declared tempo unless the user forces @--tempo giusto@.
--
-- The constants are literature-shaped, not yet fitted: the honest next
-- step is correlating the predictions against the mean tempos of the
-- 166 ASAP human performances (the residual-mining rig can do it) and
-- committing the result with its statistics, as the agogic knobs were.
--
-- License: GPL-2.0-or-later.
module OTB.TempoGiusto
  ( tempoGiusto
  ) where

import OTB.Score (Score (..), ScoreNote (..), Voice (..))
import OTB.Units (Bpm (..), WholeNotes (..))

-- | Quarter-note BPM implied by meter and note-value distribution.
tempoGiusto :: Score -> Bpm
tempoGiusto s = Bpm (max 40 (min 140 (base * meterF * densityF)))
  where
    base = 76
    notes = [n | v <- scVoices s, n <- vNotes v, snDur n > 0]
    total = length notes
    frac p
      | total == 0 = 0
      | otherwise =
          fromIntegral (length (filter p notes)) / fromIntegral total
        :: Double
    fast = frac ((<= 1 / 16) . snDur) -- sixteenths and quicker
    broad = frac ((>= 1 / 2) . snDur) -- halves and longer
    (num, den) = case scMeter s of
      ((_, m) : _) -> m
      [] -> (4, 4)
    compound = num > 3 && num `mod` 3 == 0
    -- the meter sign's character: eighth-denominator meters move
    -- lightly (compound ones dance), alla breve flows in twos
    meterF
      | den >= 8 = if compound then 1.2 else 1.1
      | den <= 2 = 1.1
      | otherwise = 1.0
    -- fast figuration needs room; sustained writing carries motion
    densityF = max 0.7 (1.1 - 0.5 * fast + 0.1 * broad)
