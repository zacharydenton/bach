-- | Tempo giusto: the tempo the notation itself implies.
--
-- Kirnberger (Kunst des reinen Satzes II) and Quantz (Versuch XVII)
-- teach that meter sign and prevailing note values carry the tempo.
-- The FORM here is theirs; the constants are FITTED against the mean
-- tempos of the ASAP human performances (log-linear regression,
-- 2026-09-01: train Book I n=28 r=0.610, held-out Book II n=30
-- r=0.373, mean |log error| 0.352 vs 0.421 for a constant 72 — a
-- real improvement, honestly modest).
--
-- The data overturned two literature-shaped priors: dense sixteenth
-- figuration predicts a FASTER beat, not a slower one (the sixteenths
-- are the motion; the beat still strides), and sustained writing
-- predicts a slower one. Kirnberger's alla breve, though, is
-- emphatically vindicated: a 2-denominator meter is the strongest
-- single predictor (+1.01 in log-space — the beat nearly triples the
-- plain-meter baseline in quarter terms).
--
-- This is a DEFAULT for scores with no @*MM@ (and, via
-- @--tempo giusto@, an override for the encoder's 72-BPM placeholders
-- half of Book II carries), never a silent override of a declared
-- tempo.
--
-- License: GPL-2.0-or-later.
module OTB.TempoGiusto
  ( tempoGiusto
  ) where

import OTB.Score (Score (..), ScoreNote (..), Voice (..))
import OTB.Units (Bpm (..), WholeNotes (..))

-- | Quarter-note BPM implied by meter and note-value distribution.
tempoGiusto :: Score -> Bpm
tempoGiusto s = Bpm (max 40 (min 140 (exp logBpm)))
  where
    -- FITTED (giusto_fit vs ASAP, 2026-09-01); exp(3.9097) ~ 50 is the
    -- plain-4/4 sustained baseline the features push off from
    logBpm =
      3.9097
        + 0.5622 * fast
        - 0.1400 * vfast
        - 0.1456 * broad
        + 0.4538 * den8
        + 1.0105 * den2
        - 0.1167 * compound
    notes = [n | v <- scVoices s, n <- vNotes v, snDur n > 0]
    total = length notes
    frac p
      | total == 0 = 0
      | otherwise =
          fromIntegral (length (filter p notes)) / fromIntegral total
        :: Double
    fast = frac ((<= 1 / 16) . snDur) -- sixteenths and quicker
    vfast = frac ((<= 1 / 32) . snDur) -- thirty-seconds and quicker
    broad = frac ((>= 1 / 2) . snDur) -- halves and longer
    (num, den) = case scMeter s of
      ((_, m) : _) -> m
      [] -> (4, 4)
    den8 = if den >= 8 then 1 else 0
    den2 = if den <= 2 then 1 else 0
    compound = if num > 3 && num `mod` 3 == 0 then 1 else 0
