{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}

-- | The rig, in types. M6's software half: instrument capabilities as
-- class constraints, so a velocity lane aimed at the Model D is a
-- compile error rather than a silent no-op at the take.
--
-- The rig (voice-assignment doctrine, see the Vault doc): the Moog takes
-- CV, everything else takes CC. Six hardware channels:
--
--   ch 0-3  A4 tracks 1-4   velocity+CC+NRPN
--   ch 4    Model D         nothing but notes (its dynamics are CV+hands)
--   ch 5    BS2             velocity+aftertouch+CC
--
-- 'velocityFor' is the only way to read a velocity for emission, and it
-- demands 'HasVelocity'. There is deliberately no such instance for
-- 'ModelD:
--
-- @
--   velocityFor \@'ModelD n   -- rejected: No instance HasVelocity 'ModelD
-- @
--
-- License: GPL-2.0-or-later.
module OTB.Instrument
  ( Target (..)
  , HasVelocity
  , HasCC
  , HasCV
  , velocityFor
  , fixedVelocity
  , hardwareChannel
  , hardwareTracks
  ) where

import Data.List (sortOn)
import Data.Ratio (approxRational)
import OTB.Explain (why)
import OTB.Player (PerfNote (..), Performance (..))
import OTB.Units (WholeNotes (..), toTicks)

data Target = A4Track | ModelD | BS2

-- | Receives velocity meaningfully.
class HasVelocity (t :: Target)
instance HasVelocity 'A4Track
instance HasVelocity 'BS2

-- | Panel addressable over CC/NRPN (the timbre lane's gate).
class HasCC (t :: Target)
instance HasCC 'A4Track
instance HasCC 'BS2

-- | Analog control inputs (the A4 CV track's destination).
class HasCV (t :: Target)
instance HasCV 'ModelD

-- | The only velocity accessor the hardware emitters may use.
velocityFor :: forall t. HasVelocity t => PerfNote -> Int
velocityFor = pnVel

-- | What a velocity-blind instrument gets instead: the fixed stroke.
fixedVelocity :: PerfNote -> Int
fixedVelocity _ = 96

-- | The rig's seat plan, voice-order: four A4 soloists, the Moog, the
-- BS2. (Kern orders voices bass-first, so voice 0 — the bass — lands on
-- A4 track 1, per the casting practice.)
hardwareChannel :: Int -> Either String (Int, String)
hardwareChannel voice = case voice of
  0 -> Right (0, "A4 track 1")
  1 -> Right (1, "A4 track 2")
  2 -> Right (2, "A4 track 3")
  3 -> Right (3, "A4 track 4")
  4 -> Right (4, "Model D")
  5 -> Right (5, "BS2")
  _ -> Left ("hardware rig has 6 seats; voice " <> show voice
               <> " has nowhere to sit")

-- | Arrange a Performance onto the rig: one hardware channel per VOICE
-- (tracks are per voice already), with each instrument's capabilities
-- enforced through the typed accessors. A voice whose sub-spines were
-- polyphonic is reduced to monophony — the decision a human arranger
-- makes when a fugue voice goes to one mono synth — and the number of
-- clipped\/dropped overlaps is reported, never silent. A note whose
-- clipped duration would not survive tick rounding is DROPPED entirely:
-- the SMF writer orders same-tick offs before ons, so a zero-tick note
-- would emit an on with no following release — a stuck hardware note.
-- Provenance is rekeyed to the new (channel, index) identities; dropped
-- notes lose theirs. More voices than seats is an error.
hardwareTracks :: Performance -> Either String (Performance, Int)
hardwareTracks (Performance tmap tracks whys cads) = do
  seated <- sequence
    [ (\(hw, _) -> [((pnChannel n, pnIndex n), remap hw n) | n <- tr])
        <$> hardwareChannel vi
    | (vi, tr) <- zip [0 ..] tracks ]
  let (reduced, counts) = unzip (map reduceTrack seated)
      whys' =
        [ ((pnChannel n, pnIndex n), ws <> agogicWhy n)
        | tr <- reduced, (old, n) <- tr, Just ws <- [lookup old whys] ]
      agogicWhy n =
        [ why "agogic-accent"
            ("duration x" <> show (1 + 0.12 * pnCharge n)
               <> " (velocity-blind channel)")
            "CPE Bach 1753; harpsichord practice"
        | pnChannel n == 4, pnCharge n > 0 ]
  pure (Performance tmap (map (map snd) reduced) whys' cads, sum counts)
  where
    remap hw n =
      let vel = case hw of
            4 -> fixedVelocity n -- Model D: no HasVelocity instance
            5 -> velocityFor @'BS2 n
            _ -> velocityFor @'A4Track n
          -- the harpsichordist's move: where the instrument cannot say
          -- an accent in loudness, say it in time — dissonance charge
          -- becomes a fuller duration on the velocity-blind channel
          -- (C.P.E. Bach 1753 on agogic emphasis); mono-reduction still
          -- clips whatever would collide
          dur = if hw == 4 && pnCharge n > 0
                  then pnDur n
                         * WholeNotes (approxRational
                             (1 + 0.12 * pnCharge n) 1e-6)
                  else pnDur n
       in n {pnChannel = hw, pnVel = vel, pnDur = dur}
    -- mono reduction: within the voice's single channel, a note ends
    -- where its successor begins (keep the moving line, clip the held);
    -- notes whose clip does not survive tick rounding are dropped
    reduceTrack tr =
      let srt = sortOn (pnOnset . snd) tr
          clipped = zipWith clip srt (map Just (drop 1 srt) <> [Nothing])
          survivors = [(k, n) | (k, n, _) <- clipped, audible n]
          nChanged = length [() | (_, n, True) <- clipped, audible n]
          nDropped = length clipped - length survivors
       in (reindex survivors, nChanged + nDropped)
    clip (k, n) (Just (_, nx))
      | pnOnset n + pnDur n > pnOnset nx =
          (k, n {pnDur = max 0 (pnOnset nx - pnOnset n)}, True)
    clip (k, n) _ = (k, n, False)
    audible n = toTicks (pnOnset n) < toTicks (pnOnset n + pnDur n)
    -- lanes collapsing onto one channel would collide on their old lane
    -- indices; renumber within the hardware channel
    reindex tr = [(k, n {pnIndex = j}) | (j, (k, n)) <- zip [0 ..] tr]
