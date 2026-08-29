-- | Dynamics from the literature — velocity as data, sources named.
--
--   * **Metrical hierarchy** (Sloboda 1983, /The communication of musical
--     metre in piano performance/): players mark the metre with velocity —
--     barlines strongest, mid-bar next, beats next, subdivisions least.
--     Computed from the parsed @*M@ meter; degrades to flat without one.
--   * **Phrase arch** (Todd 1992, /The dynamics of dynamics/): loudness
--     follows the phrase's tempo shape — rise to the middle, fall to the
--     boundary. Phrases come from the same Quantz/LBDM boundaries the
--     breaths use, so the two agree about where lines begin and end.
--   * **High-loud** (KTH rule system): higher pitch, slightly louder —
--     melodic peaks carry.
--   * **Accent marks** (kern @^@): the score's own emphasis, honoured.
--
-- C.P.E. Bach's rule — dissonances louder, consonances softer — is the
-- next rung and needs harmonic context the compiler doesn't model yet;
-- deferred, not forgotten.
--
-- All contributions are in velocity units around a base, clamped to
-- [1, 127]. The hardware note: this lane lands on the A4 and the BS2;
-- the Model D ignores velocity entirely (its dynamics are CV and hands).
--
-- License: GPL-2.0-or-later.
module OTB.Interp.Dynamics
  ( DynParams (..)
  , defaultDynParams
  , dynamicsLane
  ) where

import OTB.Kern.Token (Mark (..))
import OTB.Score (ScoreNote (..))
import OTB.Units (WholeNotes (..))

data DynParams = DynParams
  { dyBase :: !Double -- ^ centre velocity
  , dyBar :: !Double -- ^ bump at the barline
  , dyHalfBar :: !Double -- ^ bump at mid-bar (even meters)
  , dyBeat :: !Double -- ^ bump on other beats
  , dyArch :: !Double -- ^ peak of the Todd phrase arch
  , dyHighLoud :: !Double -- ^ per semitone above middle C's octave centre
  , dyAccent :: !Double -- ^ kern @^@
  }
  deriving (Show, Eq)

defaultDynParams :: DynParams
defaultDynParams = DynParams
  { dyBase = 84
  , dyBar = 12
  , dyHalfBar = 6
  , dyBeat = 3
  , dyArch = 8
  , dyHighLoud = 0.25
  , dyAccent = 14
  }

-- | Velocities for a chronological (post-ornament) lane. Boundary flags
-- come from the phrasing detector so arches and breaths agree.
dynamicsLane :: DynParams -> Maybe (Int, Int) -> [Bool] -> [ScoreNote] -> [Int]
dynamicsLane dp meter bounds ns =
  zipWith3 vel ns (archPositions bounds ns) ns
  where
    vel n x _ =
      clamp . round $
        dyBase dp
          + metrical n
          + dyArch dp * 4 * x * (1 - x)
          + dyHighLoud dp * fromIntegral (snPitch n - 66)
          + (if Accent `elem` snMarks n then dyAccent dp else 0)
    clamp = max 1 . min 127

    metrical n = case meter of
      Nothing -> 0
      Just (num, den) ->
        let bar = WholeNotes (fromIntegral num / fromIntegral den)
            beat = WholeNotes (1 / fromIntegral den)
            pos = wmod (snOnset n) bar
         in if pos == 0 then dyBar dp
            else if even num && pos == bar / 2 then dyHalfBar dp
            else if wmod pos beat == 0 then dyBeat dp
            else 0
    wmod (WholeNotes a) (WholeNotes b)
      | b <= 0 = WholeNotes a
      | otherwise = WholeNotes (a - b * fromIntegral (floor (a / b) :: Integer))

-- | Position of each note within its phrase, 0..1: phrases are the spans
-- between boundary flags (a flag on note i ends the phrase at i).
archPositions :: [Bool] -> [ScoreNote] -> [Double]
archPositions bounds ns = concatMap spread (segments (zip bounds ns))
  where
    segments [] = []
    segments xs =
      let (seg, rest') = breakAfter fst xs
       in map snd seg : segments rest'
    breakAfter p xs = case span (not . p) xs of
      (a, b : rest') -> (a <> [b], rest')
      (a, []) -> (a, [])
    spread seg =
      let k = length seg
       in [ if k <= 1 then 0.5 else fromIntegral i / fromIntegral (k - 1)
          | i <- [0 .. k - 1]
          ]
