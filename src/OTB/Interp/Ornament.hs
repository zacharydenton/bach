-- | The trill solver — ornament realisation per J.S. Bach's own table.
--
-- The authority is the /Explication/ Bach wrote into the Clavier-Büchlein
-- for W.F. Bach (1720), backed by C.P.E. Bach's /Versuch/ (1753) and
-- Neumann's /Ornamentation in Baroque and Post-Baroque Music/:
--
--   * **trill** — begins on the *upper auxiliary, on the beat*, alternates
--     down to the main note and ends on it. Rate tied to tempo (a
--     wall-clock alternation speed, converted through the score tempo),
--     with a floor of four subnotes — the short trill / half-trill of the
--     treatises.
--   * **mordent** — main, lower auxiliary, main: bites and returns.
--   * **inverted mordent** (Pralltriller) — main, upper, main.
--   * **turn** — upper, main, lower, main in equal quarters.
--
-- Auxiliary intervals come from the kern marks themselves (T=whole,
-- t=half — see Token.hs); the turn's auxiliaries default to whole tones,
-- a simplification noted here honestly (13 turns in the corpus).
--
-- Realisation happens on raw lanes *before* articulation and phrasing,
-- so downstream rules see real notes; realised subnotes are slurred
-- (legato within the ornament) except the last, which keeps the parent's
-- remaining marks and its context.
--
-- License: GPL-2.0-or-later.
module OTB.Interp.Ornament
  ( OrnamentParams (..)
  , defaultOrnamentParams
  , realizeLane
  ) where

import OTB.Kern.Token (Mark (..))
import OTB.Score (ScoreNote (..))
import OTB.Units (Bpm (..), WholeNotes (..))

data OrnamentParams = OrnamentParams
  { opTrillRate :: !Double -- ^ alternations per second (subnote rate)
  }
  deriving (Show, Eq)

defaultOrnamentParams :: OrnamentParams
defaultOrnamentParams = OrnamentParams {opTrillRate = 12}

-- | One subnote's duration in whole notes, from the wall-clock rate at
-- this tempo: bpm quarters/min = bpm/4 wholes/min = bpm/240 wholes/sec.
stepWn :: OrnamentParams -> Bpm -> WholeNotes
stepWn op (Bpm bpm) =
  WholeNotes (toRational (bpm / 240 / opTrillRate op))

realizeLane :: OrnamentParams -> Bpm -> [ScoreNote] -> [ScoreNote]
realizeLane op bpm = concatMap realize
  where
    step = stepWn op bpm
    realize sn = case ornamentOf sn of
      Nothing -> [sn]
      Just orn -> expand orn sn {snMarks = filter (not . isOrnament) (snMarks sn)}

    ornamentOf sn =
      case filter isOrnament (snMarks sn) of
        (o : _) -> Just o
        [] -> Nothing

    isOrnament m = case m of
      Trill _ -> True; Mordent _ -> True; InvMordent _ -> True
      Turn -> True; InvTurn -> True
      _ -> False

    -- subdivide sn into (pitch, dur) subnotes: legato marks stripped from
    -- all but the last, which keeps the parent's residual marks
    subdivide sn pairs =
      [ ScoreNote t d p (if lastOne then snMarks sn else [])
      | ((p, d), t, lastOne) <-
          zip3 pairs (scanl (+) (snOnset sn) (map snd pairs))
            (map (const False) (drop 1 pairs) <> [True])
      ]

    expand orn sn =
      let d = snDur sn
          p = snPitch sn
       in case orn of
            Trill aux ->
              -- even subnote count >= 4, upper start => main-note ending
              let n = max 4 (2 * floor (realToFrac (d / step) / 2 :: Double))
                  sub = d / fromIntegral n
                  pitches =
                    [if even i then p + aux else p | i <- [0 .. n - 1]]
               in subdivide sn (map (,sub) pitches)
            Mordent aux ->
              let s = min step (d / 4)
               in subdivide sn [(p, s), (p - aux, s), (p, d - 2 * s)]
            InvMordent aux ->
              let s = min step (d / 4)
               in subdivide sn [(p, s), (p + aux, s), (p, d - 2 * s)]
            Turn ->
              subdivide sn [(p + 2, d / 4), (p, d / 4), (p - 2, d / 4), (p, d / 4)]
            InvTurn ->
              subdivide sn [(p - 2, d / 4), (p, d / 4), (p + 2, d / 4), (p, d / 4)]
            _ -> [sn]
