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
-- t=half — see Token.hs); the turn's auxiliaries, which kern does not
-- state, are refined from the prevailing key by the annotation layer
-- (diatonic neighbours via the harmony model — see Annotate).
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
  , realizeNote
  , realizeGraceLane
  ) where

import Data.Ratio (approxRational)
import OTB.Interp.Express (setDur)
import OTB.Kern.Token (Mark (..))
import OTB.Score (ScoreNote (..))
import OTB.Units (Bpm (..), WholeNotes (..))

data OrnamentParams = OrnamentParams
  { opTrillRate :: !Double -- ^ alternations per second (subnote rate)
  , opTrillAccel :: !Double
    -- ^ each alternation this much faster than the last (C.P.E. Bach:
    -- trills may begin more slowly than they end); 1 = even
  , opTermination :: !Bool
    -- ^ long trills close with a Nachschlag (lower turn into the main
    -- note) per C.P.E. Bach's Versuch, Tab. IV
  , opGraceMs :: !Double
    -- ^ wall-clock length of a realised grace note (the short Vorschlag)
  , opGraceLong :: !Bool
    -- ^ realise a SINGLE grace as the long appoggiatura instead: half
    -- the main note, two thirds of a dotted one (C.P.E. Bach, Versuch
    -- I.2.§11). A per-piece stylistic decision — runs of several graces
    -- (slides, double graces) stay short either way.
  }
  deriving (Show, Eq)

defaultOrnamentParams :: OrnamentParams
defaultOrnamentParams = OrnamentParams
  { opTrillRate = 12
  , opTrillAccel = 1.08
  , opTermination = True
  , opGraceMs = 70
  , opGraceLong = False
  }

-- | One subnote's duration in whole notes, from the wall-clock rate at
-- this tempo: bpm quarters/min = bpm/4 wholes/min = bpm/240 wholes/sec.
stepWn :: OrnamentParams -> Bpm -> WholeNotes
stepWn op (Bpm bpm) =
  WholeNotes (toRational (bpm / 240 / opTrillRate op))

realizeLane :: OrnamentParams -> Bpm -> [ScoreNote] -> [ScoreNote]
realizeLane op bpm = concatMap (realizeNote op bpm)

-- | Grace realisation, C.P.E. Bach's rule (Versuch I.2): graces fall ON
-- the beat and take their time from the note they ornament. A run of
-- graces before a lane successor becomes short on-beat subnotes at the
-- main note's onset; the main note is delayed and shortened by their
-- total, never losing more than half of itself. This is the short
-- Vorschlag only — the long appoggiatura (half the main note, Versuch
-- I.2.§11) is a per-piece stylistic decision awaiting a config knob.
--
-- A grace with no lane successor keeps the wall-clock length: there is
-- nothing to steal from, but the note was written and must speak.
realizeGraceLane :: OrnamentParams -> Bpm -> [ScoreNote] -> [ScoreNote]
realizeGraceLane op (Bpm bpm) ns
  | graceWn <= 0 = filter ((> 0) . snDur) ns -- hostile rate: keep the reals
  | otherwise = go ns
  where
    -- bpm/240 wholes per second (see 'stepWn'), times the grace seconds.
    -- approxRational, not toRational: raw Double rationals carry 2^52-ish
    -- denominators that blow up downstream arithmetic (see leanGate)
    graceWn =
      WholeNotes (approxRational (opGraceMs op / 1000 * bpm / 240) 1e-9)
    isGraceNote n = snDur n <= 0 && Grace `elem` snMarks n
    place gr t g =
      let ms = filter (/= Grace) (snMarks gr)
       in gr {snOnset = t, snDur = g, snMarks = ms, snSegs = [(g, ms)]}
    go xs = case break (not . isGraceNote) xs of
      ([], []) -> []
      ([], y : ys) -> y : go ys
      (gs, y : ys)
        -- a zero-duration successor (cannot occur from the parser) has
        -- nothing to give; drop the graces rather than emit zero widths
        | g <= 0 -> y : go ys
        | otherwise ->
            [ place gr (snOnset y + g * fromIntegral i) g
            | (i, gr) <- zip [0 :: Int ..] gs ]
              <> ((setDur (snDur y - stolen) y) {snOnset = snOnset y + stolen}
                    : go ys)
        where
          k = length gs
          g | opGraceLong op && k == 1 =
                -- the long appoggiatura: half the main note, two thirds
                -- of a dotted one — dottedness is NOTATION (snDots), not
                -- duration: a triplet breve is also 3/2 but is not dotted
                if snDots y >= 1 then snDur y * 2 / 3 else snDur y / 2
            | otherwise = min graceWn (snDur y / fromIntegral (2 * k))
          stolen = g * fromIntegral k
      (gs, []) ->
        [ place gr (snOnset gr + graceWn * fromIntegral i) graceWn
        | (i, gr) <- zip [0 :: Int ..] gs ]

-- | One note: identity when unornamented, subnotes when marked. The
-- interpreter's entry point once ornaments became annotations.
realizeNote :: OrnamentParams -> Bpm -> ScoreNote -> [ScoreNote]
realizeNote op bpm = realize
  where
    step = stepWn op bpm
    realize sn = case ornamentOf sn of
      Nothing -> [sn]
      Just _ | step <= 0 -> [sn] -- unrealisable rate: leave the note plain
      Just orn -> expand orn sn {snMarks = filter (not . isOrnament) (snMarks sn)}

    ornamentOf sn =
      case filter isOrnament (snMarks sn) of
        (o : _) -> Just o
        [] -> Nothing

    isOrnament m = case m of
      Trill _ -> True; Mordent _ -> True; InvMordent _ -> True
      Turn _ _ -> True; InvTurn _ _ -> True
      _ -> False

    -- subdivide sn into (pitch, dur) subnotes: legato marks stripped from
    -- all but the last, which keeps the parent's residual marks. The
    -- parent's spelling survives only on subnotes at the parent's pitch —
    -- auxiliaries are unnotated and must not carry a contradicting one
    subdivide sn pairs =
      [ sn { snOnset = t, snDur = d, snPitch = p
           , snMarks = ms, snSegs = [(d, ms)]
           , snSpell = if p == snPitch sn then snSpell sn else Nothing }
      | ((p, d), t, lastOne) <-
          zip3 pairs (scanl (+) (snOnset sn) (map snd pairs))
            (map (const False) (drop 1 pairs) <> [True])
      , let ms = if lastOne then snMarks sn else []
      ]

    expand orn sn =
      let d = snDur sn
          p = snPitch sn
       in case orn of
            Trill aux ->
              -- even subnote count >= 4, upper start => main-note ending;
              -- durations accelerate (opTrillAccel) so the trill spins up
              -- rather than sewing-machining; long trills close with the
              -- Nachschlag (lower turn) when opTermination. The spin-up is
              -- a fixed window (8 subnotes), not the whole trill: with a
              -- per-subnote exponent the first/last ratio is accel^(n-1),
              -- so a tied whole-note trill opened at a third of nominal
              -- rate and closed in a sub-tick buzz — the steady body must
              -- run at opTrillRate
              let n = max 4 (2 * floor (realToFrac (d / step) / 2 :: Double))
                  accel = max 1 (opTrillAccel op)
                  spinup = min (n - 1) 8
                  weights =
                    [ toRational (accel ** fromIntegral (max 0 (spinup - i)))
                    | i <- [0 .. n - 1] ] :: [Rational]
                  total = sum weights
                  durs = [d * WholeNotes (w / total) | w <- weights]
                  alternation =
                    [if even i then p + aux else p | i <- [0 .. n - 1]]
                  pitches
                    | opTermination op && n >= 8 =
                        take (n - 2) alternation <> [p - 2, p]
                    | otherwise = alternation
               in subdivide sn (zip pitches durs)
            Mordent aux ->
              let s = min step (d / 4)
               in subdivide sn [(p, s), (p - aux, s), (p, d - 2 * s)]
            InvMordent aux ->
              let s = min step (d / 4)
               in subdivide sn [(p, s), (p + aux, s), (p, d - 2 * s)]
            Turn up down ->
              subdivide sn
                [(p + up, d / 4), (p, d / 4), (p - down, d / 4), (p, d / 4)]
            InvTurn up down ->
              subdivide sn
                [(p - down, d / 4), (p, d / 4), (p + up, d / 4), (p, d / 4)]
            _ -> [sn]
