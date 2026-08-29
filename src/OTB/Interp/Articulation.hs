-- | The articulation rule engine — where interpretation starts.
--
-- Baroque keyboard lines speak through note *length*. The rules, in
-- priority order (explicit marks beat inferred context):
--
-- 1. @'@ staccato mark        -> apStaccato
-- 2. @~@ tenuto mark          -> apTenuto
-- 3. inside a slur            -> apLegato
-- 4. immediately repeated note -> min with apRepeated (separation to speak)
-- 5. stepwise motion at singing values (>= eighth) -> apCantabile
-- 6. otherwise                -> apBase (détaché)
--
-- The final note of a lane always sounds full — pieces end, they don't
-- get clipped. Fermata handling is agogics and waits for M4.
--
-- Rules run on chronological *lanes* (monophonic sequences), because slur
-- state and neighbour context only mean something in line order.
--
-- License: GPL-2.0-or-later.
module OTB.Interp.Articulation
  ( articulateLane
  , articulateLane'
  ) where

import OTB.Config (ArtParams (..))
import OTB.Explain (Why, why)
import OTB.Kern.Token (Mark (..))
import OTB.Score (ScoreNote (..))
import OTB.Units (WholeNotes (..))

-- | Chronological lane in, per-note gates out.
articulateLane :: ArtParams -> [ScoreNote] -> [(ScoreNote, Rational)]
articulateLane ap = map (\(n, g, _) -> (n, g)) . articulateLane' ap

-- | The same rules with the fired branch named — the annotator's view.
articulateLane' :: ArtParams -> [ScoreNote] -> [(ScoreNote, Rational, Why)]
articulateLane' ap ns = zipWith3 gateOf ns nexts slurStates
  where
    nexts = map Just (drop 1 ns) <> [Nothing]
    slurStates = drop 1 (scanl slurStep False ns)
    -- a slur opened on a note covers it and everything to the close
    slurStep inSlur n
      | SlurOpen `elem` snMarks n = True
      | SlurClose `elem` snMarks n = False
      | otherwise = inSlur

    gateOf n next inSlur =
      let (g, w) = choose
          g' = max (apMinGate ap) g
       in (n, g', why "articulation" ("gate " <> showR g' <> " (" <> w <> ")")
                    (cite w))
      where
        choose
          | Nothing <- next = (apLegato ap, "lane-final full value")
          | Staccato `elem` snMarks n = (apStaccato ap, "staccato mark")
          | Tenuto `elem` snMarks n = (apTenuto ap, "tenuto mark")
          | inSlur || SlurClose `elem` snMarks n = (apLegato ap, "slur legato")
          | Just nx <- next, snPitch nx == snPitch n =
              (min (apRepeated ap) (fst contextual), "repeated-note separation")
          | otherwise = contextual
        contextual
          | Just nx <- next
          , abs (snPitch nx - snPitch n) <= 2
          , snDur n >= WholeNotes (1 / 8) =
              (apCantabile ap, "stepwise cantabile")
          | otherwise = (apBase ap, "détaché")
        cite w = case w of
          "détaché" -> "CPE Bach 1753, ordinary touch"
          "stepwise cantabile" -> "Quantz XI"
          _ -> ""
    showR r = show (round (fromRational r * 100 :: Double) :: Int) <> "%"
