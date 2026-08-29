-- | Field-level lexing of **kern tokens.
--
-- Kern note tokens are order-insensitive bags of characters: @[8.e\\@ and
-- @8.e[\\@ mean the same thing. So the note lexer is a single scan that
-- collects duration digits, dots, a pitch letter-run, accidentals, ties and
-- marks, and *strips* layout channels that carry no sound: stems (@/@ @\\@),
-- beams (@L@ @J@ @K@ @k@), editorial (@x@ @X@ @y@ @Y@ @&@), phrase (@{@ @}@).
--
-- Kern writes every accidental explicitly on the note (the key signature is
-- notational only), so no key-signature logic exists here — or anywhere.
--
-- License: GPL-2.0-or-later.
module OTB.Kern.Lexer
  ( lexRecord
  , lexNoteTok
  ) where

import Data.Char (isDigit)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as TR
import OTB.Kern.Token
import OTB.Units (Bpm (..), WholeNotes (..))

lexRecord :: Int -> Text -> Record
lexRecord n line
  -- a leading "!!" makes the whole line a global comment — it is not
  -- tab-fielded and may legally contain tabs (wtc1f19 line 297)
  | T.isPrefixOf "!!" line = Record n [FComment]
  | otherwise = Record n (map lexField (T.splitOn "\t" line))

lexField :: Text -> Field
lexField t
  | T.null t = FComment -- blank field: treat as inert
  | T.isPrefixOf "!" t = FComment
  | T.isPrefixOf "**kern" t = FInterp IKernStart
  | T.isPrefixOf "**" t = FInterp (IOtherExclusive t)
  | t == "*^" = FInterp ISplit
  | t == "*v" = FInterp IMerge
  | t == "*+" = FInterp IAdd
  | t == "*-" = FInterp ITerminate
  | T.isPrefixOf "*MM" t = FInterp (lexTempo (T.drop 3 t))
  | T.isPrefixOf "*M" t = FInterp (IMeter (T.drop 2 t))
  | T.isPrefixOf "*" t = FInterp ITandem
  | T.isPrefixOf "=" t = FBar t
  | t == "." = FNull
  | otherwise = FData (map lexNoteTok (T.words t))

lexTempo :: Text -> Interp
lexTempo t = case TR.double t of
  Right (v, _) | v > 0 -> ITempo (Bpm v)
  _ -> ITandem -- e.g. "*MM" with verbal tempo text; ignore

-- | One note within a data field.
lexNoteTok :: Text -> NoteTok
lexNoteTok tok = NoteTok dur pit tie marks
  where
    cs = T.unpack tok
    digits = filter isDigit cs
    dots = length (filter (== '.') cs)
    recip' = if null digits then 0 else read digits :: Integer
    -- reciprocal n = 1/n whole notes; each dot multiplies by 3/2.
    -- n == 0 is the kern breve (2 whole notes); grace notes (q) have no
    -- duration digits and fall out as dur 0 — the Player skips them for now.
    base
      | null digits = 0
      | recip' == 0 = 2
      | otherwise = 1 / fromIntegral recip'
    dur = WholeNotes (base * (3 / 2) ^^ dots * densityFix)
    -- 'q'/'Q' grace note: force zero duration regardless of digits
    densityFix = if 'q' `elem` cs || 'Q' `elem` cs then 0 else 1

    letters = filter (`elem` ("abcdefgABCDEFG" :: String)) cs
    isRest = 'r' `elem` cs
    sharps = length (filter (== '#') cs)
    flats = length (filter (== '-') cs)
    pit
      | isRest || null letters = Nothing
      | otherwise = Just (kernPitch letters + sharps - flats)

    tie
      | '[' `elem` cs = TieOpen
      | ']' `elem` cs = TieClose
      | '_' `elem` cs = TieContinue
      | otherwise = TieNone

    marks =
      concat
        [ [Staccato | '\'' `elem` cs]
        , [Tenuto | '~' `elem` cs]
        , [Accent | '^' `elem` cs]
        , [Fermata | ';' `elem` cs]
        , [Trill | any (`elem` ("Tt" :: String)) cs]
        , [Mordent | any (`elem` ("Mm" :: String)) cs]
        , [Turn | any (`elem` ("SW$w" :: String)) cs]
        , [SlurOpen | '(' `elem` cs]
        , [SlurClose | ')' `elem` cs]
        ]

-- | Kern pitch: lowercase ascends from middle C (@c@=C4, @cc@=C5),
-- uppercase descends (@C@=C3, @CC@=C2).
kernPitch :: String -> Int
kernPitch ls@(l : _)
  | l `elem` ("abcdefg" :: String) = 60 + off l + 12 * (length ls - 1)
  | otherwise = 60 + off l - 12 * length ls
  where
    -- semitone offset within the octave, case-insensitive: "c"=60, "C"=48,
    -- "B"=59 (the B just below middle C), "CC"=36.
    off ch = case ch of
      'c' -> 0; 'd' -> 2; 'e' -> 4; 'f' -> 5; 'g' -> 7; 'a' -> 9; 'b' -> 11
      'C' -> 0; 'D' -> 2; 'E' -> 4; 'F' -> 5; 'G' -> 7; 'A' -> 9; 'B' -> 11
      _ -> 0
kernPitch [] = 60
