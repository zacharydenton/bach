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
  | t == "*x" = FInterp IExchange
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
    -- the duration prefix in char-bag order: digits plus the extended
    -- reciprocal's '%' (kern @n%m@ = n/m in reciprocal position, so the
    -- duration is m/n whole notes — e.g. @2%3@ is a triplet breve, 3/2)
    durChars = filter (\c -> isDigit c || c == '%') cs
    (numStr, denStr') = break (== '%') durChars
    denStr = filter isDigit denStr'
    digits = filter isDigit durChars
    dots = length (filter (== '.') cs)
    recip' = if null numStr then 0 else read numStr :: Integer
    -- reciprocal n = 1/n whole notes; each dot multiplies by 3/2.
    -- n == 0 is the kern breve (2 wholes), each extra zero doubles (@00@
    -- longa = 4); grace notes (q) have no duration digits and fall out as
    -- dur 0 — the Player skips them for now.
    base
      | null digits = 0
      | not (null denStr) && recip' > 0 =
          fromIntegral (read denStr :: Integer) / fromIntegral recip'
      | recip' == 0 = 2 ^ length numStr
      | otherwise = 1 / fromIntegral recip'
    dur = WholeNotes (base * (3 / 2) ^^ dots * densityFix)
    -- 'q'/'Q' grace note: force zero duration regardless of digits; the
    -- Grace mark below carries the fact through to the Player's realiser
    densityFix = if isGrace then 0 else 1
    isGrace = 'q' `elem` cs || 'Q' `elem` cs

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
        , [Trill 2 | 'T' `elem` cs]
        , [Trill 1 | 't' `elem` cs]
        , [Mordent 2 | 'M' `elem` cs]
        , [Mordent 1 | 'm' `elem` cs]
        , [InvMordent 2 | 'W' `elem` cs]
        , [InvMordent 1 | 'w' `elem` cs]
        , [Turn | 'S' `elem` cs]
        , [InvTurn | '$' `elem` cs]
        , [GenericOrn | 'O' `elem` cs]
        , [Sforzando | 'z' `elem` cs]
        , [Grace | isGrace]
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
