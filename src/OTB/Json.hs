-- | A minimal JSON reader/writer for the bake pipeline. Numbers are
-- kept as their raw lexemes so integers stay integers and floats
-- round-trip byte-identically; the writer reproduces python
-- json.dump's formatting (default separators, optional indent,
-- ensure_ascii) so a re-bake of unchanged inputs is a no-op diff.
--
-- License: GPL-2.0-or-later.
module OTB.Json
  ( JValue (..)
  , parseJson
  , dumpJson
  , jStr
  , jNum
  , jLookup
  , jArrOf
  ) where

import Data.Char (isDigit, isHexDigit, ord)
import Data.Text (Text)
import Data.Text qualified as T
import Numeric (showHex)

data JValue
  = JObj [(Text, JValue)]
  | JArr [JValue]
  | JStr Text
  | JNum Text -- ^ raw lexeme
  | JBool Bool
  | JNull
  deriving (Eq, Show)

jStr :: JValue -> Maybe Text
jStr (JStr s) = Just s
jStr _ = Nothing

jNum :: JValue -> Maybe Double
jNum (JNum t) = case reads (T.unpack t) of
  [(x, "")] -> Just x
  _ -> case reads (T.unpack t <> ".0") of -- "3" has no Double read
    [(x, "")] -> Just x
    _ -> Nothing
jNum _ = Nothing

jLookup :: Text -> JValue -> Maybe JValue
jLookup k (JObj kvs) = lookup k kvs
jLookup _ _ = Nothing

jArrOf :: JValue -> [JValue]
jArrOf (JArr xs) = xs
jArrOf _ = []

-- ---------------------------------------------------------------------
-- reading

parseJson :: Text -> Either String JValue
parseJson t0 = do
  (v, rest) <- value (skipWs t0)
  if T.null (skipWs rest)
    then Right v
    else Left ("trailing input: " <> take 40 (T.unpack rest))
  where
    skipWs = T.dropWhile (`elem` (" \t\n\r" :: String))
    value t = case T.uncons t of
      Nothing -> Left "unexpected end"
      Just ('{', r) -> obj (skipWs r) []
      Just ('[', r) -> arr (skipWs r) []
      Just ('"', r) -> do
        (s, r') <- string r
        pure (JStr s, r')
      Just _
        | "true" `T.isPrefixOf` t -> pure (JBool True, T.drop 4 t)
        | "false" `T.isPrefixOf` t -> pure (JBool False, T.drop 5 t)
        | "null" `T.isPrefixOf` t -> pure (JNull, T.drop 4 t)
        | otherwise ->
            let lex' = T.takeWhile
                  (`elem` ("-+.0123456789eE" :: String)) t
             in if T.null lex'
                  then Left ("bad value at: "
                               <> take 40 (T.unpack t))
                  else pure (JNum lex', T.drop (T.length lex') t)
    obj t acc = case T.uncons t of
      Just ('}', r) -> pure (JObj (reverse acc), r)
      Just ('"', r) -> do
        (k, r1) <- string r
        r2 <- expect ':' (skipWs r1)
        (v, r3) <- value (skipWs r2)
        case T.uncons (skipWs r3) of
          Just (',', r4) -> obj (skipWs r4) ((k, v) : acc)
          Just ('}', r4) -> pure (JObj (reverse ((k, v) : acc)), r4)
          _ -> Left "expected , or } in object"
      _ -> Left "expected key or } in object"
    arr t acc = case T.uncons t of
      Just (']', r) -> pure (JArr (reverse acc), r)
      _ -> do
        (v, r1) <- value t
        case T.uncons (skipWs r1) of
          Just (',', r2) -> arr (skipWs r2) (v : acc)
          Just (']', r2) -> pure (JArr (reverse (v : acc)), r2)
          _ -> Left "expected , or ] in array"
    expect c t = case T.uncons t of
      Just (c', r) | c' == c -> Right r
      _ -> Left ("expected " <> [c])
    string = go []
      where
        go parts t =
          let (chunk, rest) = T.break (`elem` ("\"\\" :: String)) t
           in case T.uncons rest of
                Just ('"', r) ->
                  Right (T.concat (reverse (chunk : parts)), r)
                Just ('\\', r) -> case T.uncons r of
                  Just (c, r') -> case c of
                    'u' | T.length r' >= 4
                        , T.all isHexDigit (T.take 4 r') ->
                          let code = hexVal (T.unpack (T.take 4 r'))
                           in go (T.singleton (toEnum code)
                                    : chunk : parts)
                                (T.drop 4 r')
                    _ -> go (T.singleton (unescape c) : chunk : parts)
                           r'
                  Nothing -> Left "dangling escape"
                _ -> Left "unterminated string"
        unescape c = case c of
          'n' -> '\n'; 't' -> '\t'; 'r' -> '\r'; 'b' -> '\b'
          'f' -> '\f'; '/' -> '/'; '\\' -> '\\'; '"' -> '"'
          x -> x
        hexVal = foldl (\a c -> a * 16 + digitVal c) 0
        digitVal c
          | isDigit c = fromEnum c - fromEnum '0'
          | otherwise = 10 + fromEnum (toLowerAscii c) - fromEnum 'a'
        toLowerAscii c = if c >= 'A' && c <= 'F'
          then toEnum (fromEnum c + 32) else c

-- ---------------------------------------------------------------------
-- writing (python json.dump conventions)

-- | @dumpJson Nothing@ = compact with the default separators
-- @(", ", ": ")@; @dumpJson (Just n)@ = @indent=n@.
dumpJson :: Maybe Int -> JValue -> Text
dumpJson mindent v0 = T.pack (go 0 v0 "")
  where
    nl depth = case mindent of
      Nothing -> id
      Just n -> showString ("\n" <> replicate (depth * n) ' ')
    itemSep = case mindent of
      Nothing -> ", "
      Just _ -> ","
    go _ (JStr s) = escString s
    go _ (JNum t) = showString (T.unpack t)
    go _ (JBool True) = showString "true"
    go _ (JBool False) = showString "false"
    go _ JNull = showString "null"
    go _ (JArr []) = showString "[]"
    go d (JArr xs) =
      showString "["
        . joinItems [nl (d + 1) . go (d + 1) x | x <- xs]
        . nl d . showString "]"
    go _ (JObj []) = showString "{}"
    go d (JObj kvs) =
      showString "{"
        . joinItems
            [ nl (d + 1) . escString k . showString ": " . go (d + 1) v
            | (k, v) <- kvs ]
        . nl d . showString "}"
    joinItems [] = id
    joinItems [x] = x
    joinItems (x : xs) = x . showString itemSep . joinItems xs

escString :: Text -> ShowS
escString s = showString "\"" . body . showString "\""
  where
    body = foldr (\c acc -> esc c . acc) id (T.unpack s)
    esc c = case c of
      '"' -> showString "\\\""
      '\\' -> showString "\\\\"
      '\n' -> showString "\\n"
      '\t' -> showString "\\t"
      '\r' -> showString "\\r"
      '\b' -> showString "\\b"
      '\f' -> showString "\\f"
      _ | ord c < 0x20 || ord c > 0x7E -> uEsc (ord c)
        | otherwise -> showString [c]
    uEsc n
      | n > 0xFFFF = -- surrogate pair, as python does
          let n' = n - 0x10000
              hi = 0xD800 + (n' `div` 0x400)
              lo = 0xDC00 + (n' `mod` 0x400)
           in u4 hi . u4 lo
      | otherwise = u4 n
    u4 n =
      let h = showHex n ""
       in showString ("\\u" <> replicate (4 - length h) '0' <> h)
