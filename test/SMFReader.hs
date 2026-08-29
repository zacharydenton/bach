-- | A deliberately minimal Standard MIDI File reader — just enough to
-- cross-examine two writers. Third instance of the project's
-- cross-implementation pattern (bcfwconvert<->syx.rs, oracle.py):
-- Euterpea's ToMidi and our Emit.Midi must describe the same notes.
--
-- License: GPL-2.0-or-later.
module SMFReader
  ( SmfNote (..)
  , readSmf
  ) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString qualified as BS
import Data.Word (Word8)

data SmfNote = SmfNote
  { smfChannel :: !Int
  , smfPitch :: !Int
  , smfVel :: !Int
  , smfOnQ :: !Rational -- ^ onset in quarter notes
  , smfDurQ :: !Rational
  }
  deriving (Eq, Ord, Show)

-- | All notes of a file, onset in quarters (division-normalised).
readSmf :: BS.ByteString -> Either String [SmfNote]
readSmf bs
  | BS.take 4 bs /= BS.pack [0x4D, 0x54, 0x68, 0x64] = Left "no MThd"
  | otherwise =
      let division = fromIntegral (be16 (BS.take 2 (BS.drop 12 bs)))
          tracks = chunks (BS.drop 14 bs)
       in Right (concatMap (trackNotes division) tracks)
  where
    be16 b = (fromIntegral (BS.index b 0) `shiftL` 8)
               .|. fromIntegral (BS.index b 1) :: Int
    be32 b =
      foldl (\acc i -> acc `shiftL` 8 .|. fromIntegral (BS.index b i))
        (0 :: Int) [0 .. 3]
    chunks b
      | BS.length b < 8 = []
      | otherwise =
          let n = be32 (BS.take 4 (BS.drop 4 b))
              body = BS.take n (BS.drop 8 b)
           in body : chunks (BS.drop (8 + n) b)

    trackNotes :: Int -> BS.ByteString -> [SmfNote]
    trackNotes division body = close [] (walk (0 :: Int) Nothing (0 :: Int) [])
      where
        walk pos running t acc
          | pos >= BS.length body = reverse acc
          | otherwise =
              let (dt, pos1) = vlq pos 0
                  t' = t + dt
                  st0 = BS.index body pos1
                  (st, dpos) =
                    if st0 >= 0x80 then (st0, pos1 + 1) else (rq running, pos1)
               in step t' st dpos
          where
            rq (Just r) = r
            rq Nothing = 0
            vlq p acc'
              | BS.index body p >= 0x80 =
                  vlq (p + 1)
                    (acc' `shiftL` 7 .|. fromIntegral (BS.index body p .&. 0x7F))
              | otherwise =
                  (acc' `shiftL` 7 .|. fromIntegral (BS.index body p), p + 1)
            step t' st dpos
              | st == 0xFF =
                  let len = fromIntegral (BS.index body (dpos + 1))
                   in walk (dpos + 2 + len) Nothing t' acc
              | st .&. 0xF0 == 0xF0 = reverse acc -- sysex: not ours, stop
              | otherwise =
                  let n = if st .&. 0xF0 `elem` [0xC0, 0xD0] then 1 else 2
                      ev = (t', st, BS.index body dpos,
                             if n == 2 then BS.index body (dpos + 1) else 0)
                   in walk (dpos + n) (Just st) t' (ev : acc)
        -- pair ons with offs per (channel, pitch), FIFO
        close open evs = case evs of
          [] -> []
          ((t, st, d1, d2) : more)
            | isOn st d2 ->
                close ((chOf st, d1, t, d2) : open) more
            | isOff st d2 ->
                case break (\(c, p, _, _) -> c == chOf st && p == d1)
                       (reverse open) of
                  (before, (c, p, t0, v) : after) ->
                    mk c p t0 t v
                      : close (reverse (before <> after)) more
                  _ -> close open more
            | otherwise -> close open more
          where
            _ = division
        isOn st v = st .&. 0xF0 == 0x90 && v > 0
        isOff st v = st .&. 0xF0 == 0x80 || (st .&. 0xF0 == 0x90 && v == 0)
        chOf st = fromIntegral (st .&. 0x0F) :: Int
        mk c p t0 t1 v =
          SmfNote c (fromIntegral (p :: Word8)) (fromIntegral v)
            (fromIntegral t0 / fromIntegral division)
            (fromIntegral (t1 - t0) / fromIntegral division)
