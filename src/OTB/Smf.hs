{-# LANGUAGE MultiWayIf #-}
-- | A minimal Standard MIDI File reader — tempo-aware, no dependencies
-- beyond bytestring. Ports tools/smf.py operation for operation (the
-- seconds integration repeats the reference's float expression order,
-- so onsets agree bitwise): the MAESTRO campaign needs performed
-- note-ons with absolute seconds and velocities out of format-0/1 SMF,
-- nothing else. Validated the same way the Python was: parsing an ASAP
-- performance .mid reproduces the note count, onsets and velocities
-- its Vienna .match records.
--
-- License: GPL-2.0-or-later.
module OTB.Smf
  ( SmfNote (..)
  , readSmf
  , parseSmf
  ) where

import Data.Bits (shiftL, (.&.), (.|.))
import Data.ByteString qualified as BS
import Data.List (foldl', sortOn)
import Data.Map.Strict qualified as Map

data SmfNote = SmfNote
  { snPitch :: !Int
  , snVel :: !Int
  , snOnS :: !Double
  , snOffS :: !Double
  , snCh :: !Int
  , snTrack :: !Int
  } deriving (Show)

readSmf :: FilePath -> IO [SmfNote]
readSmf path = either fail pure . parseSmf =<< BS.readFile path

parseSmf :: BS.ByteString -> Either String [SmfNote]
parseSmf dat
  | BS.take 4 dat /= "MThd" = Left "not an SMF"
  | division .&. 0x8000 /= 0 = Left "SMPTE division unsupported"
  | otherwise = do
      trks <- chunkTracks 14 ntrk
      evs <- concat
        <$> mapM (\(ti, tr) -> map (\(t, k, p) -> (t, ti :: Int, k, p))
                    <$> events ti tr)
              (zip [0 ..] trks)
      pure (integrate division (sortOn (\(t, _, _, _) -> t) evs))
  where
    be n off = foldl' (\a i -> a `shiftL` 8
                         .|. fromIntegral (BS.index dat i))
                 (0 :: Int) [off .. off + n - 1]
    ntrk = be 2 10
    division = be 2 12
    chunkTracks _ 0 = Right []
    chunkTracks i n
      | BS.take 4 (BS.drop i dat) /= "MTrk" = Left "bad track header"
      | otherwise =
          let ln = be 4 (i + 4)
           in (BS.take ln (BS.drop (i + 8) dat) :)
                <$> chunkTracks (i + 8 + ln) (n - 1)

data EvKind = EvOn (Int, Int, Int) | EvOff (Int, Int, Int) | EvTempo

-- | Walk once, integrating the tempo map (default 120 bpm). Open notes
-- queue FIFO per (track, ch, pitch); dangling note-ons close at their
-- own onset.
integrate :: Int -> [(Int, Int, EvKind, Int)] -> [SmfNote]
integrate division evs0 = finish (go evs0 0 0.0 500000 Map.empty [])
  where
    go [] _ _ _ opens acc = (opens, acc)
    go ((ticks, ti, kind, p) : rest) tPrev sPrev us opens acc =
      let s = sPrev + fromIntegral ((ticks - tPrev) * us) / 1e6
                        / fromIntegral division
       in case kind of
            EvTempo -> go rest ticks s p opens acc
            EvOn (ch, pitch, vel) ->
              go rest tPrev sPrev us
                (Map.insertWith (flip (<>)) (ti, ch, pitch)
                   [(s, vel)] opens)
                acc
            EvOff (ch, pitch, _) ->
              case Map.lookup (ti, ch, pitch) opens of
                Just ((onS, vel) : more) ->
                  go rest tPrev sPrev us
                    (Map.insert (ti, ch, pitch) more opens)
                    (SmfNote pitch vel onS s ch ti : acc)
                _ -> go rest tPrev sPrev us opens acc
    finish (opens, acc) =
      sortOn (\n -> (snOnS n, snPitch n))
        (reverse acc
           <> [ SmfNote pitch vel onS onS ch ti
              | ((ti, ch, pitch), stack) <- Map.toList opens
              , (onS, vel) <- stack ])

-- | (abs_ticks, kind, payload) for note-on/off and tempo events, with
-- running status.
events :: Int -> BS.ByteString -> Either String [(Int, EvKind, Int)]
events ti track = go 0 0 0
  where
    len = BS.length track
    byte i = fromIntegral (BS.index track i) :: Int
    varlen i = vgo 0 i
      where
        vgo v j =
          let b = byte j
              v' = v `shiftL` 7 .|. (b .&. 0x7F)
           in if b .&. 0x80 /= 0 then vgo v' (j + 1) else (v', j + 1)
    go i ticks status
      | i >= len = Right []
      | otherwise =
          let (dt, i1) = varlen i
              t = ticks + dt
              b = byte i1
              (st, i2) = if b >= 0x80 then (b, i1 + 1) else (status, i1)
              kind = st .&. 0xF0
              ch = st .&. 0x0F
           in if
                | kind == 0x80 || kind == 0x90 ->
                    let pitch = byte i2
                        vel = byte (i2 + 1)
                        on = kind == 0x90 && vel > 0
                        ev = (if on then EvOn else EvOff)
                               (ch, pitch, vel)
                     in ((t, ev, 0) :) <$> go (i2 + 2) t st
                | kind == 0xA0 || kind == 0xB0 || kind == 0xE0 ->
                    go (i2 + 2) t st
                | kind == 0xC0 || kind == 0xD0 -> go (i2 + 1) t st
                | st == 0xFF ->
                    let meta = byte i2
                        (ln, i3) = varlen (i2 + 1)
                     in if meta == 0x51
                          then let us = foldl'
                                     (\a j -> a `shiftL` 8 .|. byte j)
                                     0 [i3 .. i3 + 2]
                                in ((t, EvTempo, us) :)
                                     <$> go (i3 + ln) t st
                          else go (i3 + ln) t st
                | st == 0xF0 || st == 0xF7 ->
                    let (ln, i3) = varlen i2 in go (i3 + ln) t st
                | otherwise ->
                    Left ("track " <> show ti
                            <> ": unhandled status " <> show st)
