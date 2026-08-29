-- | Hand-rolled Standard MIDI File writer, format 1.
--
-- Euterpea's own export cannot interleave per-channel pitch bend and NRPN,
-- which the temperament (M3) and timbre (M5) lanes require — so the emitter
-- is ours from the start. ~80 lines; the proven reference implementation is
-- the stdlib Python writer in bcrseq/demos/bcrseqlib.py.
--
-- Track 0 carries the tempo meta; track i+1 carries voice i on channel i.
--
-- License: GPL-2.0-or-later.
module OTB.Emit.Midi
  ( writeSmf
  , renderSmf
  ) where

import Data.Bits (shiftR, (.&.), (.|.))
import Data.ByteString qualified as BS
import Data.ByteString.Builder
import Data.ByteString.Lazy qualified as BL
import Data.List (sortOn)
import Data.Word (Word8)
import OTB.Player (PerfNote (..), Performance (..))
import OTB.Units (Bpm (..), Ticks (..), ticksPerQuarter, toTicks)

writeSmf :: FilePath -> Performance -> IO ()
writeSmf fp = BL.writeFile fp . renderSmf

-- | Pure render: the artifact is a function of the Performance alone, so
-- determinism is assertable at the byte level (and a hash names a take).
renderSmf :: Performance -> BL.ByteString
renderSmf (Performance (Bpm bpm) tracks) =
  toLazyByteString $
    header (1 + length tracks) <> tempoTrack bpm <> foldMap voiceTrack tracks

header :: Int -> Builder
header ntrks =
  byteString "MThd"
    <> word32BE 6
    <> word16BE 1 -- format 1
    <> word16BE (fromIntegral ntrks)
    <> word16BE (fromIntegral ticksPerQuarter)

trackChunk :: Builder -> Builder
trackChunk body =
  let bs = BL.toStrict (toLazyByteString (body <> endOfTrack))
   in byteString "MTrk" <> word32BE (fromIntegral (BS.length bs)) <> byteString bs
  where
    endOfTrack = vlq 0 <> word8 0xFF <> word8 0x2F <> word8 0x00

tempoTrack :: Double -> Builder
tempoTrack bpm =
  trackChunk $
    vlq 0 <> word8 0xFF <> word8 0x51 <> word8 0x03 <> word24BE usPerQn
  where
    usPerQn = round (60_000_000 / bpm) :: Int
    word24BE v =
      word8 (fromIntegral (v `shiftR` 16 .&. 0xFF))
        <> word8 (fromIntegral (v `shiftR` 8 .&. 0xFF))
        <> word8 (fromIntegral (v .&. 0xFF))

voiceTrack :: [PerfNote] -> Builder
voiceTrack notes = trackChunk (deltas 0 events)
  where
    events =
      sortOn fst $
        concat
          [ [ (onT, (0x90 .|. ch, p, fromIntegral (pnVel pn)))
            , (offT, (0x80 .|. ch, p, 0))
            ]
          | pn <- notes
          , let Ticks onT = toTicks (pnOnset pn)
                Ticks offT = toTicks (pnOnset pn + pnDur pn)
                ch = fromIntegral (pnChannel pn .&. 0x0F) :: Word8
                p = fromIntegral (pnPitch pn .&. 0x7F) :: Word8
          ]
    deltas _ [] = mempty
    deltas prev ((t, (st, d1, d2)) : rest') =
      vlq (t - prev) <> word8 st <> word8 d1 <> word8 d2 <> deltas t rest'

-- | MIDI variable-length quantity.
vlq :: Int -> Builder
vlq n
  | n < 0 = vlq 0
  | otherwise = go (n `shiftR` 7) (word8 (fromIntegral (n .&. 0x7F)))
  where
    go 0 acc = acc
    go v acc = go (v `shiftR` 7) (word8 (fromIntegral (v .&. 0x7F .|. 0x80)) <> acc)
