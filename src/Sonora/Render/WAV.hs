{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
module Sonora.Render.WAV
    ( BitDepth (..)
    , WAVFormat (..)
    , defaultWAVFormat
    , encodeWAV
    , writeWAVFile
    , renderSignalToWAV
    ) where

import Data.Binary.Put
import qualified Data.ByteString.Lazy as BL
import Data.Bits ((.&.))
import Data.Int (Int16, Int32)
import Data.Word (Word16, Word32)
import GHC.Generics (Generic)
import qualified Data.Vector.Storable as V
import Sonora.Signal.Core

-- | Bit resolution for rendered audio.
data BitDepth
    = BitDepth16
    | BitDepth24
    | BitDepth32Float
    deriving (Eq, Show, Read, Generic)

-- | WAV output format specification.
data WAVFormat = WAVFormat
    { wavBitDepth   :: !BitDepth
    , wavConfig     :: !AudioConfig
    } deriving (Eq, Show, Generic)

defaultWAVFormat :: WAVFormat
defaultWAVFormat = WAVFormat
    { wavBitDepth = BitDepth16
    , wavConfig   = defaultConfig
    }

-- | Encode a storable vector of frames into a pure canonical RIFF WAV ByteString.
encodeWAV :: BitDepth -> AudioConfig -> V.Vector Frame -> BL.ByteString
encodeWAV bitDepth cfg frames = runPut putWav
  where
    numChannels = fromIntegral (configChannels cfg) :: Word16
    sampleRate  = fromIntegral (configSampleRate cfg) :: Word32
    numFrames   = fromIntegral (V.length frames) :: Word32

    (audioFormat, bitsPerSample) = case bitDepth of
        BitDepth16      -> (1 :: Word16, 16 :: Word16)  -- 1 = PCM
        BitDepth24      -> (1 :: Word16, 24 :: Word16)  -- 1 = PCM
        BitDepth32Float -> (3 :: Word16, 32 :: Word16)  -- 3 = IEEE Float

    bytesPerSample = bitsPerSample `div` 8
    blockAlign     = numChannels * bytesPerSample
    byteRate       = sampleRate * fromIntegral blockAlign
    subchunk2Size  = numFrames * fromIntegral blockAlign
    chunkSize      = 36 + subchunk2Size

    putWav = do
        -- RIFF Header
        putByteString "RIFF"
        putWord32le chunkSize
        putByteString "WAVE"

        -- "fmt " subchunk
        putByteString "fmt "
        putWord32le 16           -- Subchunk1Size (16 for PCM)
        putWord16le audioFormat  -- AudioFormat
        putWord16le numChannels  -- NumChannels
        putWord32le sampleRate   -- SampleRate
        putWord32le byteRate     -- ByteRate
        putWord16le blockAlign   -- BlockAlign
        putWord16le bitsPerSample

        -- "data" subchunk
        putByteString "data"
        putWord32le subchunk2Size
        putSamples

    putSamples =
        V.forM_ frames $ \(Frame l r) -> do
            let clamp x = max (-1.0) (min 1.0 x)
                clL = clamp l
                clR = clamp r
            case bitDepth of
                BitDepth16 -> do
                    let toI16 s = round (s * 32767.0) :: Int16
                    putInt16le (toI16 clL)
                    putInt16le (toI16 clR)

                BitDepth24 -> do
                    let putInt24le s = do
                            let val = round (s * 8388607.0) :: Int32
                                b0 = fromIntegral (val .&. 0xFF)
                                b1 = fromIntegral ((val `div` 256) .&. 0xFF)
                                b2 = fromIntegral ((val `div` 65536) .&. 0xFF)
                            putWord8 b0
                            putWord8 b1
                            putWord8 b2
                    putInt24le clL
                    putInt24le clR

                BitDepth32Float -> do
                    putFloatle (realToFrac clL)
                    putFloatle (realToFrac clR)

-- | Render a Signal and write it directly to a WAV file on disk.
writeWAVFile :: FilePath -> BitDepth -> AudioConfig -> Signal -> IO ()
writeWAVFile path bitDepth cfg sig = do
    let frames = runSignal sig cfg
        wavBytes = encodeWAV bitDepth cfg frames
    BL.writeFile path wavBytes

-- | Convenience helper to render a signal with default settings.
renderSignalToWAV :: FilePath -> Signal -> IO ()
renderSignalToWAV path sig =
    writeWAVFile path BitDepth16 defaultConfig sig
