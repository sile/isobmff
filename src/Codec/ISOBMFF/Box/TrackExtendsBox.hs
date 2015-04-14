module Codec.ISOBMFF.Box.TrackExtendsBox where

--Box Type: trex
--Container: Movie Extends Box (mvex)
--Mandatory: Yes
--Quantity: Exactly one for each track in the Movie Box

import Codec.ISOBMFF
import Data.Word
import Data.Binary.Get
import Data.Binary.Put

data TrackExtendsBox = TrackExtendsBox {
  getVersion :: Word8,
  getFlags :: Word32,
  getTrackID :: Word32,
  getDefaultSampleDescriptionIndex :: Word32,
  getDefaultSampleDuration :: Word32,
  getDefaultSampleSize :: Word32,
  getDefaultSampleFlags :: Word32
  } -- deriving (Show)

instance Show TrackExtendsBox where
  show = getType

instance Box TrackExtendsBox where
  getType _ = "trex"

  makeBox _ = runGet $ do
    (version, flags) <- getFullBoxHeader
    trackID <- getWord32be
    defaultSampleDescriptionIndex <- getWord32be
    defaultSampleDuration <- getWord32be
    defaultSampleSize <- getWord32be
    defaultSampleFlags <- getWord32be
    return TrackExtendsBox {
      getVersion = version,
      getFlags = flags,
      getTrackID = trackID,
      getDefaultSampleDescriptionIndex = defaultSampleDescriptionIndex,
      getDefaultSampleDuration = defaultSampleDuration,
      getDefaultSampleSize = defaultSampleSize,
      getDefaultSampleFlags = defaultSampleFlags
    }

  getBody box = runPut $ do
    putFullBoxHeader (getVersion box) (getFlags box)
    putWord32be (getTrackID box)
    putWord32be (getDefaultSampleDescriptionIndex box)
    putWord32be (getDefaultSampleDuration box)
    putWord32be (getDefaultSampleSize box)
    putWord32be (getDefaultSampleFlags box)
