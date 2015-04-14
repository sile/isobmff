module Codec.ISOBMFF.Box.MovieHeaderBox where

-- Box Type: mvhd
-- Container: Movie Box(moov)
-- Mandatory: Yes
-- Exactly one
-- extends FullBox

import Codec.ISOBMFF
import Data.Word
import Data.Binary.Get
import Data.Binary.Put

data MovieHeaderBox = MovieHeaderBox{
  getVersion          :: Word8,
  getFlags            :: Word32,
  getCreationTime     :: Word64,
  getModificationTime :: Word64,
  getTimescale        :: Word32,
  getDuration         :: Word64
  } deriving (Show)

instance Box MovieHeaderBox where
  getType _ = "mvhd"

  makeBox _ = runGet $ do
    (version, flags) <- getFullBoxHeader
    (creationTime, modificationTime, timescale, duration) <-
      if version == 1
      then do
        c <- getWord64be
        m <- getWord64be
        t <- getWord32be
        d <- getWord64be
        return $ (c, m, t, d)
      else do
        c <- getWord32be
        m <- getWord32be
        t <- getWord32be
        d <- getWord32be
        return $ (fromIntegral c, fromIntegral m, t, fromIntegral d)
    return MovieHeaderBox {
      getVersion = version,
      getFlags = flags,
      getCreationTime = creationTime,
      getModificationTime = modificationTime,
      getTimescale = timescale,
      getDuration = duration
      }

  getBody box = runPut $ do
    putFullBoxHeader (getVersion box) (getFlags box)
    if getVersion box == 1
    then do
      putWord64be (getCreationTime box)
      putWord64be (getModificationTime box)
      putWord32be (getTimescale box)
      putWord64be (getDuration box)
    else do
      putWord32be $ fromIntegral (getCreationTime box)
      putWord32be $ fromIntegral (getModificationTime box)
      putWord32be (getTimescale box)
      putWord32be $ fromIntegral (getDuration box)
