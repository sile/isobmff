module Codec.ISOBMFF.Box.TrackHeaderBox where

import Codec.ISOBMFF
import Control.Applicative
import Data.Binary (put, get)
import Data.Word
import Data.Int
import Data.List as List

data TrackHeaderBox = TrackHeaderBox {
  flags            :: Word32,
  creationTime     :: Word64,
  modificationTime :: Word64,
  trackID          :: Word32,
  reserved1        :: Word32,
  duration         :: Word64,
  reserved2        :: (Word32, Word32),
  layer            :: Int16,
  alternateGroup   :: Int16,
  volume           :: Int16,
  reserved3        :: Word16,
  matrix           :: (Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32),
  width            :: Word32,
  height           :: Word32
  } deriving (Show)

instance Box TrackHeaderBox where
  getType _ = "tkhd"
  decodeBody _ = do
    (version, flags) <- getFullBoxHeader
    let v0 = version == 0
    TrackHeaderBox flags
      <$> getWord32or64be v0 <*> getWord32or64be v0 <*> get <*> get <*> getWord32or64be v0
      <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get
  encodeBody x = do
    let v0 = getVersion x == 0
    putFullBoxHeader x
    putWord32or64be v0 $ creationTime x
    putWord32or64be v0 $ modificationTime x
    put $ trackID x
    put $ reserved1 x
    putWord32or64be v0 $ duration x
    put $ reserved2 x
    put $ layer x
    put $ alternateGroup x
    put $ volume x
    put $ reserved3 x
    put $ matrix x
    put $ width x
    put $ height x

instance FullBox TrackHeaderBox where
  getVersion x = if List.all (< 0x100000000) [creationTime x, modificationTime x, duration x] then 0 else 1
  getFlags = flags
