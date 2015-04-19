{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Codec.ISOBMFF.Box where

import Codec.ISOBMFF
import Data.Binary.Put as Put
import Data.Binary.Get as Get
import Control.Applicative
import Data.Binary (put, get)
import Data.Word (Word8, Word16, Word32, Word64)
import Data.ByteString.Lazy.Char8 as Char8L
import Data.List as List

data UnknownBox = UnknownBox BoxType BoxBody deriving (Show)

instance Box UnknownBox where
  getType (UnknownBox t _) = t
  decodeBody t = UnknownBox t <$> getRemainingLazyByteString2
  encodeBody (UnknownBox _ b) = do putLazyByteString b

data FileTypeBox = FileTypeBox {
  majorBrand :: String,
  minorVersion :: Word32,
  compatibleBrands :: [String]
  } deriving (Show)

instance Box FileTypeBox where
  getType _ = "ftyp"
  decodeBody _ = FileTypeBox <$> getString 4 <*> getWord32be <*> getCompatibleBrands
    where getCompatibleBrands = do
            bytes <- getRemainingLazyByteString2
            return $ split4 $ Char8L.unpack bytes
          split4 [] = []
          split4 (a:b:c:d:xs) = (a:b:c:[d]) : split4 xs
  encodeBody x = do put $ majorBrand x; put $ minorVersion x; put $ compatibleBrands x

data FreeSpaceBox = FreeSpaceBox BoxType BoxBody deriving (Show)

instance Box FreeSpaceBox where
  getType (FreeSpaceBox t _) = t
  decodeBody t = FreeSpaceBox t <$> getRemainingLazyByteString2
  encodeBody (FreeSpaceBox _ b) = do putLazyByteString b

data MediaDataBox = MediaDataBox BoxBody deriving (Show)

instance Box MediaDataBox where
  getType _ = "mdat"
  decodeBody _ = MediaDataBox <$> getRemainingLazyByteString2
  encodeBody (MediaDataBox b) = do putLazyByteString b

data MovieBox = MovieBox deriving (Show)

instance Box MovieBox where
  getType _ = "moov"
  decodeBody _ = do return MovieBox
  encodeBody _ = do return ()

data MovieHeaderBox = MovieHeaderBox {
  creationTime :: Word64,
  modificationTime :: Word64,
  timescale :: Word32,
  duration :: Word64,
  rate :: Double,
  volume :: Float,
  reserved1 :: Word16,
  reserved2 :: (Word32, Word32),
  matrix :: (Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32),
  preDefined :: (Word32, Word32, Word32, Word32, Word32, Word32),
  nextTrackID :: Word32
  } deriving (Show)

instance Box MovieHeaderBox where
  getType _ = "mvhd"
  decodeBody _ = do
    (version, _) <- getFullBoxHeader
    let v0 = version == 0
    MovieHeaderBox
      <$> getWord32or64be v0 <*> getWord32or64be v0 <*> get <*> getWord32or64be v0
      <*> getFixedPoint32 <*> getFixedPoint16 <*> get <*> get
      <*> get <*> get <*> get
  encodeBody x = do
    let v0 = getVersion x == 0
    putFullBoxHeader x
    putWord32or64be v0 $ creationTime x
    putWord32or64be v0 $ modificationTime x
    put $ timescale x
    putWord32or64be v0 $ duration x
    putFixedPoint32 $ rate x
    putFixedPoint16 $ volume x
    put $ reserved1 x
    put $ reserved2 x
    put $ matrix x
    put $ preDefined x
    put $ nextTrackID x

instance FullBox MovieHeaderBox where
  getVersion x = if List.all (< 0x100000000) [creationTime x, modificationTime x, duration x] then 0 else 1
  getFlags _ = 0

data TrackBox = TrackBox deriving (Show)

instance Box TrackBox where
  getType _ = "trak"
  decodeBody _ = do return TrackBox
  encodeBody _ = do return ()
