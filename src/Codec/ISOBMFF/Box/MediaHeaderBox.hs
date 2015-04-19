module Codec.ISOBMFF.Box.MediaHeaderBox where

import Codec.ISOBMFF
import Control.Applicative
import Data.Binary (put, get)
import Data.Word
import Data.List as List

data MediaHeaderBox = MediaHeaderBox {
  creationTime     :: Word64,
  modificationTime :: Word64,
  timescale        :: Word32,
  duration         :: Word64,
  language         :: Word16, -- TODO:
  preDefined       :: Word16
  } deriving (Show)

instance Box MediaHeaderBox where
  getType _ = "mdhd"
  decodeBody _ = do
    (version, _) <- getFullBoxHeader
    let v0 = version == 0
    MediaHeaderBox
      <$> getWord32or64be v0 <*> getWord32or64be v0 <*> get <*> getWord32or64be v0
      <*> get <*> get
  encodeBody x = do
    let v0 = getVersion x == 0
    putFullBoxHeader x
    putWord32or64be v0 $ creationTime x
    putWord32or64be v0 $ modificationTime x
    put $ timescale x
    putWord32or64be v0 $ duration x
    put $ language x
    put $ preDefined x

instance FullBox MediaHeaderBox where
  getVersion x = if List.all (< 0x100000000) [creationTime x, modificationTime x, duration x] then 0 else 1
  getFlags _ = 0
