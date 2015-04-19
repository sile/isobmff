module Codec.ISOBMFF.Box.TrackFragmentHeaderBox where

import Codec.ISOBMFF
import Control.Applicative
import Data.Binary (put, get)
import Data.Word
import Data.List as List

data TrackFragmentHeaderBox = TrackFragmentHeaderBox {
  flags :: Word32,
  trackID :: Word32,
  bytes :: BoxBody -- TODO
  } deriving (Show)

instance Box TrackFragmentHeaderBox where
  getType _ = "tfhd"
  decodeBody _ = do
    (_, flags) <- getFullBoxHeader
    TrackFragmentHeaderBox flags <$> get <*> getRemainingLazyByteString2
  encodeBody x = do
    putFullBoxHeader x
    put $ trackID x
    put $ bytes x

instance FullBox TrackFragmentHeaderBox where
  getVersion _ = 0
  getFlags = flags
