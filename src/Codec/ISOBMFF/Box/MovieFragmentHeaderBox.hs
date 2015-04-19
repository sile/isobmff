module Codec.ISOBMFF.Box.MovieFragmentHeaderBox where

import Codec.ISOBMFF
import Control.Applicative
import Data.Binary (put, get)
import Data.Word
import Data.List as List

data MovieFragmentHeaderBox = MovieFragmentHeaderBox {
  sequenceNumbder :: Word32
  } deriving (Show)

instance Box MovieFragmentHeaderBox where
  getType _ = "mfhd"
  decodeBody _ = do
    (_, _) <- getFullBoxHeader
    MovieFragmentHeaderBox <$> get
  encodeBody x = do
    putFullBoxHeader x
    put $ sequenceNumbder x

instance FullBox MovieFragmentHeaderBox where
  getVersion _ = 0
  getFlags _ = 0
