module Codec.ISOBMFF.Box.SoundMediaHeaderBox where

import Codec.ISOBMFF
import Control.Applicative
import Data.Binary (put, get)
import Data.Word
import Data.List as List

data SoundMediaHeaderBox = SoundMediaHeaderBox {
  balance :: Float,
  reserved1 :: Word16
  } deriving (Show)

instance Box SoundMediaHeaderBox where
  getType _ = "smhd"
  decodeBody _ = do
    (_, _) <- getFullBoxHeader
    SoundMediaHeaderBox <$> getFixedPoint16 <*> get
  encodeBody x = do
    putFullBoxHeader x
    putFixedPoint16 $ balance x
    put $ reserved1 x

instance FullBox SoundMediaHeaderBox where
  getVersion _ = 0
  getFlags _ = 0
