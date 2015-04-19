module Codec.ISOBMFF.Box.VideoMediaHeaderBox where

import Codec.ISOBMFF
import Control.Applicative
import Data.Binary (put, get)
import Data.Word
import Data.List as List

data VideoMediaHeaderBox = VideoMediaHeaderBox {
  graphicsmode :: Word16,
  opcolor :: (Word16, Word16, Word16)
  } deriving (Show)

instance Box VideoMediaHeaderBox where
  getType _ = "vmhd"
  decodeBody _ = do
    (_, _) <- getFullBoxHeader
    VideoMediaHeaderBox <$> get <*> get
  encodeBody x = do
    putFullBoxHeader x
    put $ graphicsmode x
    put $ opcolor x

instance FullBox VideoMediaHeaderBox where
  getVersion _ = 0
  getFlags _ = 1
