module Codec.ISOBMFF.Box.MediaBox where

import Codec.ISOBMFF

data MediaBox = MediaBox deriving (Show)

instance Box MediaBox where
  getType _ = "mdia"
  decodeBody _ = do return MediaBox
  encodeBody _ = do return ()
