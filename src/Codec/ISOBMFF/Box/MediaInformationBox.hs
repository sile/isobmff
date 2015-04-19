module Codec.ISOBMFF.Box.MediaInformationBox where

import Codec.ISOBMFF

data MediaInformationBox = MediaInformationBox deriving (Show)

instance Box MediaInformationBox where
  getType _ = "minf"
  decodeBody _ = do return MediaInformationBox
  encodeBody _ = do return ()
