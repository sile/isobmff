module Codec.ISOBMFF.Box.DataInformationBox where

import Codec.ISOBMFF

data DataInformationBox = DataInformationBox deriving (Show)

instance Box DataInformationBox where
  getType _ = "dinf"
  decodeBody _ = do return DataInformationBox
  encodeBody _ = do return ()
