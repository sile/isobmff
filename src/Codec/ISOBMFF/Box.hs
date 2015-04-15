module Codec.ISOBMFF.Box where

import Codec.ISOBMFF4
import Codec.ISOBMFF.BoxUtil as Util
import Data.Binary (Binary, get, put)
import Data.Binary.Put as Put
import Data.Binary.Get as Get

data UnknownBox c = UnknownBox BoxType BoxBody deriving (Show)

instance Box UnknownBox where
  getType (UnknownBox t _) = t
  encodeBody (UnknownBox _ b) = b
  decodeBody = UnknownBox

instance Binary (UnknownBox c) where
  put (UnknownBox t b) = Util.putBox t b
  get = do (t, b) <- Util.getBox; return $ UnknownBox t b
