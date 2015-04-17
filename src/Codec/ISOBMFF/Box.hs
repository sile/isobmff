{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Codec.ISOBMFF.Box where

import Codec.ISOBMFF
import Codec.ISOBMFF.BoxUtil as Util
import Data.Binary (Binary, get, put)
import Data.Binary.Put as Put
import Data.Binary.Get as Get

instance (Box a) => Binary a where
  put = Util.putBox2
  get = Util.getBox2

data UnknownBox c = UnknownBox BoxType BoxBody deriving (Show)

instance Box (UnknownBox c) where
  type Child (UnknownBox c) = c
  getType (UnknownBox t _) = t
  encodeBody (UnknownBox _ b) = b
  decodeBody = UnknownBox
