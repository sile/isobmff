{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Codec.ISOBMFF.StdBox where

import Codec.ISOBMFF
import Codec.ISOBMFF.Box

data StdBox = Unknown (UnknownBox StdBox)
            deriving (Show)

data BoxLike = forall a . (Box a, Child a ~ StdBox) => BoxLike a

getBoxLike :: StdBox -> BoxLike
getBoxLike (Unknown x) = BoxLike x

instance Box BoxLike where
  type Child BoxLike = StdBox
  getType (BoxLike x) = getType x
  getSize (BoxLike x) = getSize x
  decodeBody _ _ = undefined
  encodeBody (BoxLike x) = encodeBody x
  getChildren (BoxLike x) = getChildren x

instance Box StdBox where
  type Child StdBox = StdBox

  getType = getType . getBoxLike
  getSize = getSize . getBoxLike
  getChildren = getChildren . getBoxLike
  encodeBody = encodeBody . getBoxLike
  decodeBody _ _ = undefined
