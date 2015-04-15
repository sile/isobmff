{-# LANGUAGE ExistentialQuantification #-}
module Codec.ISOBMFF2 where

class Box2 b where
  getType :: b c -> String

  getChildren2 :: b c -> [c]
  getChildren2 _ = []

data MovieBox2 c = MovieBox2 [c] deriving (Show)

instance Box2 MovieBox2 where
  getType _ = "moov"

  getChildren2 (MovieBox2 cs) = cs

data HogeBox c = HogeBox deriving (Show)

instance Box2 HogeBox where
  getType _ = "hoge"

data StdBox c = Moov (MovieBox2 c)
              | Hoge (HogeBox c)
              deriving (Show)

newtype StdBoxSet = StdBox StdBoxSet

data BoxA c = forall a . Box2 a => BA (a c)

instance Box2 BoxA where
  getType (BA x) = getType x

getBox (Moov x) = BA x
getBox (Hoge x) = BA x

instance Box2 StdBox where
  getType = getType . getBox
--  getType (a x) = getType x

  getChildren2 (Moov x) = getChildren2 x
  getChildren2 _ = []

-- TODO: 差分プログラミング (Box, FullBox)
