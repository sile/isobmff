{-# LANGUAGE ExistentialQuantification #-}
module Codec.ISOBMFF3 where

import Data.Tree as T
import Data.Binary (Binary)
import Data.Binary.Put as Put
import Data.Binary.Get as Get
import Data.ByteString.Lazy as BL
import Data.Word (Word8, Word32)

type BoxType = String
type BoxBody = BL.ByteString
type BoxVersion = Word8
type BoxFlags = Word32

class (Binary a) => Box a where
--class Box a where
  getType :: a -> BoxType

class (Box a) => FullBox a where
  getVersion :: a -> BoxVersion
  getFlags :: a -> BoxFlags

-- data MovieBox = MovieBox deriving (Show)

-- instance Box MovieBox where
--   getType _ = "moov"

data UnknownBox = UnknownBox BoxType BoxBody deriving (Show)

instance Box UnknownBox where
  getType (UnknownBox t _) = t

instance Binary UnknownBox where
  put (UnknownBox t b) = do

data StdBox = Unknown UnknownBox
            deriving (Show)

type StdBoxTree = T.Tree StdBox

-----------------------------------------------------------------
