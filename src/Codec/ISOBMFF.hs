{-# LANGUAGE TypeFamilies #-}
module Codec.ISOBMFF where

import Data.ByteString.Lazy as BL
import Data.Binary (Binary, put)
import Data.Binary.Put (runPut)
import Data.Word (Word8, Word32, Word64)

type BoxType = String
type BoxBody = BL.ByteString
type BoxSize = Word64
type BoxVersion = Word8
type BoxFlags = Word32
type Bytes = BL.ByteString

class Box a where
  type Child a

  getType :: a -> BoxType

  getSize :: (Binary a) => a -> BoxSize
  getSize = fromIntegral . BL.length . runPut . put

  decodeBody :: BoxType -> BoxBody -> a
  encodeBody :: a -> BoxBody

  getChildren :: a -> BoxList (Child a)
  getChildren _ = BoxList []

class FullBox a where
  getVersion :: a -> BoxVersion
  getFlags   :: a -> BoxFlags

data BoxList a = BoxList [a] deriving (Show)
