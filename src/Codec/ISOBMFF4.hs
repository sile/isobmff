module Codec.ISOBMFF4 where

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

class Box b where
  getType :: b c -> BoxType

  getSize :: (Binary (b c)) => b c -> BoxSize
  getSize = fromIntegral . BL.length . runPut . put

  decodeBody :: BoxType -> BoxBody -> b c
  encodeBody :: b c -> BoxBody

  getChildren :: b c -> BoxList c
  getChildren _ = BoxList []

class FullBox b where
  getVersion :: b c -> BoxVersion
  getFlags   :: b c -> BoxFlags

data BoxList b = BoxList [b] deriving (Show) -- TODO: Foldable, etc (see Data.Tree)
