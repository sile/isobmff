module Codec.ISOBMFF.Box.HandlerReferenceBox where

import Codec.ISOBMFF
import Control.Applicative
import Data.Binary (put, get)
import Data.Word
import Data.List as List

data HandlerReferenceBox = HandlerReferenceBox {
  preDefined :: Word32,
  handlerType :: String,
  reserved1 :: (Word32, Word32, Word32),
  name :: String
  } deriving (Show)

instance Box HandlerReferenceBox where
  getType _ = "hdlr"
  decodeBody _ = do
    (_, _) <- getFullBoxHeader
    HandlerReferenceBox
      <$> get <*> getString 4 <*> get <*> getStringNul
  encodeBody x = do
    putFullBoxHeader x
    put $ preDefined x
    put $ handlerType x
    put $ reserved1 x
    putStringNul $ name x

instance FullBox HandlerReferenceBox where
  getVersion _ = 0
  getFlags _ = 0
