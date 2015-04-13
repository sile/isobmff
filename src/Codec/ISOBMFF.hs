module Codec.ISOBMFF where

import Data.ByteString.Lazy as BL
import Data.ByteString.Char8 as Char8
import Data.Binary.Get
import Data.Binary.Put

type BoxType = String
type Body = BL.ByteString
type Bytes = BL.ByteString

class Box a where
  makeBox :: BoxType -> Body -> a

  getType :: a -> BoxType
  getBody :: a -> Body

  getChildren :: (Box b) => a -> [b]
  getChildren _ = []

class Codec a where
  decodeBox :: (Box b) => a -> Bytes -> (b, Bytes) -- TODO: handle partial
  decodeBox _ input =
    let (box, rest, _) = runGetState decode input 0
    in (box, rest)
    where decode = do
            boxSize <- getWord32be -- TODO: 1, 0
            boxType <- getByteString 4 -- TODO: uuid
            body    <- getLazyByteString (fromIntegral boxSize-8)
            return $ makeBox (Char8.unpack boxType) body

  encodeBox :: (Box b) => a -> b -> Bytes
  encodeBox _ box = runPut $ do
    let boxType = Char8.pack $ getType box
    let boxBody = getBody box
    let boxSize = 4 + 4 + BL.length boxBody
    putWord32be $ fromIntegral boxSize
    putByteString boxType
    putLazyByteString boxBody

decodeAll :: (Codec a) => (Box b) => a -> Bytes -> [b] -- TODO: handle partial
decodeAll codec input =
  if BL.null input
  then []
  else let (box, input') = decodeBox codec input
       in box : decodeAll codec input'

decodeFile :: (Codec a) => (Box b) => a -> FilePath -> IO [b]
decodeFile codec path = do
  input <- BL.readFile path
  return $ decodeAll codec input
