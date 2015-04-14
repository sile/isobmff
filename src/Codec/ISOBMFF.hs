module Codec.ISOBMFF where

import Data.ByteString.Lazy as BL
import Data.ByteString.Char8 as Char8
import Data.Binary.Get
import Data.Binary.Put
import Data.Word

type BoxType = String
type Body = BL.ByteString
type Bytes = BL.ByteString

class Box a where
  makeBox :: BoxType -> Body -> a

  getType :: a -> BoxType
  getBody :: a -> Body

--  getChildren :: a -> b
--  getChildren _ = []

--class (Box a) => FullBox a where
--  getVersion :: a -> Word8
--  getFlags   :: a -> Word32

decodeBox :: (Box b) => Bytes -> (b, Bytes) -- TODO: handle partial
decodeBox input =
  let (box, rest, _) = runGetState decode input 0
  in (box, rest)
  where decode = do
          boxSize <- getWord32be -- TODO: 1, 0
          boxType <- getByteString 4 -- TODO: uuid
          body    <- getLazyByteString (fromIntegral boxSize-8)
          return $ makeBox (Char8.unpack boxType) body

encodeBox :: (Box b) => b -> Bytes
encodeBox box = runPut $ do
  let boxType = Char8.pack $ getType box
  let boxBody = getBody box
  let boxSize = 4 + 4 + BL.length boxBody
  putWord32be $ fromIntegral boxSize
  putByteString boxType
  putLazyByteString boxBody

decodeAll :: (Box b) => Bytes -> [b] -- TODO: handle partial
decodeAll input =
  if BL.null input
  then []
  else let (box, input') = decodeBox input
       in box : decodeAll input'

decodeFile ::(Box b) => FilePath -> IO [b]
decodeFile path = do
  input <- BL.readFile path
  return $ decodeAll input

encodeAll :: (Box a) => [a] -> Bytes
encodeAll boxes = runPut $ encode boxes
  where encode []     = do return ()
        encode (b:bs) = do
          putLazyByteString $ encodeBox b
          encode bs

getFlagsInternal :: Get Word32
getFlagsInternal = do
  high <- getWord8
  mid <- getWord8
  low <- getWord8
  return $ makeFlags (fromIntegral high) (fromIntegral mid) (fromIntegral low)

makeFlags :: Word32 -> Word32 -> Word32 -> Word32
makeFlags high mid low = high * 0x10000 + mid * 0x100 + low

getFullBoxHeader :: Get (Word8, Word32)
getFullBoxHeader = do
  version <- getWord8
  flags <- getFlagsInternal
  return $ (version, flags)

putFullBoxHeader :: Word8 -> Word32 -> Put
putFullBoxHeader version flags = do
  putWord8 version
  putWord8 $ fromIntegral flags `div` 0x10000
  putWord8 $ fromIntegral flags `div` 0x100
  putWord8 $ fromIntegral flags
