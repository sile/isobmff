module Codec.ISOBMFF.BoxUtil where

import Codec.ISOBMFF4
import Data.Binary (Binary)
import Data.Binary.Put as Put
import Data.Binary.Get as Get
import Data.ByteString.Lazy as BL
import Data.ByteString.Char8 as Char8

putBox :: BoxType -> BoxBody -> Put
putBox boxType body = do
  putString $ ifF isUuid "uuid" boxType
  putWord32be $ ifF isLarge 1 (fromIntegral boxSize)
  whenM isUuid (putString boxType)
  whenM isLarge (putWord64be $ fromIntegral boxSize)
  putLazyByteString body
  where isUuid   = boxType == "uuid"
        isLarge  = boxSize > 0xFFFFFFFF
        typeSize = if isUuid; then 20; else 4
        bodySize = BL.length body
        boxSize  =
          let size = typeSize + bodySize
          in if size < 0x100000000; then size; else size + 8

getBox :: Get (BoxType, BoxBody)
getBox = do
  startPos <- bytesRead
  boxSize <- getWord32be
  boxType <- getByteString 4
  boxSize <-
    case boxSize of
      1 -> getWord64be
      _ -> return $ fromIntegral boxSize
  boxType <-
    case Char8.unpack boxType of
      "uuid" -> do
        userType <- getByteString 16
        return $ Char8.unpack userType
      boxType -> return boxType
  beforeBodyPos <- bytesRead
  body <-
    case boxSize of
      0 -> getRemainingLazyByteString
      _ -> getLazyByteString $ (fromIntegral boxSize) - (beforeBodyPos - startPos)
  return $ (boxType, body)

putString :: String -> Put
putString = putByteString . Char8.pack

ifF :: Bool -> a -> a -> a
ifF True x _ = x
ifF _    _ y = y

whenM :: (Monad m) => Bool -> m () -> m ()
whenM True x = x
whenM _    _ = return ()
