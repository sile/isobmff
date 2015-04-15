{-# LANGUAGE ExistentialQuantification #-}
module Codec.ISOBMFF3 where

-- TODO: デコードの失敗を扱えるようにする (不正データ、データ不足)

import Data.Tree as T
import Data.Binary (Binary, put, get)
import Data.Binary.Put as Put
import Data.Binary.Get as Get
import Data.ByteString.Lazy as BL
import Data.ByteString.Char8 as Char8
import Data.Word (Word8, Word32)
import Data.Foldable as Foldable
import Data.List as List

type BoxType = String
type BoxBody = BL.ByteString
type BoxVersion = Word8
type BoxFlags = Word32

class (Binary a) => Box a where
  getType :: a -> BoxType
  getBody :: a -> BoxBody
-- TODO: getBox

class (Box a) => FullBox a where
  getVersion :: a -> BoxVersion
  getFlags :: a -> BoxFlags

-- data MovieBox = MovieBox deriving (Show)

-- instance Box MovieBox where
--   getType _ = "moov"

data UnknownBox = UnknownBox BoxType BoxBody deriving (Show)

instance Box UnknownBox where
  getType (UnknownBox t _) = t
  getBody (UnknownBox _ b) = b

instance Binary UnknownBox where
  put (UnknownBox t b) = putBox t b
  get = do (t, b) <- getBox; return $ UnknownBox t b

putBox :: BoxType -> BoxBody -> Put
putBox boxType body = do
  putByteString $ Char8.pack $ if' isUuid "uuid" boxType
  putWord32be $ if' isLarge 1 (fromIntegral boxSize)
  when' isUuid (putByteString $ Char8.pack boxType)
  when' isLarge (putWord64be $ fromIntegral boxSize)
  putLazyByteString body
  where isUuid   = boxType == "uuid"
        isLarge  = boxSize > 0xFFFFFFFF
        typeSize = if isUuid; then 20; else 4
        bodySize = BL.length body
        boxSize  =
          let size = typeSize + bodySize
          in if size < 0x100000000; then size; else size + 8

if' :: Bool -> a -> a -> a
if' True x _ = x
if' _    _ y = y

when' :: (Monad m) => Bool -> m () -> m ()
when' True x = x
when' _    _ = return ()

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

data StdBox = Unknown UnknownBox
            deriving (Show)

data BoxLike = forall a . Box a => BoxLike a

instance Binary BoxLike where
  put (BoxLike x) = put x
  get = error "Unsupported"

instance Box BoxLike where
  getType (BoxLike x) = getType x
  getBody (BoxLike x) = getBody x

getBoxLike :: StdBox -> BoxLike
getBoxLike (Unknown x) = BoxLike x

getStdBox :: BoxType -> Get StdBox
getStdBox _ = do x <- get; return $ Unknown x

instance Binary StdBox where
  put = put . getBoxLike
  get = do (t, _) <- lookAhead getBox; getStdBox t

newtype StdBoxTree = StdBoxTree (T.Tree StdBox) deriving (Show)
newtype StdBoxForest = StdBoxForest (T.Forest StdBox) deriving (Show)

instance Binary StdBoxTree where
  put (StdBoxTree (Node root children)) =
    let rootType = getType $ getBoxLike root
        rootBody = getBody $ getBoxLike root
        childrenBytes = runPut $ put $ StdBoxForest children
    in do putBox rootType $ BL.append rootBody childrenBytes

  get = do
    (t, b) <- lookAhead getBox
    root <- getStdBox t
    let (StdBoxForest children) = runGet get b
    return $ StdBoxTree $ Node root children

instance Binary StdBoxForest where
  put (StdBoxForest ts) = Foldable.mapM_ put ts

  get = do
    ts <- mapWhile isEmpty getTree
    return $ StdBoxForest ts
    where getTree = do (StdBoxTree t) <- get; return t

mapWhile :: (Monad m) => m Bool -> m a -> m [a]
mapWhile pred f = do
  end <- pred
  if end
    then return []
    else do x <- f
            xs <- mapWhile pred f
            return $ x:xs
