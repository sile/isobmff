{-# LANGUAGE TypeFamilies #-}
module Codec.ISOBMFF where

import Data.ByteString.Lazy as BL
import Data.Binary (Binary, put, get)
import Data.Binary.Put as Put
import Data.Binary.Get as Get
import Data.Word (Word8, Word32, Word64)
import Data.Tree (Tree)
import Data.Tree as T
import Data.ByteString.Char8 as Char8
import Data.ByteString.Lazy.Char8 as Char8L
import Data.Foldable (Foldable, foldMap)
import Data.Foldable as F
import Data.Functor
import Data.Monoid
import Data.List as List
import Control.Applicative

type BoxType = String
type BoxBody = BL.ByteString
type BoxSize = Word64
type BoxVersion = Word8
type BoxFlags = Word32
type Bytes = BL.ByteString

class Box a where
  getType :: a -> BoxType
-- TODO: hasChild
  decodeBody :: BoxType -> Get a
  encodeBody :: a -> Put

class FullBox a where
  getVersion :: a -> BoxVersion
  getFlags   :: a -> BoxFlags

data BoxRoot a = BoxRoot [Tree a] deriving (Show)

boxForest :: BoxRoot a -> [Tree a]
boxForest (BoxRoot ts) = ts

instance Foldable BoxRoot where
  foldMap f (BoxRoot ts) = List.foldl (\acc t -> acc `mappend` foldMap f t) mempty ts

instance Functor BoxRoot where
  fmap f (BoxRoot ts) = BoxRoot $ [fmap f t | t <- ts]

-- TODO: Traversable, etc

instance (Box a) => Binary (BoxRoot a) where
  get = do ts <- getBoxForest; return $ BoxRoot ts
  put (BoxRoot ts) = putBoxForest ts


getBoxTree :: (Box a) => Get (Tree a)
getBoxTree = do
  (boxType, body) <- getBoxImpl
  let tree = Get.runGet (getTree boxType) body
  return tree
  where getTree boxType = do
          box <- decodeBody boxType
          children <- getBoxForest
          return $ T.Node box children

getBoxForest :: (Box a) => Get [Tree a]
getBoxForest = do
  e <- isEmpty
  if e
    then return []
    else do t  <- getBoxTree
            ts <- getBoxForest
            return $ t:ts

putBoxForest :: (Box a) => [Tree a] -> Put
putBoxForest = F.mapM_ putBoxTree

putBoxTree :: (Box a) => Tree a -> Put
putBoxTree (T.Node b ts) = do
  let payload = runPut $ do
        encodeBody b
        putBoxForest ts
  putBoxImpl (getType b) payload

putBoxImpl :: BoxType -> BoxBody -> Put
putBoxImpl boxType body = do
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

ifF :: Bool -> a -> a -> a
ifF True x _ = x
ifF _    _ y = y

whenM :: (Monad m) => Bool -> m () -> m ()
whenM True x = x
whenM _    _ = return ()

putString :: String -> Put
putString = putByteString . Char8.pack

getString :: Int -> Get String
getString n = do s <- getByteString n; return $ Char8.unpack s

getRemainingLazyByteString2 :: Get BL.ByteString
getRemainingLazyByteString2 = do
  s <- remaining
  getLazyByteString s

getFullBoxHeader :: Get (BoxVersion, BoxFlags)
getFullBoxHeader = (,) <$> getWord8 <*> getWord24be

putFullBoxHeader :: (FullBox a) => a -> Put
putFullBoxHeader x = do
  putWord8    $ getVersion x
  putWord24be $ getFlags x

getWord24be :: Get Word32
getWord24be = do
  n1 <- getWord8
  n2 <- getWord16be
  return $ fromIntegral n1 + fromIntegral n2

putWord24be :: Word32 -> Put
putWord24be n = do
  putWord8 $ fromIntegral $ n `div` 0x10000
  putWord16be $ fromIntegral n

-- TODO: signed or unsigned の確認
getFixedPoint32 :: Get Double
getFixedPoint32 = do
  n <- getWord32be
  return $ (fromIntegral n) / 0x10000

getFixedPoint16 :: Get Float
getFixedPoint16 = do
  n <- getWord16be
  return $ (fromIntegral n) / 0x100

putFixedPoint32 :: Double -> Put
putFixedPoint32 n = do
  putWord16be $ truncate n
  putWord16be $ truncate (n * 0x10000)

putFixedPoint16 :: Float -> Put
putFixedPoint16 n = do
  putWord8 $ truncate n
  putWord8 $ truncate (n * 0x100)

getWord32or64be :: Bool -> Get Word64
getWord32or64be True  = fromIntegral <$> getWord32be
getWord32or64be False = getWord64be

putWord32or64be :: Bool -> Word64 -> Put
putWord32or64be True  = putWord32be . fromIntegral
putWord32or64be False = putWord64be

getBoxImpl :: Get (BoxType, BoxBody)
getBoxImpl = do
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

-- TODO: null or eos
getStringNul :: Get String
-- getStringNul = Char8L.unpack <$> getLazyByteStringNul
getStringNul = Char8L.unpack <$> getRemainingLazyByteString2

putStringNul :: String -> Put
putStringNul s = do
  put $ Char8.pack s
--  putWord8 0
