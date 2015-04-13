module Codec.ISOBMFF.Box.FileTypeBox where

-- Box Type: ftype
-- Container: File
-- Mandatory: Yes
-- Quantity: Exactly one (but see below)

import Codec.ISOBMFF
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString.Char8 as Char8
import Data.ByteString.Lazy.Char8 as LChar8
import Data.Word

data FileTypeBox = FileTypeBox {
  getMajorBrand :: String,
  getMinorVersion :: Word32,
  getCompatibleBrands :: [String]
  } deriving (Show)

instance Box FileTypeBox where
  getType _ = "ftyp"

  makeBox _ = runGet $ do
    majorBrand <- getByteString 4
    minorVersion <- getWord32be
    compatibleBrands <- decodeCompatibleBrands
    return FileTypeBox {
      getMajorBrand = Char8.unpack majorBrand,
      getMinorVersion = minorVersion,
      getCompatibleBrands = compatibleBrands
      }

  getBody box = runPut $ do
    putByteString $ Char8.pack $ getMajorBrand box
    putWord32be $ getMinorVersion box
    putByteString $ Char8.pack $ Prelude.concat $ getCompatibleBrands box

decodeCompatibleBrands :: Get [String]
decodeCompatibleBrands = do
  bytes <- getRemainingLazyByteString
  return $ split4 $ LChar8.unpack bytes
  where split4 [] = []
        split4 (a:b:c:d:xs) = (a:b:c:[d]) : split4 xs
