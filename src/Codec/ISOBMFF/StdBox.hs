{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Codec.ISOBMFF.StdBox where

import Codec.ISOBMFF
import Codec.ISOBMFF.Box
import Codec.ISOBMFF.Box.TrackHeaderBox as TrackHeaderBox
import Codec.ISOBMFF.Box.MediaBox as MediaBox
import Codec.ISOBMFF.Box.MediaHeaderBox as MediaHeaderBox
import Codec.ISOBMFF.Box.HandlerReferenceBox as HandlerReferenceBox
import Codec.ISOBMFF.Box.MediaInformationBox as MediaInformationBox
import Codec.ISOBMFF.Box.VideoMediaHeaderBox as VideoMediaHeaderBox
import Codec.ISOBMFF.Box.SoundMediaHeaderBox as SoundMediaHeaderBox
import Codec.ISOBMFF.Box.DataInformationBox as DataInformationBox
import Data.Binary.Get (Get)

data StdBox = Unknown UnknownBox
            | Ftyp FileTypeBox
            | Free FreeSpaceBox
            | Skip FreeSpaceBox
            | Mdat MediaDataBox
            | Moov MovieBox
            | Mvhd MovieHeaderBox
            | Trak TrackBox
            | Tkhd TrackHeaderBox.TrackHeaderBox
            | Mdia MediaBox.MediaBox
            | Mdhd MediaHeaderBox.MediaHeaderBox
            | Hdlr HandlerReferenceBox.HandlerReferenceBox
            | Minf MediaInformationBox.MediaInformationBox
            | Vmhd VideoMediaHeaderBox.VideoMediaHeaderBox
            | Smhd SoundMediaHeaderBox.SoundMediaHeaderBox
            | Dinf DataInformationBox.DataInformationBox
             deriving (Show)

data AnyBox = forall a . (Box a) => AnyBox a

instance Box AnyBox where
  getType (AnyBox x) = getType x
  decodeBody _ = undefined
  encodeBody (AnyBox x) = encodeBody x

getAnyBox :: StdBox -> AnyBox
getAnyBox (Ftyp x) = AnyBox x
getAnyBox (Free x) = AnyBox x
getAnyBox (Skip x) = AnyBox x
getAnyBox (Mdat x) = AnyBox x
getAnyBox (Moov x) = AnyBox x
getAnyBox (Mvhd x) = AnyBox x
getAnyBox (Trak x) = AnyBox x
getAnyBox (Tkhd x) = AnyBox x
getAnyBox (Mdia x) = AnyBox x
getAnyBox (Mdhd x) = AnyBox x
getAnyBox (Hdlr x) = AnyBox x
getAnyBox (Minf x) = AnyBox x
getAnyBox (Vmhd x) = AnyBox x
getAnyBox (Smhd x) = AnyBox x
getAnyBox (Dinf x) = AnyBox x
getAnyBox (Unknown x) = AnyBox x

instance Box StdBox where
  getType = getType . getAnyBox
  encodeBody = encodeBody . getAnyBox
  decodeBody t @ "ftyp" = decodeBody2 t Ftyp
  decodeBody t @ "free" = decodeBody2 t Free
  decodeBody t @ "skip" = decodeBody2 t Skip
  decodeBody t @ "mdat" = decodeBody2 t Mdat
  decodeBody t @ "moov" = decodeBody2 t Moov
  decodeBody t @ "mvhd" = decodeBody2 t Mvhd
  decodeBody t @ "trak" = decodeBody2 t Trak
  decodeBody t @ "tkhd" = decodeBody2 t Tkhd
  decodeBody t @ "mdia" = decodeBody2 t Mdia
  decodeBody t @ "mdhd" = decodeBody2 t Mdhd
  decodeBody t @ "hdlr" = decodeBody2 t Hdlr
  decodeBody t @ "minf" = decodeBody2 t Minf
  decodeBody t @ "vmhd" = decodeBody2 t Vmhd
  decodeBody t @ "smhd" = decodeBody2 t Smhd
  decodeBody t @ "dinf" = decodeBody2 t Dinf
  decodeBody t          = decodeBody2 t Unknown

decodeBody2 :: (Box a) => BoxType -> (a -> StdBox) -> Get StdBox
decodeBody2 t f = do
  b <- decodeBody t
  return $ f b

-- bs <- (B.decodeFile "/paht/to/file.mp4") :: IO (BoxRoot StdBox)
-- Prelude.putStr $ T.drawForest $ boxForest $ fmap getType bs
-- List.filter (\x -> getType x == "hdlr") $ Data.Foldable.toList bs
