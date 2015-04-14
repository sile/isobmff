module Codec.ISOBMFF.StdCodec where

-- TOOD: rename StdBox

import Codec.ISOBMFF
import Codec.ISOBMFF.Box.UnknownBox
import Codec.ISOBMFF.Box.FileTypeBox
import Codec.ISOBMFF.Box.FreeSpaceBox
import Codec.ISOBMFF.Box.MediaDataBox
import Codec.ISOBMFF.Box.MovieBox

import Data.ByteString.Lazy as BL

data StdBox = Unknown UnknownBox
            | Ftyp FileTypeBox
            | Free FreeSpaceBox
            | Skip FreeSpaceBox
            | Mdat MediaDataBox
            | Moov (MovieBox StdBox)
            deriving (Show)

-- XXX: too redundant code

instance Box StdBox where
  makeBox t @ "ftyp" b = Ftyp $ makeBox t b
  makeBox t @ "free" b = Free $ makeBox t b
  makeBox t @ "skip" b = Skip $ makeBox t b
  makeBox t @ "mdat" b = Mdat $ makeBox t b
  makeBox t @ "moov" b = Moov $ makeBox t b
  makeBox t          b = Unknown $ makeBox t b

  getType (Ftyp x) = getType x
  getType (Free x) = getType x
  getType (Skip x) = getType x
  getType (Mdat x) = getType x
  getType (Moov x) = getType x
  getType (Unknown x) = getType x

  getBody (Ftyp x) = getBody x
  getBody (Free x) = getBody x
  getBody (Skip x) = getBody x
  getBody (Mdat x) = getBody x
  getBody (Moov x) = getBody x
  getBody (Unknown x) = getBody x

getChildren (Moov x) = Codec.ISOBMFF.Box.MovieBox.getChildren x
getChildren _        = []
