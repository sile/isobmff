module Codec.ISOBMFF.StdBox where

import Codec.ISOBMFF4 (Box, FullBox, BoxList)
import Codec.ISOBMFF.Box

data StdBox c = Unknown (UnknownBox c)
            deriving (Show)

instance Box StdBox

type StdBoxList = BoxList StdBox
