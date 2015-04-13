module Codec.ISOBMFF.Box.UnknownBox where

import Codec.ISOBMFF

data UnknownBox = UnknownBox BoxType Body
                -- deriving (Show)

instance Box UnknownBox where
  makeBox = UnknownBox
  getType (UnknownBox t _) = t
  getBody (UnknownBox _ b) = b

instance Show UnknownBox where
  show (UnknownBox t _) = t
