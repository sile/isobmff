module Codec.ISOBMFF.Box.FreeSpaceBox where

-- Box Types: free, skip
-- Container: File or other box
-- Mandatory: No
-- Quantity: Zero or more

import Codec.ISOBMFF

data FreeSpaceBox = FreeSpaceBox BoxType Body

instance Box FreeSpaceBox where
  makeBox = FreeSpaceBox
  getType (FreeSpaceBox t _) = t
  getBody (FreeSpaceBox _ b) = b

instance Show FreeSpaceBox where
  show (FreeSpaceBox t _) = t
