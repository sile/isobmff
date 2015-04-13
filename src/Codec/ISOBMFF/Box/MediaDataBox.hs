module Codec.ISOBMFF.Box.MediaDataBox where

-- Box Type: mdat
-- Container: File
-- Mandatory: No
-- Quantity: Zero or more
import Codec.ISOBMFF

import Data.ByteString.Lazy as BL

data MediaDataBox = MediaDataBox Body

instance Box MediaDataBox where
  makeBox _ = MediaDataBox
  getType _ = "mdat"
  getBody (MediaDataBox b) = b

instance Show MediaDataBox where
  show (MediaDataBox b) = show $ BL.length b
