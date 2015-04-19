module Codec.ISOBMFF.Box.TrackFragmentBox where

import Codec.ISOBMFF

data TrackFragmentBox = TrackFragmentBox deriving (Show)

instance Box TrackFragmentBox where
  getType _ = "traf"
  decodeBody _ = do return TrackFragmentBox
  encodeBody _ = do return ()
