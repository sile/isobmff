module Codec.ISOBMFF.Box.MovieFragmentBox where

import Codec.ISOBMFF

data MovieFragmentBox = MovieFragmentBox deriving (Show)

instance Box MovieFragmentBox where
  getType _ = "moof"
  decodeBody _ = do return MovieFragmentBox
  encodeBody _ = do return ()
