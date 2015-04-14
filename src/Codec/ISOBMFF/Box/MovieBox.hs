module Codec.ISOBMFF.Box.MovieBox where

--Box Type: moov
--Container: File
--Mandatory: Yes
--Quantity: Exactly one

import Codec.ISOBMFF

data MovieBox child = MovieBox [child] deriving (Show)

instance (Box c) => Box (MovieBox c) where
  getType _ = "moov"

  makeBox _ body = MovieBox $ decodeAll body
  getBody box = encodeAll $ getChildren box

getChildren :: (MovieBox c) -> [c]
getChildren (MovieBox cs) = cs
