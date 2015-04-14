module Codec.ISOBMFF.Box.MovieExtendsBox where

--Box Type: mvex
--Container: Movie Box (moov)
--Mandatory: No
--Quantity: Zero or more

import Codec.ISOBMFF

data MovieExtendsBox child = MovieExtendsBox [child] deriving (Show)

instance (Box c) => Box (MovieExtendsBox c) where
  getType _ = "mvex"

  makeBox _ body = MovieExtendsBox $ decodeAll body
  getBody box = encodeAll $ getChildren box

getChildren :: (MovieExtendsBox c) -> [c]
getChildren (MovieExtendsBox cs) = cs
