module Codec.ISOBMFF.Box.TrackBox where

--Box Type: trak
--Container: Movie Box (moov)
--Mandatory: Yes
--Quantity: One or more

import Codec.ISOBMFF

data TrackBox child = TrackBox [child] deriving (Show)

instance (Box c) => Box (TrackBox c) where
  getType _ = "trak"

  makeBox _ body = TrackBox $ decodeAll body
  getBody box = encodeAll $ getChildren box

getChildren :: (TrackBox c) -> [c]
getChildren (TrackBox cs) = cs
