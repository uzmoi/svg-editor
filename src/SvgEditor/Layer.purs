module SvgEditor.Layer (Layer) where

import Halogen.Svg.Attributes (PathCommand)

type Layer
  = { id :: Int
    , name :: String
    , drawPath :: Array PathCommand
    }
