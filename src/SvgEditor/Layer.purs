module SvgEditor.Layer (Layer) where

import Prelude
import Halogen.Svg.Attributes (PathCommand)

type Layer
  = { name :: String
    , drawPath :: Array PathCommand
    }
