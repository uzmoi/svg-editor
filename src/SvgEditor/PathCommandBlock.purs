module SvgEditor.PathCommandBlock where

import Prelude
import Data.Tuple (Tuple)
import SvgEditor.PathCommand (PathCommand)

type PathCommandBlock
  = { id :: String, command :: PathCommand }
