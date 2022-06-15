module SvgEditor.View.Overlay
  ( overlay
  ) where

import Prelude
import Data.Array (concatMap)
import Halogen.HTML as HH
import Data.Array (concatMap)
import Halogen.Svg.Elements as HSE
import Halogen.Svg.Attributes as HSA
import Halogen.Svg.Attributes (Color(..))
import SvgEditor.PathCommand (PathCommand(..))

overlay :: forall a b. Array PathCommand -> Array (HH.HTML a b)
overlay pathCommands =
  pathCommands
    # concatMap \pathCommand ->
        map
          ( \v ->
              HSE.circle
                [ HSA.cx v.x
                , HSA.cy v.y
                , HSA.r 1.0
                , HSA.fill $ Named "black"
                ]
          ) case pathCommand of
          Move ref v -> [ v ]
          Line ref v -> [ v ]
          Bez3 ref v1 v2 v3 -> [ v1, v2, v3 ]
          Bez3' ref v1 v2 -> [ v1, v2 ]
          Bez2 ref v1 v2 -> [ v1, v2 ]
          Bez2' ref v1 -> [ v1 ]
          Close -> []
