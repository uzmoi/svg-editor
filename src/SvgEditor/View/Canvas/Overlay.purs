module SvgEditor.View.Canvas.Overlay
  ( overlay
  ) where

import Prelude
import Data.Array (concat, mapWithIndex)
import Data.Tuple (Tuple(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Svg.Elements as HSE
import Halogen.Svg.Attributes as HSA
import Halogen.Svg.Attributes (Color(..))
import SvgEditor.PathCommand (PathCommand, Vec2, points)

overlayPoint ::
  forall a b.
  ((Vec2 -> PathCommand) -> b) ->
  Tuple Vec2 (Vec2 -> PathCommand) ->
  HH.HTML a b
overlayPoint f (Tuple v updateV) =
  HSE.circle
    [ HSA.cx v.x
    , HSA.cy v.y
    , HSA.r 1.0
    , HSA.fill $ Named "black"
    , HE.onMouseDown \_ -> f updateV
    ]

overlay :: forall a b. (Int -> (Vec2 -> PathCommand) -> b) -> Array PathCommand -> Array (HH.HTML a b)
overlay f = concat <<< mapWithIndex \i -> map (overlayPoint $ f i) <<< points
