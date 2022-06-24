module SvgEditor.View.Canvas.Overlay
  ( overlayLines
  , overlayPoints
  ) where

import Prelude
import Data.Array (concat, mapWithIndex, scanl, uncons)
import Data.Tuple (Tuple(..), snd)
import Data.Maybe (maybe)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Svg.Elements as HSE
import Halogen.Svg.Attributes as HSA
import Halogen.Svg.Attributes (Color(..))
import Halogen.Svg.Attributes.Path as SP
import SvgEditor.PathCommand (PathCommand, Vec2, points, nextPoint, toHalogenPathCommand)

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

overlayPoints ::
  forall a b.
  (Int -> (Vec2 -> PathCommand) -> b) ->
  Array PathCommand -> Array (HH.HTML a b)
overlayPoints f = concat <<< mapWithIndex \i -> points >>> map (overlayPoint $ f i)

overlayLine :: forall a b. b -> Tuple Vec2 PathCommand -> HH.HTML a b
overlayLine f (Tuple v0 pathCommand) =
  HSE.path
    [ HSA.d [ SP.m SP.Abs v0.x v0.y, toHalogenPathCommand pathCommand ]
    , HSA.fillOpacity 0.0
    , HSA.stroke $ Named "black"
    , HSA.strokeDashArray "4"
    , HE.onClick \_ -> f
    ]

overlayLines :: forall a b. (Int -> b) -> Array PathCommand -> Array (HH.HTML a b)
overlayLines f =
  uncons >>> maybe [] (\{ head, tail } -> scanl (Tuple <<< snd) (Tuple head head) tail)
    >>> mapWithIndex \i (Tuple a b) -> overlayLine (f $ i + 1) $ Tuple (nextPoint a) b
