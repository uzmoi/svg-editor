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
  Number ->
  Tuple Vec2 (Vec2 -> PathCommand) ->
  HH.HTML a b
overlayPoint f size (Tuple v updateV) =
  HSE.circle
    [ HSA.cx v.x
    , HSA.cy v.y
    , HSA.r $ size / 1.5
    , HSA.fill $ Named "black"
    , HE.onMouseDown \_ -> f updateV
    ]

overlayPoints ::
  forall a b.
  (Int -> (Vec2 -> PathCommand) -> b) ->
  Number -> Array PathCommand -> Array (HH.HTML a b)
overlayPoints f size = concat <<< mapWithIndex \i -> points >>> map (overlayPoint (f i) size)

overlayLine :: forall a b. b -> Number -> Tuple Vec2 PathCommand -> HH.HTML a b
overlayLine f size (Tuple v0 pathCommand) =
  HSE.path
    [ HSA.d [ SP.m SP.Abs v0.x v0.y, toHalogenPathCommand pathCommand ]
    , HSA.fillOpacity 0.0
    , HSA.stroke $ Named "black"
    , HSA.strokeWidth $ size / 2.0
    , HSA.strokeDashArray $ (show $ size * 3.0) <> " " <> show size
    , HE.onClick \_ -> f
    ]

overlayLines :: forall a b. (Int -> b) -> Number -> Array PathCommand -> Array (HH.HTML a b)
overlayLines f size =
  uncons >>> maybe [] (\{ head, tail } -> scanl (Tuple <<< snd) (Tuple head head) tail)
    >>> mapWithIndex \i (Tuple a b) -> overlayLine (f $ i + 1) size $ Tuple (nextPoint a) b
