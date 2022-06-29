module SvgEditor.View.Canvas
  ( canvasContainerRef
  , svgCanvas
  ) where

import Prelude
import Data.Array (filter, find, snoc)
import Data.Maybe (maybe)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties (IProp)
import Halogen.Svg.Elements as HSE
import Halogen.Svg.Attributes (class_)
import Halogen.Svg.Attributes as HSA
import Halogen.Svg.Indexed as I
import SvgEditor.Canvas (Canvas)
import SvgEditor.Layer (Layer)
import SvgEditor.PathCommand (PathCommand, Vec2)
import SvgEditor.View.Canvas.Layer (svgLayer)
import SvgEditor.View.Canvas.Overlay (overlayPoints, overlayLines)

canvasProps :: Canvas -> forall i. Array (IProp I.SVGsvg i)
canvasProps { viewBox } =
  [ HSA.viewBox
      viewBox.top
      viewBox.left
      viewBox.bottom
      viewBox.right
  ]

canvasContainerRef :: H.RefLabel
canvasContainerRef = H.RefLabel "canvasContainer"

svgCanvas ::
  forall a b c.
  { dragStart :: Int -> (Vec2 -> PathCommand) -> b
  , addCommand :: Int -> b
  } ->
  Number ->
  { translate :: Vec2
  , canvas :: Canvas
  , layers :: Array Layer
  , selectedLayer :: Int
  | c
  } ->
  HH.HTML a b
svgCanvas actions scale { translate, canvas, layers, selectedLayer } =
  HH.div
    [ HP.ref canvasContainerRef
    , HP.class_ $ HH.ClassName "canvas-container"
    , HP.style
        $ ("transform:translate(" <> show translate.x <> "px," <> show translate.y <> "px)")
        <> ("scale(" <> show scale <> ")")
    ]
    [ HSE.svg (canvasProps canvas) $ layers # filter _.show # map svgLayer
    , HSE.svg
        (snoc (canvasProps canvas) $ class_ $ HH.ClassName "overlay")
        ( layers # find (_.id >>> (==) selectedLayer)
            # maybe [] \layer ->
                overlayLines actions.addCommand size layer.drawPath
                  <> overlayPoints actions.dragStart size layer.drawPath
        )
    ]
  where
  size = 1.0 / scale
