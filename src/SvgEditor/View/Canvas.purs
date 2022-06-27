module SvgEditor.View.Canvas
  ( canvasContainerRef
  , svgCanvas
  ) where

import Prelude
import Data.Array (filter, find, snoc)
import Data.Maybe (Maybe(..))
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
  forall a b.
  { dragStart :: Int -> (Vec2 -> PathCommand) -> b
  , addCommand :: Int -> b
  } ->
  Number ->
  Canvas ->
  Array Layer ->
  Int ->
  HH.HTML a b
svgCanvas actions scale canvas layers selectedLayer =
  HH.div
    [ HP.ref canvasContainerRef
    , HP.class_ $ HH.ClassName "canvas-container"
    , HP.style $ "transform:scale(" <> show scale <> ")"
    ]
    [ HSE.svg (canvasProps canvas) $ showLayers # map svgLayer
    , case showLayers # find (_.id >>> (==) selectedLayer) of
        (Just layer) ->
          HSE.svg
            (snoc (canvasProps canvas) $ class_ $ HH.ClassName "overlay")
            $ overlayLines actions.addCommand size layer.drawPath
            <> overlayPoints actions.dragStart size layer.drawPath
        Nothing -> HH.div_ []
    ]
  where
  size = 1.0 / scale

  showLayers = layers # filter _.show
