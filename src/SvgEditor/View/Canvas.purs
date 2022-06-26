module SvgEditor.View.Canvas
  ( canvasContainerRef
  , svgCanvas
  ) where

import Prelude
import Data.Array (filter, find, snoc)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties (IProp)
import Halogen.Svg.Elements as HSE
import Halogen.Svg.Attributes (class_)
import Halogen.Svg.Attributes as HSA
import Halogen.Svg.Indexed as I
import Web.UIEvent.MouseEvent (MouseEvent)
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
  { drag :: MouseEvent -> b
  , dragStart :: Int -> (Vec2 -> PathCommand) -> b
  , addCommand :: Int -> b
  } ->
  Canvas ->
  Array Layer ->
  Int ->
  HH.HTML a b
svgCanvas actions canvas layers selectedLayer =
  HH.div
    [ HP.ref canvasContainerRef
    , HP.class_ $ HH.ClassName "canvas-container"
    , HE.onMouseMove actions.drag
    ]
    [ HSE.svg (canvasProps canvas) $ showLayers # map svgLayer
    , case showLayers # find (_.id >>> (==) selectedLayer) of
        (Just layer) ->
          HSE.svg
            (snoc (canvasProps canvas) $ class_ $ HH.ClassName "overlay")
            $ overlayLines actions.addCommand layer.drawPath
            <> overlayPoints actions.dragStart layer.drawPath
        Nothing -> HH.div_ []
    ]
  where
  showLayers = layers # filter _.show
