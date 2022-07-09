module SvgEditor.View.Canvas
  ( canvasContainerRef
  , svgCanvas
  ) where

import Prelude
import Data.Array (filter, find)
import Data.Maybe (maybe)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties (IProp)
import Halogen.HTML.Properties.ARIA as ARIA
import Halogen.Svg.Elements as HSE
import Halogen.Svg.Attributes (class_)
import Halogen.Svg.Attributes as HSA
import Halogen.Svg.Indexed as I
import SvgEditor.Vec (Vec2, vec2)
import SvgEditor.Canvas (Canvas)
import SvgEditor.Layer (Layer)
import SvgEditor.PathCommand (PathCommand)
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

transform :: Vec2 Number -> Number -> String
transform translate scale =
  "transform:"
    <> (translate # vec2 \x y -> "translate(" <> show x <> "px," <> show y <> "px)")
    <> ("scale(" <> show scale <> ")")

svgCanvas ::
  forall a b c.
  { dragStart :: Int -> (Vec2 Number -> PathCommand) -> b
  , addCommand :: Int -> b
  } ->
  Number ->
  { translate :: Vec2 Number
  , canvas :: Canvas
  , refImage ::
      { uri :: String
      , translate :: Vec2 Number
      , scale :: Number
      , show :: Boolean
      }
  , layers :: Array Layer
  , selectedLayer :: Int
  | c
  } ->
  HH.HTML a b
svgCanvas actions scale { translate, canvas, refImage, layers, selectedLayer } =
  HH.div
    [ HP.ref canvasContainerRef
    , HP.class_ $ HH.ClassName "canvas-container"
    , HP.style $ transform translate scale
    ]
    [ HSE.svg (canvasProps canvas) $ layers # filter _.show # map svgLayer
    , HH.div
        [ HP.class_ $ HH.ClassName "ref-image-container" ]
        [ HH.div
            [ HP.class_ $ HH.ClassName "ref-image"
            , HP.style $ "background-image:url(\"" <> refImage.uri <> "\");"
                <> transform refImage.translate refImage.scale
            , ARIA.hidden $ show $ not refImage.show
            ]
            []
        ]
    , HSE.svg
        (canvasProps canvas <> [ class_ $ HH.ClassName "overlay" ])
        ( layers # find (_.id >>> (==) selectedLayer)
            # maybe [] \layer ->
                overlayLines actions.addCommand size layer.drawPath
                  <> overlayPoints actions.dragStart size layer.drawPath
        )
    ]
  where
  size = 1.0 / scale
