module SvgEditor.View.Canvas (canvasProps) where

import Halogen.Svg.Attributes as HSA
import SvgEditor.Canvas (Canvas)
import Halogen.HTML.Properties (IProp)
import Halogen.Svg.Indexed as I

canvasProps :: Canvas -> forall i. Array (IProp I.SVGsvg i)
canvasProps { viewBox } =
  [ HSA.viewBox
      viewBox.top
      viewBox.left
      viewBox.bottom
      viewBox.right
  ]
