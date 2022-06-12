module SvgEditor.View.Canvas (canvasProps) where

import Halogen.HTML.Properties (IProp)
import Halogen.Svg.Attributes as HSA
import Halogen.Svg.Indexed as I
import SvgEditor.Canvas (Canvas)

canvasProps :: Canvas -> forall i. Array (IProp I.SVGsvg i)
canvasProps { viewBox } =
  [ HSA.viewBox
      viewBox.top
      viewBox.left
      viewBox.bottom
      viewBox.right
  ]
