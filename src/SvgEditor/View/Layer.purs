module SvgEditor.View.Layer (layer) where

import Halogen.HTML as HH
import Halogen.Svg.Elements as HSE
import Halogen.Svg.Attributes as HSA
import SvgEditor.Layer (Layer)

layer :: forall a b. Layer -> HH.HTML a b
layer { drawPath } = HSE.path [ HSA.d drawPath ]
