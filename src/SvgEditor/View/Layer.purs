module SvgEditor.View.Layer (layer) where

import Unsafe.Coerce (unsafeCoerce)
import Halogen.HTML as HH
import Halogen.HTML.Properties (IProp)
import Halogen.Svg.Elements as HSE
import Halogen.Svg.Attributes as HSA
import SvgEditor.Layer (Layer, fillRule)

-- FIXME: HSA.strokeOpacity :: ... (strokeOpacity :: String ...) ...
strokeOpacity :: forall r i. Number -> IProp ( strokeOpacity :: Number | r ) i
strokeOpacity = unsafeCoerce HSA.strokeOpacity

-- FIXME: HSA.strokeDashOffset :: ... (strokeDashOffset :: String ...) ...
strokeDashOffset :: forall r i. Number -> IProp ( strokeDashOffset :: Number | r ) i
strokeDashOffset = unsafeCoerce HSA.strokeDashOffset

layer :: forall a b. Layer -> HH.HTML a b
layer { drawPath, fill, stroke } =
  HSE.path
    [ HSA.d drawPath
    , HSA.fill fill.color
    , HSA.fillOpacity fill.opacity
    , fillRule fill.rule
    , HSA.stroke stroke.color
    , strokeOpacity stroke.opacity
    , HSA.strokeWidth stroke.width
    , strokeDashOffset stroke.dashOffset
    , HSA.strokeDashArray stroke.dashArray
    , HSA.strokeLineCap stroke.lineCap
    , HSA.strokeLineJoin stroke.lineJoin
    , HSA.strokeMiterLimit stroke.miterLimit
    ]
