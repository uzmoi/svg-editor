module SvgEditor.View.Layer (layer) where

import Prelude
import Data.Array (catMaybes)
import Data.Maybe (Maybe(..))
import Unsafe.Coerce (unsafeCoerce)
import Halogen.HTML as HH
import Halogen.HTML.Properties (IProp)
import Halogen.Svg.Elements as HSE
import Halogen.Svg.Attributes as HSA
import Halogen.Svg.Attributes.StrokeLineCap (StrokeLineCap(..))
import Halogen.Svg.Attributes.StrokeLineJoin (StrokeLineJoin(..))
import SvgEditor.Layer (Layer, FillRule(..), fillRule)

-- FIXME: HSA.strokeOpacity :: ... (strokeOpacity :: String ...) ...
strokeOpacity :: forall r i. Number -> IProp ( strokeOpacity :: Number | r ) i
strokeOpacity = unsafeCoerce HSA.strokeOpacity

-- FIXME: HSA.strokeDashOffset :: ... (strokeDashOffset :: String ...) ...
strokeDashOffset :: forall r i. Number -> IProp ( strokeDashOffset :: Number | r ) i
strokeDashOffset = unsafeCoerce HSA.strokeDashOffset

justIf :: forall a b. (a -> Boolean) -> (a -> b) -> a -> Maybe b
justIf f g value = if f value then Nothing else Just $ g value

transparent :: HSA.Color -> Boolean
transparent = case _ of
  HSA.RGBA _ _ _ 0.0 -> true
  HSA.Named "transparent" -> true
  _ -> false

layer :: forall a b. Layer -> HH.HTML a b
layer { drawPath, fill, stroke } =
  HSE.path
    $ catMaybes
        [ Just $ HSA.d drawPath
        , justIf transparent HSA.fill fill.color
        , justIf ((==) 1.0) HSA.fillOpacity fill.opacity
        , justIf ((==) NonZero) fillRule fill.rule
        , justIf transparent HSA.stroke stroke.color
        , justIf ((==) 1.0) strokeOpacity stroke.opacity
        , justIf ((==) 1.0) HSA.strokeWidth stroke.width
        , justIf ((==) 0.0) strokeDashOffset stroke.dashOffset
        , justIf ((==) "") HSA.strokeDashArray stroke.dashArray
        , justIf ((==) LineCapButt) HSA.strokeLineCap stroke.lineCap
        , justIf ((==) LineJoinMiter) HSA.strokeLineJoin stroke.lineJoin
        , justIf ((==) 4.0) HSA.strokeMiterLimit stroke.miterLimit
        ]
