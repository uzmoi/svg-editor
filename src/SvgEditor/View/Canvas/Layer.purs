module SvgEditor.View.Canvas.Layer (svgLayer) where

import Prelude
import Data.Array (catMaybes)
import Data.Maybe (Maybe(..))
import Unsafe.Coerce (unsafeCoerce)
import Halogen.HTML as HH
import Halogen.HTML.Properties (IProp)
import Halogen.Svg.Elements as HSE
import Halogen.Svg.Attributes as HSA
import SvgEditor.Layer (Layer, fillRule, defaultFill, defaultStroke)
import SvgEditor.PathCommand (toHalogenPathCommand)

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

svgLayer :: forall a b. Layer -> HH.HTML a b
svgLayer { drawPath, fill, stroke } =
  HSE.path
    $ catMaybes
        [ Just $ HSA.d (drawPath # map toHalogenPathCommand)
        , justIf transparent HSA.fill fill.color
        , justIf ((==) defaultFill.opacity) HSA.fillOpacity fill.opacity
        , justIf ((==) defaultFill.rule) fillRule fill.rule
        , justIf transparent HSA.stroke stroke.color
        , justIf ((==) defaultStroke.opacity) strokeOpacity stroke.opacity
        , justIf ((==) defaultStroke.width) HSA.strokeWidth stroke.width
        , justIf ((==) defaultStroke.dashOffset) strokeDashOffset stroke.dashOffset
        , justIf ((==) defaultStroke.dashArray) HSA.strokeDashArray stroke.dashArray
        , justIf ((==) defaultStroke.lineCap) HSA.strokeLineCap stroke.lineCap
        , justIf ((==) defaultStroke.lineJoin) HSA.strokeLineJoin stroke.lineJoin
        , justIf ((==) defaultStroke.miterLimit) HSA.strokeMiterLimit stroke.miterLimit
        ]
