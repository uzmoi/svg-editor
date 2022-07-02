module SvgEditor.View.Canvas.Layer (svgLayer) where

import Prelude
import Data.Array (catMaybes)
import Data.Maybe (Maybe(..))
import Unsafe.Coerce (unsafeCoerce)
import Halogen.HTML as HH
import Halogen.HTML.Properties (IProp)
import Halogen.Svg.Elements as HSE
import Halogen.Svg.Attributes as HSA
import SvgEditor.Layer (Layer, Fill, Stroke, fillRule, defaultFill, defaultStroke)
import SvgEditor.PathCommand (toHalogenPathCommand)

-- FIXME: HSA.strokeOpacity :: ... (strokeOpacity :: String ...) ...
strokeOpacity :: forall r i. Number -> IProp ( strokeOpacity :: Number | r ) i
strokeOpacity = unsafeCoerce HSA.strokeOpacity

-- FIXME: HSA.strokeDashOffset :: ... (strokeDashOffset :: String ...) ...
strokeDashOffset :: forall r i. Number -> IProp ( strokeDashOffset :: Number | r ) i
strokeDashOffset = unsafeCoerce HSA.strokeDashOffset

svgLayer :: forall a b. Layer -> HH.HTML a b
svgLayer { drawPath, fill, stroke } =
  HSE.path
    $ catMaybes
        [ Just $ HSA.d (drawPath # map toHalogenPathCommand)
        , fillAttr _.paint # map (HSA.Named >>> HSA.fill)
        , fillAttr _.opacity # map HSA.fillOpacity
        , fillAttr _.rule # map fillRule
        , strokeAttr _.paint # map (HSA.Named >>> HSA.stroke)
        , strokeAttr _.opacity # map strokeOpacity
        , strokeAttr _.width # map HSA.strokeWidth
        , strokeAttr _.dashOffset # map strokeDashOffset
        , strokeAttr _.dashArray # map HSA.strokeDashArray
        , strokeAttr _.lineCap # map HSA.strokeLineCap
        , strokeAttr _.lineJoin # map HSA.strokeLineJoin
        , strokeAttr _.miterLimit # map HSA.strokeMiterLimit
        ]
  where
  fillAttr :: forall a. Eq a => (Fill -> a) -> Maybe a
  fillAttr = justIfNotDefault defaultFill fill

  strokeAttr :: forall a. Eq a => (Stroke -> a) -> Maybe a
  strokeAttr = justIfNotDefault defaultStroke stroke

justIfNotDefault :: forall a b. Eq b => a -> a -> (a -> b) -> Maybe b
justIfNotDefault default value f =
  let
    value' = f value
  in
    if f default == value' then
      Nothing
    else
      Just value'
