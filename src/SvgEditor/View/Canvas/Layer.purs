module SvgEditor.View.Canvas.Layer (svgLayer) where

import Prelude
import Data.Array (catMaybes)
import Data.Maybe (Maybe(..))
import Unsafe.Coerce (unsafeCoerce)
import Halogen.HTML as HH
import Halogen.HTML.Properties (IProp)
import Halogen.Svg.Elements as HSE
import Halogen.Svg.Attributes as HSA
import SvgEditor.Layer as Layer
import SvgEditor.PathCommand (toHalogenPathCommand)

-- FIXME: HSA.strokeOpacity :: ... (strokeOpacity :: String ...) ...
strokeOpacity :: forall r i. Number -> IProp ( strokeOpacity :: Number | r ) i
strokeOpacity = unsafeCoerce HSA.strokeOpacity

-- FIXME: HSA.strokeDashOffset :: ... (strokeDashOffset :: String ...) ...
strokeDashOffset :: forall r i. Number -> IProp ( strokeDashOffset :: Number | r ) i
strokeDashOffset = unsafeCoerce HSA.strokeDashOffset

svgLayer :: forall a b. Layer.Layer -> HH.HTML a b
svgLayer { drawPath, fill, stroke, attr } =
  HSE.path
    $ catMaybes
        [ Just $ HSA.d (drawPath # map toHalogenPathCommand)
        , fillAttr _.paint # map (HSA.Named >>> HSA.fill)
        , fillAttr _.opacity # map HSA.fillOpacity
        , fillAttr _.rule # map Layer.fillRule
        , strokeAttr _.paint # map (HSA.Named >>> HSA.stroke)
        , strokeAttr _.opacity # map strokeOpacity
        , strokeAttr _.width # map HSA.strokeWidth
        , strokeAttr _.dashOffset # map strokeDashOffset
        , strokeAttr _.dashArray # map HSA.strokeDashArray
        , strokeAttr _.lineCap # map HSA.strokeLineCap
        , strokeAttr _.lineJoin # map HSA.strokeLineJoin
        , strokeAttr _.miterLimit # map HSA.strokeMiterLimit
        , layerAttr _.id # map HSA.id
        , layerAttr _.class # map (HH.ClassName >>> HSA.class_)
        ]
  where
  fillAttr :: forall c. Eq c => (Layer.Fill -> c) -> Maybe c
  fillAttr f = justIfNotDefault (f Layer.defaultFill) (f fill)

  strokeAttr :: forall c. Eq c => (Layer.Stroke -> c) -> Maybe c
  strokeAttr f = justIfNotDefault (f Layer.defaultStroke) (f stroke)

  layerAttr :: forall c. Eq c => (Layer.Attr -> c) -> Maybe c
  layerAttr f = justIfNotDefault (f Layer.defaultAttr) (f attr)

justIfNotDefault :: forall a. Eq a => a -> a -> Maybe a
justIfNotDefault default value = if default == value then Nothing else Just value
