module SvgEditor.View.Canvas.Define
  ( defines
  , gradientDefine
  ) where

import Prelude
import Type.Row (type (+))
import Color (Color, toHexString)
import Halogen.HTML as HH
import Halogen.HTML.Core (ElemName(..), AttrName(..))
import Halogen.HTML.Elements (Node, Leaf)
import Halogen.HTML.Properties (IProp, attr)
import Halogen.Svg.Elements as HSE
import Halogen.Svg.Attributes as HSA
import Halogen.Svg.Indexed as I
import SvgEditor.Define (Gradient, GradientMode(..), GradientStop, gradientHash)

type GradientStopAttributes
  = ( offset :: Int
    , stopColor :: String
    , stopOpacity :: Number
    )

gradientStop :: forall p i. Leaf GradientStopAttributes p i
gradientStop props = HSE.element (ElemName "stop") props []

offset :: forall r i. Int -> IProp ( offset :: Int | r ) i
offset = attr (AttrName "offset") <<< (<>) "%" <<< show

stopColor :: forall r i. Color -> IProp ( stopColor :: String | r ) i
stopColor = attr (AttrName "stop-color") <<< toHexString

stopOpacity :: forall r i. Number -> IProp ( stopOpacity :: Number | r ) i
stopOpacity = attr (AttrName "stop-opacity") <<< show

type GradientAttributes
  = I.GlobalAttributes + I.AllPresentationAttributes + ()

linearGradient :: forall p i. Node GradientAttributes p i
linearGradient = HSE.element $ ElemName "linearGradient"

radialGradient :: forall p i. Node GradientAttributes p i
radialGradient = HSE.element $ ElemName "radialGradient"

gradientDefineStop :: forall a b. GradientStop -> HH.HTML a b
gradientDefineStop stop =
  gradientStop
    [ offset stop.offset
    , stopColor stop.color
    , stopOpacity stop.opacity
    ]

gradientDefine :: forall a b. Gradient -> HH.HTML a b
gradientDefine gradient =
  ( case gradient.mode of
      Linear -> linearGradient
      Radial -> radialGradient
  )
    [ HSA.id $ gradientHash gradient ]
    $ gradientDefineStop
    <$> gradient.stops

defines :: forall a b. Array Gradient -> HH.HTML a b
defines = HSE.defs [] <<< map gradientDefine
