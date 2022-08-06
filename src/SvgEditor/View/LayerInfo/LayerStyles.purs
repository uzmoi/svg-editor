module SvgEditor.View.LayerInfo.LayerStyles
  ( layerStyles
  ) where

import Prelude
import Data.Maybe (fromJust)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Array (find)
import Partial.Unsafe (unsafePartial)
import Effect.Aff (Aff)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Svg.Attributes.StrokeLineCap (StrokeLineCap(..), printStrokeLineCap)
import Halogen.Svg.Attributes.StrokeLineJoin (StrokeLineJoin(..), printStrokeLineJoin)
import SvgEditor.Layer (Layer, Fill, Stroke, FillRule(..))
import SvgEditor.View.NumberInput (numberInput)
import SvgEditor.View.InputControl (Slot)

select :: forall a b x. Eq x => String -> Array x -> (x -> String) -> x -> (x -> b) -> HH.HTML a b
select id xs print value f =
  let
    ys = xs # map \x -> Tuple (print x) x
  in
    HH.select
      [ HP.id $ id
      , HP.class_ $ HH.ClassName "input"
      , HE.onValueInput \x ->
          f $ snd $ unsafePartial fromJust $ ys # find (fst >>> (==) x)
      ]
      $ ys
      # map \(Tuple name x) ->
          HH.option
            [ HP.value name, HP.selected $ value == x ]
            [ HH.text name ]

fill_ :: (Fill -> Fill) -> Layer -> Layer
fill_ f layer = layer { fill = f layer.fill }

stroke_ :: (Stroke -> Stroke) -> Layer -> Layer
stroke_ f layer = layer { stroke = f layer.stroke }

layerStyles ::
  forall a b.
  { editLayer :: (Layer -> Layer) -> a | b } ->
  Fill -> Stroke -> HH.ComponentHTML a (Slot Number) Aff
layerStyles actions fill stroke =
  HH.div_
    [ stringInput'
        { name: "fill-paint"
        , value: fill.paint
        , onChange: \x -> fill_ _ { paint = x }
        }
    , numberInput'
        { name: "fill-opacity"
        , value: fill.opacity
        , onChange: \x -> fill_ _ { opacity = clamp 0.0 1.0 x }
        }
    , selectInput'
        { name: "fill-rule"
        , value: fill.rule
        , xs: [ NonZero, EvenOdd ]
        , print: show
        , onChange: \x -> fill_ _ { rule = x }
        }
    , stringInput'
        { name: "stroke-paint"
        , value: stroke.paint
        , onChange: \x -> stroke_ _ { paint = x }
        }
    , numberInput'
        { name: "stroke-opacity"
        , value: stroke.opacity
        , onChange: \x -> stroke_ _ { opacity = clamp 0.0 1.0 x }
        }
    , numberInput'
        { name: "stroke-width"
        , value: stroke.width
        , onChange: \x -> stroke_ _ { width = x }
        }
    , numberInput'
        { name: "dash-offset"
        , value: stroke.dashOffset
        , onChange: \x -> stroke_ _ { dashOffset = x }
        }
    , stringInput'
        { name: "dash-array"
        , value: stroke.dashArray
        , onChange: \x -> stroke_ _ { dashArray = x }
        }
    , selectInput'
        { name: "line-cap"
        , value: stroke.lineCap
        , xs: [ LineCapButt, LineCapSquare, LineCapRound ]
        , print: printStrokeLineCap
        , onChange: \x -> stroke_ _ { lineCap = x }
        }
    , selectInput'
        { name: "line-join"
        , value: stroke.lineJoin
        , xs: [ LineJoinArcs, LineJoinBevel, LineJoinMiter, LineJoinMiterClip, LineJoinRound ]
        , print: printStrokeLineJoin
        , onChange: \x -> stroke_ _ { lineJoin = x }
        }
    , numberInput'
        { name: "miter-limit"
        , value: stroke.miterLimit
        , onChange: \x -> stroke_ _ { miterLimit = x }
        }
    ]
  where
  stringInput' { name, value, onChange } =
    input' name
      $ HH.input
          [ HP.id $ "layer-info." <> name
          , HP.value value
          , HE.onValueInput (actions.editLayer <<< onChange)
          , HP.class_ $ HH.ClassName "input"
          ]

  numberInput' { name, value, onChange } =
    input' name
      $ numberInput ("layer-info." <> name) value (actions.editLayer <<< onChange)

  selectInput' ::
    forall c x.
    Eq x =>
    { name :: String
    , value :: x
    , xs :: Array x
    , print :: x -> String
    , onChange :: x -> Layer -> Layer
    } ->
    HH.HTML c a
  selectInput' { name, value, xs, print, onChange } =
    input' name
      $ select ("layer-info." <> name) xs print value (actions.editLayer <<< onChange)

  input' :: forall c d. String -> HH.HTML c d -> HH.HTML c d
  input' name input =
    HH.dl
      [ HP.class_ $ HH.ClassName "style-input" ]
      [ HH.dt_ [ HH.label [ HP.for $ "layer-info." <> name ] [ HH.text name ] ]
      , HH.dd_ [ input ]
      ]