module SvgEditor.View.LayerInfo where

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
import SvgEditor.Layer (Layer, FillRule(..))
import SvgEditor.PathCommand (PathCommand)
import SvgEditor.View.DrawPath (drawPath)
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

layerInfo ::
  forall a.
  { editLayer :: (Layer -> Layer) -> a
  , deleteLayer :: a
  , editCommand :: Int -> PathCommand -> a
  } ->
  Layer -> HH.ComponentHTML a (Slot Number) Aff
layerInfo actions { name, drawPath: drawPath', fill, stroke } =
  HH.div
    [ HP.class_ $ HH.ClassName "layer-info" ]
    [ HH.input
        [ HP.value name
        , HE.onValueInput \value -> actions.editLayer _ { name = value }
        , HP.class_ $ HH.ClassName "layer-name-input"
        ]
    , HH.button
        [ HE.onClick \_ -> actions.deleteLayer ]
        [ HH.text "delete layer" ]
    , stringInput'
        { name: "fill-paint"
        , value: fill.paint
        , onChange: \x -> _ { fill { paint = x } }
        }
    , numberInput'
        { name: "fill-opacity"
        , value: fill.opacity
        , onChange: \x -> _ { fill { opacity = clamp 0.0 1.0 x } }
        }
    , selectInput'
        { name: "fill-rule"
        , value: fill.rule
        , xs: [ NonZero, EvenOdd ]
        , print: show
        , onChange: \x -> _ { fill { rule = x } }
        }
    , stringInput'
        { name: "stroke-paint"
        , value: stroke.paint
        , onChange: \x -> _ { stroke { paint = x } }
        }
    , numberInput'
        { name: "stroke-opacity"
        , value: stroke.opacity
        , onChange: \x -> _ { stroke { opacity = clamp 0.0 1.0 x } }
        }
    , numberInput'
        { name: "stroke-width"
        , value: stroke.width
        , onChange: \x -> _ { stroke { width = x } }
        }
    , numberInput'
        { name: "dash-offset"
        , value: stroke.dashOffset
        , onChange: \x -> _ { stroke { dashOffset = x } }
        }
    , stringInput'
        { name: "dash-array"
        , value: stroke.dashArray
        , onChange: \x -> _ { stroke { dashArray = x } }
        }
    , selectInput'
        { name: "line-cap"
        , value: stroke.lineCap
        , xs: [ LineCapButt, LineCapSquare, LineCapRound ]
        , print: printStrokeLineCap
        , onChange: \x -> _ { stroke { lineCap = x } }
        }
    , selectInput'
        { name: "line-join"
        , value: stroke.lineJoin
        , xs: [ LineJoinArcs, LineJoinBevel, LineJoinMiter, LineJoinMiterClip, LineJoinRound ]
        , print: printStrokeLineJoin
        , onChange: \x -> _ { stroke { lineJoin = x } }
        }
    , numberInput'
        { name: "miter-limit"
        , value: stroke.miterLimit
        , onChange: \x -> _ { stroke { miterLimit = x } }
        }
    , HH.hr_
    , drawPath { editCommand: actions.editCommand } drawPath'
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
    forall b x.
    Eq x =>
    { name :: String
    , value :: x
    , xs :: Array x
    , print :: x -> String
    , onChange :: x -> Layer -> Layer
    } ->
    HH.HTML b a
  selectInput' { name, value, xs, print, onChange } =
    input' name
      $ select ("layer-info." <> name) xs print value (actions.editLayer <<< onChange)

  input' :: forall a b. String -> HH.HTML a b -> HH.HTML a b
  input' name input =
    HH.dl
      [ HP.class_ $ HH.ClassName "style-input" ]
      [ HH.dt_ [ HH.label [ HP.for $ "layer-info." <> name ] [ HH.text name ] ]
      , HH.dd_ [ input ]
      ]
