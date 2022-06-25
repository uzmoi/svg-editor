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
import SvgEditor.View.NumberInput (numberInput, Slot)

select :: forall a b x. Array x -> (x -> String) -> (x -> b) -> HH.HTML a b
select xs print f =
  let
    ys = xs # map \x -> Tuple (print x) x
  in
    HH.select
      [ HE.onValueInput \x ->
          f $ snd $ unsafePartial fromJust $ ys # find (fst >>> (==) x)
      ]
      $ ys
      # map \x -> let name = fst x in HH.option [ HP.value name ] [ HH.text name ]

layerInfo ::
  forall a.
  { editLayer :: (Layer -> Layer) -> a
  , deleteLayer :: a
  , editCommand :: Int -> PathCommand -> a
  } ->
  Layer -> HH.ComponentHTML a Slot Aff
layerInfo actions { name, drawPath: drawPath', fill, stroke } =
  HH.div
    [ HP.class_ $ HH.ClassName "layer-info" ]
    [ HH.input
        [ HP.value name
        , HE.onValueInput \value -> actions.editLayer _ { name = value }
        ]
    , HH.button
        [ HE.onClick \_ -> actions.deleteLayer ]
        [ HH.text "delete layer" ]
    , HH.div_
        [ HH.text "fill-color"
        , HH.input
            [ HP.value fill.color
            , HE.onValueInput \x ->
                actions.editLayer _ { fill { color = x } }
            ]
        ]
    , HH.div_
        [ HH.text "fill-opacity"
        , numberInput "layer-info.fill-opacity" fill.opacity \x ->
            actions.editLayer _ { fill { opacity = clamp 0.0 1.0 x } }
        ]
    , HH.div_
        [ HH.text "fill-rule"
        , select [ NonZero, EvenOdd ] show \x ->
            actions.editLayer _ { fill { rule = x } }
        ]
    , HH.div_
        [ HH.text "stroke-color"
        , HH.input
            [ HP.value stroke.color
            , HE.onValueInput \x ->
                actions.editLayer _ { stroke { color = x } }
            ]
        ]
    , HH.div_
        [ HH.text "stroke-opacity"
        , numberInput "layer-info.stroke-opacity" stroke.opacity \x ->
            actions.editLayer _ { stroke { opacity = clamp 0.0 1.0 x } }
        ]
    , HH.div_
        [ HH.text "stroke-width"
        , numberInput "layer-info.stroke-width" stroke.width \x ->
            actions.editLayer _ { stroke { width = x } }
        ]
    , HH.div_
        [ HH.text "dash-offset"
        , numberInput "layer-info.dash-offset" stroke.dashOffset \x ->
            actions.editLayer _ { stroke { dashOffset = x } }
        ]
    , HH.div_
        [ HH.text "dash-array"
        , HH.input
            [ HP.value stroke.dashArray
            , HE.onValueInput \x ->
                actions.editLayer _ { stroke { dashArray = x } }
            ]
        ]
    , HH.div_
        [ HH.text "line-cap"
        , select
            [ LineCapButt, LineCapSquare, LineCapRound ]
            printStrokeLineCap \x ->
            actions.editLayer _ { stroke { lineCap = x } }
        ]
    , HH.div_
        [ HH.text "line-join"
        , select
            [ LineJoinMiter, LineJoinMiterClip, LineJoinArcs, LineJoinBevel, LineJoinRound ]
            printStrokeLineJoin \x ->
            actions.editLayer _ { stroke { lineJoin = x } }
        ]
    , HH.div_
        [ HH.text "miter-limit"
        , numberInput "layer-info.miter-limit" stroke.miterLimit \x ->
            actions.editLayer _ { stroke { miterLimit = x } }
        ]
    , drawPath { editCommand: actions.editCommand } drawPath'
    ]
