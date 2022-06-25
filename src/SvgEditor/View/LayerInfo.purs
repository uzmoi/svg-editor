module SvgEditor.View.LayerInfo where

import Prelude
import Effect.Aff (Aff)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import SvgEditor.Layer (Layer)
import SvgEditor.PathCommand (PathCommand)
import SvgEditor.View.DrawPath (drawPath)
import SvgEditor.View.NumberInput (numberInput, Slot)

layerInfo ::
  forall a.
  { editLayer :: (Layer -> Layer) -> a
  , deleteLayer :: a
  , editCommand :: Int -> PathCommand -> a
  } ->
  Layer -> HH.ComponentHTML a Slot Aff
layerInfo actions { name, drawPath: drawPath', stroke } =
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
        [ HH.text "stroke-width"
        , numberInput "layer-info.stroke-width" stroke.width \width ->
            actions.editLayer _ { stroke { width = width } }
        ]
    , drawPath { editCommand: actions.editCommand } drawPath'
    ]
