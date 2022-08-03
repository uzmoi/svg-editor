module SvgEditor.View.LayerInfo where

import Prelude
import Effect.Aff (Aff)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import SvgEditor.Layer (Layer)
import SvgEditor.PathCommand (PathCommand)
import SvgEditor.View.DrawPath (drawPath)
import SvgEditor.View.LayerStyles (layerStyles)
import SvgEditor.View.InputControl (Slot)

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
    , layerStyles actions fill stroke
    , HH.hr_
    , HH.section_
        [ HH.h3_ [ HH.text "path commands" ]
        , drawPath { editCommand: actions.editCommand } drawPath'
        ]
    ]
