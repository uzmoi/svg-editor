module SvgEditor.View.LayerInfo where

import Prelude
import Data.Maybe (maybe)
import Data.Number (fromString)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import SvgEditor.Layer (Layer)
import SvgEditor.PathCommand (PathCommand)
import SvgEditor.View.DrawPath (drawPath)

layerInfo ::
  forall a b.
  { editLayer :: (Layer -> Layer) -> b
  , deleteLayer :: b
  , editCommand :: Int -> PathCommand -> b
  , noop :: b
  } ->
  Layer -> HH.HTML a b
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
        [ HH.text "stroke width"
        , HH.input
            [ HP.value $ show stroke.width
            , HE.onValueInput $ fromString
                >>> maybe actions.noop \width ->
                    actions.editLayer _ { stroke { width = width } }
            , HP.class_ $ HH.ClassName "number-input"
            ]
        ]
    , drawPath { editCommand: actions.editCommand, noop: actions.noop } drawPath'
    ]
