module SvgEditor.View.LayerInfo.LayerAttr
  ( layerAttr
  ) where

import Prelude
import Effect.Aff (Aff)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import SvgEditor.Layer (Layer, Attr)
import SvgEditor.View.InputControl (Slot)

layerAttr ::
  forall a b.
  { editLayer :: (Layer -> Layer) -> a | b } ->
  Attr ->
  HH.ComponentHTML a (Slot Number) Aff
layerAttr actions attr =
  HH.div_
    [ stringInput'
        { name: "id"
        , value: attr.id
        , onChange: \x -> _ { attr { id = x } }
        }
    , stringInput'
        { name: "class"
        , value: attr.class
        , onChange: \x -> _ { attr { class = x } }
        }
    ]
  where
  stringInput' { name, value, onChange } =
    input' name
      $ HH.input
          [ HP.id $ "layer-attr." <> name
          , HP.value value
          , HE.onValueInput (actions.editLayer <<< onChange)
          , HP.class_ $ HH.ClassName "input"
          ]

  input' :: forall c d. String -> HH.HTML c d -> HH.HTML c d
  input' name input =
    HH.dl
      [ HP.class_ $ HH.ClassName "style-input" ]
      [ HH.dt_ [ HH.label [ HP.for $ "layer-attr." <> name ] [ HH.text name ] ]
      , HH.dd_ [ input ]
      ]
