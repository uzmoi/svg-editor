module SvgEditor.View.Radio where

import Prelude
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

radio :: forall a b x. Eq x => String -> Array x -> (x -> String) -> x -> (x -> b) -> Array (HH.HTML a b)
radio id xs print value f =
  xs
    # map \x ->
        HH.label_
          [ HH.input
              [ HP.type_ HP.InputRadio
              , HP.name id
              , HP.value $ print x
              , HP.checked $ x == value
              , HE.onValueChange \_ -> f x
              ]
          , HH.span
              (if x == value then [ HP.class_ $ HH.ClassName "radio-selected" ] else [])
              [ HH.text $ print x ]
          ]
