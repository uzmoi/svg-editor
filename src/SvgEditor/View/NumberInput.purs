module SvgEditor.View.NumberInput
  ( numberInput
  ) where

import Prelude
import Data.Number (fromString)
import Data.Maybe (isNothing)
import Effect.Aff (Aff)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import SvgEditor.View.InputControl (inputControl, inputControlActions, Slot)

numberInput :: forall a. String -> Number -> (Number -> a) -> HH.ComponentHTML a (Slot Number) Aff
numberInput key value onChange =
  inputControl key
    { value: value
    , format: show
    , parse: fromString
    , render
    }
    onChange
  where
  render string =
    HH.input
      [ HP.id key
      , HP.value string
      , HE.onValueInput inputControlActions.change
      , HE.onFocus \_ -> inputControlActions.focus
      , HE.onBlur \_ -> inputControlActions.blur
      , HP.class_ $ HH.ClassName "input"
      , ARIA.invalid $ show $ fromString string # isNothing
      ]
