module SvgEditor.View.NumberInput
  ( Slot
  , numberInput
  ) where

import Prelude
import Data.Number (fromString)
import Data.Maybe (Maybe(..), maybe)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))

type Slot
  = ( numberInput :: forall query. H.Slot query Number String )

data Action
  = Focus
  | Blur
  | Input String
  | Receive Number

_numberInput = Proxy :: Proxy "numberInput"

numberInput :: forall a. String -> Number -> (Number -> a) -> HH.ComponentHTML a Slot Aff
numberInput key = HH.slot _numberInput key numberInputComponent

numberInputComponent :: forall query. H.Component query Number Number Aff
numberInputComponent =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , receive = Just <<< Receive
              }
    }
  where
  initialState x = { value: show x, inputValue: x, focus: false }

  render { value } =
    HH.input
      [ HP.value value
      , HE.onValueInput Input
      , HE.onFocus \_ -> Focus
      , HE.onBlur \_ -> Blur
      , HP.class_ $ HH.ClassName "number-input"
      ]

  handleAction = case _ of
    Focus -> H.modify_ _ { focus = true }
    Blur -> H.modify_ \state -> state { focus = false, value = show state.inputValue }
    Input value -> do
      H.modify_ _ { value = value }
      fromString value # maybe (pure unit) H.raise
    Receive x ->
      H.modify_ \state@{ focus } ->
        if focus then
          state { inputValue = x }
        else
          state { inputValue = x, value = show x }
