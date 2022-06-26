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

type Param
  = { key :: String, x :: Number }

data Action
  = Focus
  | Blur
  | Input String
  | Receive Param

_numberInput = Proxy :: Proxy "numberInput"

numberInput :: forall a. String -> Number -> (Number -> a) -> HH.ComponentHTML a Slot Aff
numberInput key x = HH.slot _numberInput key numberInputComponent { key, x }

numberInputComponent :: forall query. H.Component query Param Number Aff
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
  initialState { key, x } = { id: key, value: show x, inputValue: x, focus: false }

  render { id, value } =
    HH.input
      [ HP.id id
      , HP.value value
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
    Receive { key, x } ->
      H.modify_ \state@{ focus } ->
        if focus then
          state { id = key, inputValue = x }
        else
          state { id = key, inputValue = x, value = show x }
