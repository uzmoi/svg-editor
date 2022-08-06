module SvgEditor.View.NumberInput
  ( numberInput
  ) where

import Prelude
import Data.Number (fromString)
import Data.Maybe (maybe, isNothing)
import Effect.Aff (Aff)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import Web.Event.Event (preventDefault)
import Web.UIEvent.WheelEvent (toEvent, toMouseEvent, deltaY)
import Web.UIEvent.MouseEvent (ctrlKey)
import SvgEditor.View.InputControl (inputControl, inputControlActions, Slot)

numberInput :: forall a. String -> Number -> (Number -> a) -> HH.ComponentHTML a (Slot Number) Aff
numberInput key value =
  inputControl key
    { value: value
    , format: show
    , parse: fromString
    , render
    }
  where
  render focus string =
    HH.input
      [ HP.id key
      , HP.value string
      , HE.onValueInput inputControlActions.change
      , HE.onFocus \_ -> inputControlActions.focus
      , HE.onBlur \_ -> inputControlActions.blur
      , HE.onWheel \e ->
          if focus then
            let
              dx = deltaY e / if e # toMouseEvent # ctrlKey then 1000.0 else 100.0
            in
              inputControlActions.effect (preventDefault $ e # toEvent)
                <> ( inputControlActions.change
                      $ fromString string
                      # maybe string \x -> show $ x - dx
                  )
          else
            inputControlActions.effect $ pure unit
      , HP.class_ $ HH.ClassName "input"
      , ARIA.invalid $ show $ fromString string # isNothing
      ]
