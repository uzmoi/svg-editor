module SvgEditor.View.InputControl
  ( Action
  , Props
  , Slot
  , inputControl
  , inputControlActions
  ) where

import Prelude
import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))

type Slot io
  = ( inputControl :: forall query. H.Slot query io String )

type Props io raw
  = { value :: io
    , format :: io -> raw
    , parse :: raw -> Maybe io
    , render :: raw -> HH.ComponentHTML (Action io raw) (Slot io) Aff
    }

inputControlActions ::
  forall io raw.
  { change :: raw -> Action io raw
  , focus :: Action io raw
  , blur :: Action io raw
  , effect :: Effect Unit -> Action io raw
  }
inputControlActions = { change: Change, focus: Focus, blur: Blur, effect: Eff }

data Action io raw
  = Focus
  | Blur
  | Change raw
  | Receive (Props io raw)
  | Eff (Effect Unit)
  | Composed (Action io raw) (Action io raw)

instance name :: Semigroup (Action io raw) where
  append = Composed

_inputControl = Proxy :: Proxy "inputControl"

inputControl ::
  forall action io raw.
  String ->
  Props io raw ->
  (io -> action) ->
  HH.ComponentHTML action (Slot io) Aff
inputControl key = HH.slot _inputControl key inputControlComponent

inputControlComponent :: forall query io raw. H.Component query (Props io raw) io Aff
inputControlComponent =
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
  initialState props =
    let
      x = props.format props.value
    in
      { realValue: x
      , validValue: x
      , focus: false
      , props
      }

  render { props, realValue } = props.render realValue

  handleAction = case _ of
    Focus -> H.modify_ _ { focus = true }
    Blur -> H.modify_ \state -> state { focus = false, realValue = state.validValue }
    Change realValue -> do
      { props } <- H.get
      H.modify_ _ { realValue = realValue }
      props.parse realValue # maybe (pure unit) H.raise
    Receive props ->
      let
        x = props.format props.value
      in
        H.modify_ \state@{ focus } ->
          if focus then
            state { props = props, validValue = x }
          else
            state { props = props, validValue = x, realValue = x }
    Eff eff -> H.liftEffect eff
    Composed action1 action2 -> handleAction action1 <> handleAction action2
