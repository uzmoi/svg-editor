module Main (main) where

import Prelude
import Control.Monad.Error.Class (throwError)
import Effect (Effect)
import Effect.Exception (error)
import Data.Maybe (maybe)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)
import Effect.Aff (Aff)
import Web.DOM.ParentNode (QuerySelector(..))
import Web.HTML.HTMLElement (HTMLElement)

awaitSelect :: QuerySelector -> Aff HTMLElement
awaitSelect querySelector@(QuerySelector str) = do
  HA.awaitLoad
  element <- HA.selectElement $ querySelector
  maybe (throwError $ error $ "Could not find '" <> str <> "'") pure element

main :: Effect Unit
main =
  HA.runHalogenAff do
    appRoot <- awaitSelect $ QuerySelector "#app"
    runUI component unit appRoot

type Params
  = Unit

data Action
  = Increment
  | Decrement

component :: forall query message. H.Component query Params message Aff
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  initialState _ = { count: 0 }

  render { count } =
    HH.div_
      [ HH.button [ HE.onClick \_ -> Decrement ] [ HH.text "-" ]
      , HH.div_ [ HH.text $ show count ]
      , HH.button [ HE.onClick \_ -> Increment ] [ HH.text "+" ]
      ]

  handleAction = case _ of
    Increment -> H.modify_ \{ count } -> { count: count + 1 }
    Decrement -> H.modify_ \{ count } -> { count: count - 1 }
