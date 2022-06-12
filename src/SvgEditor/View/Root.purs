module SvgEditor.View.Root (appRoot) where

import Prelude
import Data.Array (filter, snoc)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Svg.Elements as HSE
import Halogen.Svg.Attributes.Path as SP
import Effect.Aff (Aff)
import Effect.Random (randomInt)
import SvgEditor.View.Layer (layer)
import SvgEditor.View.Canvas (canvasProps)

type Params
  = Unit

data Action
  = AddLayer
  | DeleteLayer Int

appRoot :: forall query message. H.Component query Params message Aff
appRoot =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , initialize = Just AddLayer
              }
    }
  where
  initialState _ =
    { canvas:
        { viewBox:
            { top: 0.0
            , bottom: 100.0
            , left: 0.0
            , right: 100.0
            }
        }
    , layers: []
    }

  render { canvas, layers } =
    HH.div_
      [ HSE.svg (canvasProps canvas) $ map layer layers
      , HH.ul_
          $ map
              ( \layer ->
                  HH.li_
                    [ HH.div_
                        [ HH.text layer.name
                        , HH.button
                            [ HE.onClick $ \_ -> DeleteLayer layer.id ]
                            [ HH.text "delete layer" ]
                        ]
                    ]
              )
              layers
      , HH.button
          [ HE.onClick $ \_ -> AddLayer ]
          [ HH.text "add layer" ]
      ]

  handleAction = case _ of
    AddLayer -> do
      id <- H.liftEffect $ randomInt 0 0x10000000
      H.modify_ \state ->
        state
          { layers =
            snoc state.layers
              { id
              , name: "Layer"
              , drawPath:
                  [ SP.m SP.Abs 0.0 0.0
                  , SP.l SP.Abs 100.0 100.0
                  , SP.z
                  ]
              }
          }
    DeleteLayer id ->
      H.modify_ \state ->
        state
          { layers = filter (\layer -> layer.id /= id) state.layers
          }
