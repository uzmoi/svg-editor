module SvgEditor.View.Root (appRoot) where

import Prelude
import Data.Array (filter, find, snoc)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
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
  | SelectLayer Int
  | EditLayerName Int String

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
    , selectedLayer: -1
    }

  render { canvas, layers, selectedLayer } =
    HH.div_
      [ HSE.svg (canvasProps canvas) $ map layer layers
      , HH.ul_
          $ map
              ( \{ id, name } ->
                  HH.li_
                    [ HH.div
                        [ HE.onClick $ \_ -> SelectLayer id
                        , HP.class_ $ HH.ClassName if id == selectedLayer then "selected" else ""
                        ]
                        [ HH.text name ]
                    ]
              )
              layers
      , HH.button
          [ HE.onClick $ \_ -> AddLayer ]
          [ HH.text "add layer" ]
      , case find (\layer -> layer.id == selectedLayer) layers of
          Just { id, name } ->
            HH.div_
              [ HH.input
                  [ HP.value name
                  , HE.onValueInput $ EditLayerName id
                  ]
              , HH.button
                  [ HE.onClick $ \_ -> DeleteLayer id ]
                  [ HH.text "delete layer" ]
              ]
          Nothing -> HH.div_ []
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
    SelectLayer id ->
      H.modify_ \state ->
        state { selectedLayer = if state.selectedLayer == id then -1 else id }
    EditLayerName id name ->
      H.modify_ \state ->
        state
          { layers =
            map
              ( \layer ->
                  if layer.id == id then layer { name = name } else layer
              )
              state.layers
          }
