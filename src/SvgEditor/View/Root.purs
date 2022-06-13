module SvgEditor.View.Root (appRoot) where

import Prelude
import Data.Array (filter, find, snoc)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Svg.Elements as HSE
import Halogen.Svg.Attributes (Color(..))
import Halogen.Svg.Attributes.StrokeLineCap (StrokeLineCap(..))
import Halogen.Svg.Attributes.StrokeLineJoin (StrokeLineJoin(..))
import Halogen.Svg.Attributes.Path as SP
import Effect.Aff (Aff)
import Effect.Random (randomInt)
import SvgEditor.Layer (FillRule(..))
import SvgEditor.View.Layer (layer)
import SvgEditor.View.Canvas (canvasProps)

data Action
  = AddLayer
  | DeleteLayer
  | SelectLayer Int
  | EditLayerName String

appRoot :: forall query message. H.Component query Unit message Aff
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
      [ HSE.svg (canvasProps canvas) $ layers # map layer
      , HH.ul_ $ layers
          # map \{ id, name } ->
              HH.li_
                [ HH.div
                    [ HE.onClick $ \_ -> SelectLayer id
                    , HP.class_ $ HH.ClassName if id == selectedLayer then "selected" else ""
                    ]
                    [ HH.text name ]
                ]
      , HH.button
          [ HE.onClick $ \_ -> AddLayer ]
          [ HH.text "add layer" ]
      , case layers # find \layer -> layer.id == selectedLayer of
          Just { name } ->
            HH.div_
              [ HH.input
                  [ HP.value name
                  , HE.onValueInput $ EditLayerName
                  ]
              , HH.button
                  [ HE.onClick $ \_ -> DeleteLayer ]
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
              , fill:
                  { color: RGB 0 0 0
                  , opacity: 1.0
                  , rule: NonZero
                  }
              , stroke:
                  { color: RGB 0 0 0
                  , opacity: 1.0
                  , width: 1.0
                  , dashOffset: 0.0
                  , dashArray: ""
                  , lineCap: LineCapButt
                  , lineJoin: LineJoinBevel
                  , miterLimit: 0.0
                  }
              }
          }
    DeleteLayer ->
      H.modify_ \state@{ selectedLayer } ->
        state { layers = state.layers # filter \layer -> layer.id /= selectedLayer }
    SelectLayer id ->
      H.modify_ \state ->
        state { selectedLayer = if state.selectedLayer == id then -1 else id }
    EditLayerName name ->
      H.modify_ \state@{ selectedLayer } ->
        state
          { layers =
            state.layers
              # map \layer ->
                  if layer.id == selectedLayer then layer { name = name } else layer
          }
