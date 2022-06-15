module SvgEditor.View.Root (appRoot) where

import Prelude
import Data.Array (filter, find, snoc)
import Data.Maybe (Maybe(..))
import Data.Number (fromString)
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
import SvgEditor.Layer (Layer, FillRule(..))
import SvgEditor.View.Layer (layer)
import SvgEditor.View.Canvas (canvasProps)

data Action
  = AddLayer
  | DeleteLayer
  | EditLayer Int (Layer -> Layer)
  | SelectLayer Int
  | EditSelectedLayer (Layer -> Layer)
  | NOOP

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
      [ HSE.svg (canvasProps canvas) $ layers # filter _.show # map layer
      , HH.ul_ $ layers
          # map \{ id, name, show } ->
              HH.li_
                [ HH.div
                    [ HP.class_ $ HH.ClassName if id == selectedLayer then "selected" else "" ]
                    [ HH.p
                        [ HE.onClick \_ -> SelectLayer id ]
                        [ HH.text name ]
                    , HH.button
                        [ HE.onClick \_ -> EditLayer id _ { show = not show } ]
                        [ HH.text $ if show then "hide" else "show" ]
                    ]
                ]
      , HH.button
          [ HE.onClick \_ -> AddLayer ]
          [ HH.text "add layer" ]
      , case layers # find (_.id >>> (==) selectedLayer) of
          Just { name, stroke } ->
            HH.div_
              [ HH.input
                  [ HP.value name
                  , HE.onValueInput \value -> EditSelectedLayer _ { name = value }
                  ]
              , HH.button
                  [ HE.onClick \_ -> DeleteLayer ]
                  [ HH.text "delete layer" ]
              , HH.div_
                  [ HH.text "stroke width"
                  , HH.input
                      [ HP.value $ show stroke.width
                      , HE.onValueInput \value -> case fromString value of
                          Just width -> EditSelectedLayer _ { stroke { width = width } }
                          Nothing -> NOOP
                      ]
                  ]
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
              , show: true
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
        state { layers = state.layers # filter (_.id >>> (/=) selectedLayer) }
    EditLayer id f ->
      H.modify_ \state ->
        state { layers = state.layers # map \layer -> if layer.id == id then f layer else layer }
    SelectLayer id ->
      H.modify_ \state ->
        state { selectedLayer = if state.selectedLayer == id then -1 else id }
    EditSelectedLayer f -> do
      { selectedLayer } <- H.get
      handleAction $ EditLayer selectedLayer f
    NOOP -> pure unit
