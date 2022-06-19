module SvgEditor.View.Root (appRoot) where

import Prelude
import Data.Tuple (Tuple(..))
import Data.Array (filter, find, updateAt, snoc)
import Data.Maybe (Maybe(..), maybe)
import Data.Int (toNumber)
import Data.Number (fromString)
import Data.Number.Format (toStringWith, fixed)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.HTML.HTMLElement (toElement)
import Web.DOM.Element (getBoundingClientRect)
import Web.UIEvent.MouseEvent (MouseEvent, clientX, clientY)
import Effect.Aff (Aff)
import Effect.Random (randomInt)
import SvgEditor.Layer (Layer, defaultFill, defaultStroke)
import SvgEditor.PathCommand (PathCommand(..), Pos(..), Vec2)
import SvgEditor.View.Canvas (svgCanvas, canvasContainerRef)

data Action
  = AddLayer
  | DeleteLayer
  | EditLayer Int (Layer -> Layer)
  | SelectLayer Int
  | EditSelectedLayer (Layer -> Layer)
  | DragStart Int (Vec2 -> PathCommand)
  | Drag MouseEvent
  | DragEnd
  | NOOP

toFixed :: Number -> String
toFixed = toStringWith $ fixed 1

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
    , cursorPos: { x: 0.0, y: 0.0 }
    , dragging: Nothing
    }

  render { canvas, layers, selectedLayer, cursorPos } =
    HH.div [ HE.onMouseUp \_ -> DragEnd ]
      [ svgCanvas Drag DragStart canvas layers selectedLayer
      , HH.p_
          [ HH.text $ toFixed cursorPos.x
          , HH.text ", "
          , HH.text $ toFixed cursorPos.y
          ]
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
                  [ Move Abs { x: 0.0, y: 0.0 }
                  , Line Abs { x: 100.0, y: 100.0 }
                  , Close
                  ]
              , fill: defaultFill
              , stroke: defaultStroke
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
    DragStart i j -> H.modify_ _ { dragging = Just $ Tuple i j }
    DragEnd -> H.modify_ _ { dragging = Nothing }
    Drag e ->
      H.getHTMLElementRef canvasContainerRef
        >>= case _ of
            Just canvasContainerEl -> do
              canvasContainerRect <- H.liftEffect $ getBoundingClientRect $ toElement canvasContainerEl
              { canvas: { viewBox }, layers, selectedLayer, dragging } <- H.get
              let
                offsetX = (e # clientX # toNumber) - canvasContainerRect.left

                offsetY = (e # clientY # toNumber) - canvasContainerRect.top

                cursorPos =
                  { x: offsetX * (viewBox.right - viewBox.left) / canvasContainerRect.width
                  , y: offsetY * (viewBox.bottom - viewBox.top) / canvasContainerRect.height
                  }
              H.modify_ _ { cursorPos = cursorPos }
              handleAction
                $ maybe NOOP EditSelectedLayer do
                    Tuple i j <- dragging
                    layer <- layers # find (_.id >>> (==) selectedLayer)
                    drawPath <- layer.drawPath # updateAt i (j cursorPos)
                    Just _ { drawPath = drawPath }
            Nothing -> pure unit
    NOOP -> pure unit
