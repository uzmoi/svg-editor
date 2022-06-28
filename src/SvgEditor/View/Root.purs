module SvgEditor.View.Root (appRoot) where

import Prelude
import Data.Tuple (Tuple(..))
import Data.Array (filter, find, insertAt, updateAt, snoc)
import Data.Maybe (Maybe(..), maybe)
import Data.Int (toNumber, floor)
import Data.Number.Format (toStringWith, fixed)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.HTML.HTMLElement (toElement)
import Web.DOM.Element (getBoundingClientRect)
import Web.UIEvent.MouseEvent (MouseEvent, clientX, clientY, button)
import Web.UIEvent.WheelEvent (deltaY)
import Effect.Aff (Aff)
import Effect.Random (randomInt)
import SvgEditor.Layer (Layer, defaultFill, defaultStroke)
import SvgEditor.PathCommand (PathCommand(..), Pos(..), Vec2)
import SvgEditor.View.Canvas (svgCanvas, canvasContainerRef)
import SvgEditor.View.LayerList (layerList)
import SvgEditor.View.LayerInfo (layerInfo)

data Action
  = Scale Number
  | AddLayer
  | DeleteLayer
  | EditLayer Int (Layer -> Layer)
  | SelectLayer Int
  | EditSelectedLayer (Layer -> Layer)
  | AddCommand Int
  | EditCommand Int PathCommand
  | TranslateStart MouseEvent
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
    , scale: 10
    , translate: { x: 0.0, y: 0.0 }
    , layers: []
    , selectedLayer: -1
    , cursorPos: { x: 0.0, y: 0.0 }
    , dragging: Nothing
    , translating: Nothing
    }

  render { canvas, scale, translate, layers, selectedLayer, cursorPos } =
    HH.div
      [ HE.onMouseUp \_ -> DragEnd, HP.class_ $ HH.ClassName "root" ]
      [ HH.div [ HP.class_ $ HH.ClassName "header" ]
          [ HH.text "Svg editor"
          ]
      , HH.div [ HP.class_ $ HH.ClassName "main" ]
          [ HH.div
              [ HP.class_ $ HH.ClassName "center-panel"
              , HE.onWheel \e -> Scale $ e # deltaY
              , HE.onMouseMove Drag
              , HE.onMouseDown TranslateStart
              ]
              [ svgCanvas
                  { dragStart: DragStart
                  , addCommand: AddCommand
                  }
                  (toNumber scale / 10.0)
                  translate
                  canvas
                  layers
                  selectedLayer
              , HH.div [ HP.class_ $ HH.ClassName "center-panel-footer" ]
                  [ HH.p_
                      [ HH.text $ show $ scale * 10
                      , HH.text "%"
                      ]
                  , HH.p_
                      [ HH.text $ toFixed cursorPos.x
                      , HH.text ", "
                      , HH.text $ toFixed cursorPos.y
                      ]
                  , HH.p_
                      [ HH.text $ toFixed translate.x
                      , HH.text ", "
                      , HH.text $ toFixed translate.y
                      ]
                  ]
              ]
          , HH.div
              [ HP.class_ $ HH.ClassName "right-panel" ]
              [ layerList
                  { addLayer: AddLayer
                  , selectLayer: SelectLayer
                  , editLayer: EditLayer
                  }
                  layers
                  selectedLayer
              , layers # find (_.id >>> (==) selectedLayer)
                  # ( maybe (HH.div_ [])
                        $ layerInfo
                            { editLayer: EditSelectedLayer
                            , deleteLayer: DeleteLayer
                            , editCommand: EditCommand
                            }
                    )
              ]
          ]
      ]

  handleAction = case _ of
    Scale deltaY -> do
      let
        scale = floor $ deltaY / 100.0
      H.modify_ \state ->
        state
          { scale = clamp 1 100 $ state.scale - scale
          }
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
    AddCommand i -> do
      { layers, selectedLayer, cursorPos } <- H.get
      let
        point = Line Abs
      handleAction
        $ maybe NOOP EditSelectedLayer do
            layer <- layers # find (_.id >>> (==) selectedLayer)
            drawPath <- layer.drawPath # insertAt i (point cursorPos)
            Just _ { drawPath = drawPath }
    EditCommand i j -> do
      { layers, selectedLayer } <- H.get
      handleAction
        $ maybe NOOP EditSelectedLayer do
            layer <- layers # find (_.id >>> (==) selectedLayer)
            drawPath <- layer.drawPath # updateAt i j
            Just _ { drawPath = drawPath }
    TranslateStart e ->
      if (e # button) == 1 then do
        let
          x = e # clientX # toNumber

          y = e # clientY # toNumber
        { translate } <- H.get
        H.modify_ _ { translating = Just $ Tuple { x, y } translate }
      else
        pure unit
    DragStart i j -> H.modify_ _ { dragging = Just $ Tuple i j }
    DragEnd -> H.modify_ _ { dragging = Nothing, translating = Nothing }
    Drag e -> do
      { translating } <- H.get
      translating
        # maybe (pure unit) \(Tuple startPos startTranslate) ->
            H.modify_
              _
                { translate =
                  { x: startTranslate.x + (e # clientX # toNumber) - startPos.x
                  , y: startTranslate.y + (e # clientY # toNumber) - startPos.y
                  }
                }
      H.getHTMLElementRef canvasContainerRef
        >>= case _ of
            Just canvasContainerEl -> do
              canvasContainerRect <- H.liftEffect $ getBoundingClientRect $ toElement canvasContainerEl
              { canvas: { viewBox }, dragging } <- H.get
              let
                offsetX = (e # clientX # toNumber) - canvasContainerRect.left

                offsetY = (e # clientY # toNumber) - canvasContainerRect.top

                cursorPos =
                  { x: offsetX * (viewBox.right - viewBox.left) / canvasContainerRect.width
                  , y: offsetY * (viewBox.bottom - viewBox.top) / canvasContainerRect.height
                  }
              H.modify_ _ { cursorPos = cursorPos }
              handleAction $ dragging # maybe NOOP \(Tuple i j) -> EditCommand i $ j cursorPos
            Nothing -> pure unit
    NOOP -> pure unit
