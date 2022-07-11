module SvgEditor.View.Root (appRoot) where

import Prelude
import Data.Tuple (Tuple(..))
import Data.Array (filter, find, insertAt, updateAt, head)
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
import Web.UIEvent.WheelEvent (WheelEvent, toMouseEvent, deltaY)
import Web.File.File (File, toBlob)
import Web.File.Url (createObjectURL, revokeObjectURL)
import Effect.Aff (Aff)
import Effect.Random (randomInt)
import SvgEditor.Vec (Vec2(..), vec2)
import SvgEditor.Layer (Layer, defaultFill, defaultStroke)
import SvgEditor.PathCommand (PathCommand(..), PathCommandType(..), Pos(..), pathCommand)
import SvgEditor.View.NumberInput (numberInput)
import SvgEditor.View.Canvas (RefImage, svgCanvas, canvasContainerRef)
import SvgEditor.View.LayerList (layerList)
import SvgEditor.View.LayerInfo (layerInfo)

data Action
  = Scale WheelEvent
  | SetRefImage File
  | ModifyRefImage (RefImage -> RefImage)
  | AddLayer
  | DeleteLayer
  | EditLayer Int (Layer -> Layer)
  | SelectLayer Int
  | EditSelectedLayer (Layer -> Layer)
  | SelectCommand PathCommandType
  | AddCommand Int
  | EditCommand Int PathCommand
  | TranslateStart MouseEvent
  | DragStart Int (Vec2 Number -> PathCommand)
  | Drag MouseEvent
  | DragEnd
  | NOOP

toFixed :: Number -> String
toFixed = toStringWith $ fixed 1

radio :: forall a b x. Eq x => String -> Array x -> (x -> String) -> x -> (x -> b) -> Array (HH.HTML a b)
radio id xs print value f =
  xs
    # map \x ->
        HH.label_
          [ HH.input
              [ HP.type_ HP.InputRadio
              , HP.name id
              , HP.value $ print x
              , HP.checked $ x == value
              , HE.onValueChange \_ -> f x
              ]
          , HH.span
              (if x == value then [ HP.class_ $ HH.ClassName "radio-selected" ] else [])
              [ HH.text $ print x ]
          ]

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
    , translate: zero
    , refImage:
        { uri: ""
        , translate: zero
        , scale: 1.0
        , opacity: 0.5
        , show: true
        }
    , layers: []
    , selectedLayer: -1
    , cursorPos: zero
    , command: L
    , dragging: Nothing
    , translating: Nothing
    }

  render state =
    HH.div
      [ HE.onMouseUp \_ -> DragEnd, HP.class_ $ HH.ClassName "root" ]
      [ HH.div [ HP.class_ $ HH.ClassName "header" ]
          [ HH.h1_ [ HH.text "Svg editor" ]
          , HH.div [ HP.class_ $ HH.ClassName "menu-bar" ]
              [ HH.div_
                  $ radio "path-command-type" [ M, L, C, S, Q, T, Z ] show state.command SelectCommand
              , HH.p_
                  [ HH.text $ show $ state.scale * 10
                  , HH.text "%"
                  ]
              , HH.p_
                  $ state.cursorPos
                  # vec2 \x y ->
                      [ HH.text $ toFixed x
                      , HH.text ", "
                      , HH.text $ toFixed y
                      ]
              , HH.input
                  [ HP.type_ HP.InputFile
                  , HE.onFileUpload $ head >>> maybe NOOP SetRefImage
                  ]
              , HH.div_
                  $ state.refImage.translate
                  # vec2 \x y ->
                      [ numberInput "ref-image.x" x \x ->
                          ModifyRefImage \refimg ->
                            refimg { translate = refimg.translate # vec2 \_ y -> Vec2 { x, y } }
                      , numberInput "ref-image.y" y \y ->
                          ModifyRefImage \refimg ->
                            refimg { translate = refimg.translate # vec2 \x _ -> Vec2 { x, y } }
                      , numberInput "ref-image.scale"
                          state.refImage.scale \scale -> ModifyRefImage _ { scale = scale }
                      , numberInput "ref-image.opacity"
                          state.refImage.opacity \opacity ->
                          ModifyRefImage _ { opacity = clamp 0.0 1.0 opacity }
                      , HH.button
                          [ HE.onClick \_ -> ModifyRefImage _ { show = not state.refImage.show } ]
                          [ HH.text if state.refImage.show then "hide" else "show" ]
                      ]
              ]
          ]
      , HH.div [ HP.class_ $ HH.ClassName "main" ]
          [ HH.div
              [ HP.class_ $ HH.ClassName "center-panel"
              , HE.onWheel Scale
              , HE.onMouseMove Drag
              , HE.onMouseDown TranslateStart
              ]
              [ svgCanvas
                  { dragStart: DragStart
                  , addCommand: AddCommand
                  }
                  (toNumber state.scale / 10.0)
                  state
              ]
          , HH.div
              [ HP.class_ $ HH.ClassName "right-panel" ]
              [ layerList
                  { addLayer: AddLayer
                  , selectLayer: SelectLayer
                  , editLayer: EditLayer
                  }
                  state.layers
                  state.selectedLayer
              , state.layers # find (_.id >>> (==) state.selectedLayer)
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

  clientPos e = Vec2 { x: e # clientX, y: e # clientY }

  handleAction = case _ of
    Scale e -> do
      { scale, translate } <- H.get
      H.getHTMLElementRef canvasContainerRef
        >>= maybe (pure unit) \canvasContainerEl -> do
            canvasRect <- H.liftEffect $ getBoundingClientRect $ toElement canvasContainerEl
            let
              offset =
                (toNumber <$> (clientPos $ e # toMouseEvent))
                  - Vec2 { x: canvasRect.left, y: canvasRect.top }

              newScale = clamp 1 100 $ scale - (floor $ deltaY e / 100.0)

              deltaTranslate = offset - ((*) scaleRate <$> offset)
                where
                scaleRate = (newScale # toNumber) / (scale # toNumber)
            H.modify_ _ { scale = newScale, translate = translate + deltaTranslate }
    SetRefImage file -> do
      { refImage } <- H.get
      refImage <-
        H.liftEffect
          $ revokeObjectURL refImage.uri
          *> createObjectURL (file # toBlob)
      H.modify_ _ { refImage { uri = refImage } }
    ModifyRefImage f -> H.modify_ \state -> state { refImage = f state.refImage }
    AddLayer -> do
      id <- H.liftEffect $ randomInt 0 0x10000000
      let
        newLayer =
          { id
          , name: "Layer"
          , show: true
          , drawPath:
              [ Move Abs $ Vec2 { x: 0.0, y: 0.0 }
              , Line Abs $ Vec2 { x: 100.0, y: 100.0 }
              , Close
              ]
          , fill: defaultFill
          , stroke: defaultStroke
          }
      H.modify_ \state -> state { layers = state.layers <> [ newLayer ] }
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
    SelectCommand commandType -> do
      H.modify_ _ { command = commandType }
    AddCommand i -> do
      { layers, selectedLayer, cursorPos, command } <- H.get
      handleAction
        $ maybe NOOP EditSelectedLayer do
            layer <- layers # find (_.id >>> (==) selectedLayer)
            drawPath <- layer.drawPath # insertAt i (pathCommand command cursorPos)
            Just _ { drawPath = drawPath }
    EditCommand i j -> do
      { layers, selectedLayer } <- H.get
      handleAction
        $ maybe NOOP EditSelectedLayer do
            layer <- layers # find (_.id >>> (==) selectedLayer)
            drawPath <- layer.drawPath # updateAt i j
            Just _ { drawPath = drawPath }
    TranslateStart e -> case e # button of
      1 -> do
        { translate } <- H.get
        H.modify_ _ { translating = Just $ translate - (toNumber <$> clientPos e) }
      _ -> pure unit
    DragStart i j -> H.modify_ _ { dragging = Just $ Tuple i j }
    DragEnd -> H.modify_ _ { dragging = Nothing, translating = Nothing }
    Drag e -> do
      { translating } <- H.get
      translating
        # maybe (pure unit) \startTranslate ->
            H.modify_ _ { translate = startTranslate + (toNumber <$> clientPos e) }
      H.getHTMLElementRef canvasContainerRef
        >>= maybe (pure unit) \canvasContainerEl -> do
            canvasRect <- H.liftEffect $ getBoundingClientRect $ toElement canvasContainerEl
            { canvas: { viewBox }, dragging } <- H.get
            let
              offset = (toNumber <$> clientPos e) - Vec2 { x: canvasRect.left, y: canvasRect.top }

              canvasRate =
                Vec2
                  { x: (viewBox.right - viewBox.left) / canvasRect.width
                  , y: (viewBox.bottom - viewBox.top) / canvasRect.height
                  }
            { cursorPos } <- H.modify _ { cursorPos = offset * canvasRate }
            handleAction $ dragging # maybe NOOP \(Tuple i j) -> EditCommand i $ j cursorPos
    NOOP -> pure unit
