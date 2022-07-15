module SvgEditor.View.Root (appRoot) where

import Prelude
import Data.Tuple (Tuple(..))
import Data.List (List(..), uncons, null, (:))
import Data.Array (filter, find, insertAt, updateAt, head)
import Data.Maybe (Maybe(..), maybe)
import Data.Int (toNumber, floor)
import Data.Number (pow, sign)
import Data.Number.Format (toStringWith, fixed)
import Data.MediaType (MediaType(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.Event (eventListener)
import Web.HTML (window)
import Web.HTML.Window as Window
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement (toElement)
import Web.DOM.Element (getBoundingClientRect)
import Web.UIEvent.KeyboardEvent (KeyboardEvent, key, ctrlKey, fromEvent)
import Web.UIEvent.KeyboardEvent.EventTypes (keydown)
import Web.UIEvent.MouseEvent (MouseEvent, clientX, clientY, button)
import Web.UIEvent.WheelEvent (WheelEvent, toMouseEvent, deltaY)
import Web.File.File (File, toBlob)
import Web.File.Url (createObjectURL, revokeObjectURL)
import DOM.HTML.Indexed.InputAcceptType (mediaType)
import Effect.Aff (Aff)
import Effect.Random (randomInt)
import SvgEditor.Vec (Vec2(..), vec2)
import SvgEditor.Layer (Layer, layer)
import SvgEditor.PathCommand (PathCommand(..), PathCommandType(..), Pos(..), pathCommand)
import SvgEditor.View.NumberInput (numberInput)
import SvgEditor.View.Canvas (RefImage, svgCanvas, canvasContainerRef)
import SvgEditor.View.LayerList (layerList)
import SvgEditor.View.LayerInfo (layerInfo)

data Action
  = Init
  | KeyDown KeyboardEvent
  | Undo
  | Redo
  | Scale WheelEvent
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

initWithHistory :: forall a. a -> { past :: List a, future :: List a, present :: a }
initWithHistory present = { past: Nil, future: Nil, present }

appRoot :: forall query message. H.Component query Unit message Aff
appRoot =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , initialize = Just Init
              }
    }
  where
  initialState _ =
    { svg:
        initWithHistory
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
    , scale: 100.0
    , translate: zero
    , refImage:
        { uri: ""
        , translate: zero
        , scale: 1.0
        , opacity: 0.5
        , show: true
        }
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
                  [ HH.text $ toFixed state.scale
                  , HH.text "%"
                  ]
              , HH.p_
                  $ state.cursorPos
                  # vec2 \x y ->
                      [ HH.text $ toFixed x
                      , HH.text ", "
                      , HH.text $ toFixed y
                      ]
              , HH.button
                  [ HE.onClick \_ -> Undo
                  , HP.disabled $ null state.svg.past
                  ]
                  [ HH.text "undo" ]
              , HH.button
                  [ HE.onClick \_ -> Redo
                  , HP.disabled $ null state.svg.future
                  ]
                  [ HH.text "redo" ]
              , HH.input
                  [ HP.type_ HP.InputFile
                  , HP.accept $ mediaType $ MediaType "image/*"
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
                  (state.scale / 100.0)
                  state
                  state.svg.present
              ]
          , HH.div
              [ HP.class_ $ HH.ClassName "right-panel" ]
              [ layerList
                  { addLayer: AddLayer
                  , selectLayer: SelectLayer
                  , editLayer: EditLayer
                  }
                  state.svg.present.layers
                  state.selectedLayer
              , state.svg.present.layers # find (_.id >>> (==) state.selectedLayer)
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

  pushHistory f state = { past: state.present : state.past, future: Nil, present: f state.present }

  modifySvg f state = state { svg = f state.svg }

  modifyLayers f = modifySvg $ pushHistory \state -> state { layers = f state.layers }

  handleAction = case _ of
    Init -> do
      document <- H.liftEffect $ Window.document =<< window
      H.subscribe' \_ ->
        eventListener keydown
          (HTMLDocument.toEventTarget document)
          (map KeyDown <<< fromEvent)
      handleAction AddLayer
    KeyDown e -> case key e of
      "z"
        | ctrlKey e -> handleAction Undo
      "y"
        | ctrlKey e -> handleAction Redo
      _ -> pure unit
    Undo ->
      H.modify_
        $ modifySvg \state ->
            uncons state.past
              # maybe state \{ head, tail } ->
                  { past: tail, future: state.present : state.future, present: head }
    Redo ->
      H.modify_
        $ modifySvg \state ->
            uncons state.future
              # maybe state \{ head, tail } ->
                  { past: state.present : state.past, future: tail, present: head }
    Scale e -> do
      { scale, translate } <- H.get
      H.getHTMLElementRef canvasContainerRef
        >>= maybe (pure unit) \canvasContainerEl -> do
            canvasRect <- H.liftEffect $ getBoundingClientRect $ toElement canvasContainerEl
            let
              offset =
                (toNumber <$> (clientPos $ e # toMouseEvent))
                  - Vec2 { x: canvasRect.left, y: canvasRect.top }

              isEnlarge = (sign $ deltaY e) == -1.0

              isNormal = if isEnlarge then scale >= 100.0 else scale > 100.0

              rate scale = if (floor scale `mod` 3 == 0) == (isNormal == isEnlarge) then 4.0 / 3.0 else 3.0 / 2.0

              newScale =
                clamp (100.0 / 96.0) 12800.0
                  if isNormal then
                    scale * (rate scale `pow` (deltaY e / -100.0))
                  else
                    let
                      scaleRate = rate (200.0 / scale) `pow` (deltaY e / -100.0)
                    in
                      -- HACK: `scale * scaleRate`と同じだけど謎に誤差が出てバグるので謎の方法で誤差を抑える
                      200.0 / ((200.0 / scale) / scaleRate)

              deltaTranslate = offset - ((*) scaleRate <$> offset)
                where
                scaleRate = newScale / scale
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
        drawPath =
          [ Move Abs $ Vec2 { x: 0.0, y: 0.0 }
          , Line Abs $ Vec2 { x: 100.0, y: 100.0 }
          , Close
          ]
      H.modify_ $ modifyLayers (_ <> [ layer id drawPath ])
    DeleteLayer -> do
      { selectedLayer } <- H.get
      H.modify_ $ modifyLayers $ filter $ _.id >>> (/=) selectedLayer
    EditLayer id f ->
      H.modify_ $ modifyLayers
        $ map \layer -> if layer.id == id then f layer else layer
    SelectLayer id ->
      H.modify_ \state ->
        state { selectedLayer = if state.selectedLayer == id then -1 else id }
    EditSelectedLayer f -> do
      { selectedLayer } <- H.get
      handleAction $ EditLayer selectedLayer f
    SelectCommand commandType -> do
      H.modify_ _ { command = commandType }
    AddCommand i -> do
      { svg, selectedLayer, cursorPos, command } <- H.get
      handleAction
        $ maybe NOOP EditSelectedLayer do
            layer <- svg.present.layers # find (_.id >>> (==) selectedLayer)
            drawPath <- layer.drawPath # insertAt i (pathCommand command cursorPos)
            Just _ { drawPath = drawPath }
    EditCommand i j -> do
      { svg, selectedLayer } <- H.get
      handleAction
        $ maybe NOOP EditSelectedLayer do
            layer <- svg.present.layers # find (_.id >>> (==) selectedLayer)
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
            { svg, dragging } <- H.get
            let
              offset = (toNumber <$> clientPos e) - Vec2 { x: canvasRect.left, y: canvasRect.top }

              viewBox = svg.present.canvas.viewBox

              canvasRate =
                Vec2
                  { x: (viewBox.right - viewBox.left) / canvasRect.width
                  , y: (viewBox.bottom - viewBox.top) / canvasRect.height
                  }
            { cursorPos } <- H.modify _ { cursorPos = offset * canvasRate }
            handleAction $ dragging # maybe NOOP \(Tuple i j) -> EditCommand i $ j cursorPos
    NOOP -> pure unit
