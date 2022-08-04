module SvgEditor.View.Root (appRoot) where

import Prelude
import Data.Array (filter, find, insertAt, updateAt, head)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Int (toNumber, floor)
import Data.Number (sign)
import Data.Number.Format (toStringWith, fixed)
import Data.String (toLower)
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
import Web.DOM.Node (nodeName, fromEventTarget)
import Web.DOM.Element (getBoundingClientRect)
import Web.Event.Event (target)
import Web.UIEvent.KeyboardEvent (KeyboardEvent, key, ctrlKey, fromEvent, toEvent)
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
import SvgEditor.History as History
import SvgEditor.PathCommand (PathCommand(..), PathCommandType(..), Pos(..), pathCommand)
import SvgEditor.View.Radio (radio)
import SvgEditor.View.NumberInput (numberInput)
import SvgEditor.View.Canvas (RefImage, svgCanvas, canvasContainerRef)
import SvgEditor.View.LayerList (layerList)
import SvgEditor.View.LayerInfo (layerInfo, LayerInfoTab(..))

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
  | SelectTab LayerInfoTab
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

data Dragging
  = DraggingCanvas (Vec2 Number)
  | DraggingPoint Int (Vec2 Number -> PathCommand)
  | NotDragging

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
        History.history
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
    , selected: Nothing
    , cursorPos: zero
    , command: L
    , dragging: NotDragging
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
                  , HP.disabled $ not $ History.canUndo state.svg
                  ]
                  [ HH.text "undo" ]
              , HH.button
                  [ HE.onClick \_ -> Redo
                  , HP.disabled $ not $ History.canRedo state.svg
                  ]
                  [ HH.text "redo" ]
              , HH.input
                  [ HP.type_ HP.InputFile
                  , HP.accept $ mediaType $ MediaType "image/*"
                  , HE.onFileUpload $ head >>> maybe NOOP SetRefImage
                  ]
              , HH.div_
                  $ state.refImage.translate
                  # vec2 \tx ty ->
                      [ numberInput "ref-image.x" tx \x ->
                          ModifyRefImage \refimg ->
                            refimg { translate = refimg.translate # vec2 \_ y -> Vec2 { x, y } }
                      , numberInput "ref-image.y" ty \y ->
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
                  $ History.present state.svg
              ]
          , HH.div
              [ HP.class_ $ HH.ClassName "right-panel" ]
              [ layerList
                  { addLayer: AddLayer
                  , selectLayer: SelectLayer
                  , editLayer: EditLayer
                  }
                  (History.present state.svg).layers
                  (state.selected # maybe (-1) _.layerId)
              , fromMaybe (HH.div_ []) do
                  selected <- state.selected
                  layer <- (History.present state.svg).layers # find (_.id >>> (==) selected.layerId)
                  pure
                    $ layerInfo
                        { editLayer: EditSelectedLayer
                        , deleteLayer: DeleteLayer
                        , editCommand: EditCommand
                        , selectTab: SelectTab
                        }
                        layer
                        selected
              ]
          ]
      ]

  clientPos e = Vec2 { x: e # clientX, y: e # clientY }

  modifySvg f state = state { svg = f state.svg }

  modifyLayers f = modifySvg $ History.next \state -> state { layers = f state.layers }

  modifyLayers' f = modifySvg $ History.update \state -> state { layers = f state.layers }

  isFormField node = case nodeName node # toLower of
    "select" -> true
    "textarea" -> true
    "input" -> true
    _ -> false

  scaleChange isEnlarge scale =
    let
      isNormal = if isEnlarge then scale >= 100.0 else scale > 100.0

      -- 拡大率100%以下は逆数にする
      -- ただし66.666...とかが少数でmod 3できないので200
      x = floor if isNormal then scale else 200.0 / scale

      rate =
        if (x `mod` 3 == 0) == (isNormal == isEnlarge) then
          4.0 / 3.0
        else
          3.0 / 2.0

      op = if isEnlarge then (*) else (/)

      newScale = scale `op` rate
    in
      clamp (100.0 / 96.0) 12800.0 newScale

  handleAction = case _ of
    Init -> do
      document <- H.liftEffect $ Window.document =<< window
      H.subscribe' \_ ->
        eventListener keydown
          (HTMLDocument.toEventTarget document)
          (map KeyDown <<< fromEvent)
      -- AddLayer
      id <- H.liftEffect $ randomInt 0 0x10000000
      let
        drawPath =
          [ Move Abs $ Vec2 { x: 0.0, y: 0.0 }
          , Line Abs $ Vec2 { x: 100.0, y: 100.0 }
          , Close
          ]
      H.modify_ $ modifyLayers' (_ <> [ layer id drawPath ])
    KeyDown e ->
      unless (e # toEvent # target >>= fromEventTarget # maybe true isFormField) case key e of
        "z"
          | ctrlKey e -> handleAction Undo
        "y"
          | ctrlKey e -> handleAction Redo
        "m" -> handleAction $ SelectCommand M
        "l" -> handleAction $ SelectCommand L
        "c" -> handleAction $ SelectCommand C
        "s" -> handleAction $ SelectCommand S
        "q" -> handleAction $ SelectCommand Q
        "t" -> handleAction $ SelectCommand T
        -- "a" -> handleAction $ SelectCommand A
        "z" -> handleAction $ SelectCommand Z
        _ -> pure unit
    Undo -> H.modify_ $ modifySvg \svg -> fromMaybe svg $ History.undo svg
    Redo -> H.modify_ $ modifySvg \svg -> fromMaybe svg $ History.redo svg
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

              newScale = scaleChange isEnlarge scale

              deltaTranslate = offset - ((*) scaleRate <$> offset)
                where
                scaleRate = newScale / scale
            H.modify_ _ { scale = newScale, translate = translate + deltaTranslate }
    SetRefImage file -> do
      { refImage } <- H.get
      uri <-
        H.liftEffect
          $ revokeObjectURL refImage.uri
          *> createObjectURL (file # toBlob)
      H.modify_ _ { refImage { uri = uri } }
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
      H.get >>= _.selected
        >>> maybe (pure unit) \selected ->
            H.modify_ $ modifyLayers $ filter $ _.id >>> (/=) selected.layerId
    EditLayer id f ->
      H.modify_ $ modifyLayers'
        $ map \layer -> if layer.id == id then f layer else layer
    SelectLayer id -> do
      let
        f selected = if selected.layerId == id then -1 else id
      selectedLayerId <- H.get # map (_.selected >>> maybe id f)
      H.modify_ _ { selected = Just { layerId: selectedLayerId, tab: PathStylesTab } }
    SelectTab tab ->
      H.get >>= _.selected
        >>> maybe (pure unit) \selected ->
            H.modify_ _ { selected = Just $ selected { tab = tab } }
    EditSelectedLayer f ->
      H.get >>= _.selected
        >>> maybe (pure unit) \selected ->
            handleAction $ EditLayer selected.layerId f
    SelectCommand commandType -> H.modify_ _ { command = commandType }
    AddCommand i -> do
      { svg, selected, cursorPos, command } <- H.get
      handleAction
        $ maybe NOOP EditSelectedLayer do
            selected' <- selected
            layer <- (History.present svg).layers # find (_.id >>> (==) selected'.layerId)
            drawPath <- layer.drawPath # insertAt i (pathCommand command cursorPos)
            Just _ { drawPath = drawPath }
    EditCommand i j -> do
      { svg, selected } <- H.get
      handleAction
        $ maybe NOOP EditSelectedLayer do
            selected' <- selected
            layer <- (History.present svg).layers # find (_.id >>> (==) selected'.layerId)
            drawPath <- layer.drawPath # updateAt i j
            Just _ { drawPath = drawPath }
    TranslateStart e -> case e # button of
      1 -> do
        { translate } <- H.get
        H.modify_ _ { dragging = DraggingCanvas $ translate - (toNumber <$> clientPos e) }
      _ -> pure unit
    DragStart i j -> do
      H.modify_ $ modifyLayers identity
      H.modify_ _ { dragging = DraggingPoint i j }
    DragEnd -> H.modify_ _ { dragging = NotDragging }
    Drag e -> do
      H.getHTMLElementRef canvasContainerRef
        >>= maybe (pure unit) \canvasContainerEl -> do
            canvasRect <- H.liftEffect $ getBoundingClientRect $ toElement canvasContainerEl
            { svg } <- H.get
            let
              offset = (toNumber <$> clientPos e) - Vec2 { x: canvasRect.left, y: canvasRect.top }

              viewBox = (History.present svg).canvas.viewBox

              canvasRate =
                Vec2
                  { x: (viewBox.right - viewBox.left) / canvasRect.width
                  , y: (viewBox.bottom - viewBox.top) / canvasRect.height
                  }
            H.modify_ _ { cursorPos = offset * canvasRate }
      { dragging, cursorPos } <- H.get
      case dragging of
        DraggingCanvas startTranslate ->
          H.modify_
            _ { translate = startTranslate + (toNumber <$> clientPos e) }
        DraggingPoint i j -> handleAction $ EditCommand i $ j cursorPos
        NotDragging -> pure unit
    NOOP -> pure unit
