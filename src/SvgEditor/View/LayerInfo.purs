module SvgEditor.View.LayerInfo
  ( LayerInfoTab(..)
  , layerInfo
  ) where

import Prelude
import Effect.Aff (Aff)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import SvgEditor.Layer (Layer)
import SvgEditor.PathCommandBlock (PathCommandBlock)
import SvgEditor.View.LayerInfo.PathCommand (pathCommandInfo)
import SvgEditor.View.LayerInfo.LayerStyles (layerStyles)
import SvgEditor.View.LayerInfo.LayerAttr (layerAttr)
import SvgEditor.View.InputControl (Slot)
import SvgEditor.View.Radio (radio)

data LayerInfoTab
  = PathStylesTab
  | PathCommandsTab
  | AttrTab

derive instance eqLayerInfoTab :: Eq LayerInfoTab

printLayerInfoTab :: LayerInfoTab -> String
printLayerInfoTab = case _ of
  PathStylesTab -> "styles"
  PathCommandsTab -> "commands"
  AttrTab -> "attr"

layerInfo ::
  forall a b.
  { editLayer :: (Layer -> Layer) -> a
  , deleteLayer :: a
  , editCommand :: PathCommandBlock -> a
  , selectTab :: LayerInfoTab -> a
  } ->
  Layer ->
  { tab :: LayerInfoTab | b } ->
  HH.ComponentHTML a (Slot Number) Aff
layerInfo actions layer selected =
  HH.div
    [ HP.class_ $ HH.ClassName "layer-info" ]
    [ HH.div
        [ HP.class_ $ HH.ClassName "layer-profile" ]
        [ HH.input
            [ HP.value layer.name
            , HE.onValueInput \value -> actions.editLayer _ { name = value }
            , HP.class_ $ HH.ClassName "layer-name-input"
            ]
        , HH.button
            [ HE.onClick \_ -> actions.deleteLayer ]
            [ HH.text "delete layer" ]
        ]
    , HH.div_
        $ radio "layer-info-tab"
            [ PathStylesTab, PathCommandsTab, AttrTab ]
            printLayerInfoTab
            selected.tab
            actions.selectTab
    , HH.section
        [ HP.class_ $ HH.ClassName "layer-info-tab-contents" ]
        [ case selected.tab of
            PathStylesTab -> layerStyles actions layer.fill layer.stroke
            PathCommandsTab -> pathCommandInfo { editCommand: actions.editCommand } layer.drawPath
            AttrTab -> layerAttr actions layer.attr
        ]
    ]
