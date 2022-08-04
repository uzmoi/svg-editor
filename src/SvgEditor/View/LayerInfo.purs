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
import SvgEditor.PathCommand (PathCommand)
import SvgEditor.View.DrawPath (drawPath)
import SvgEditor.View.LayerStyles (layerStyles)
import SvgEditor.View.LayerAttr (layerAttr)
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
  , editCommand :: Int -> PathCommand -> a
  , selectTab :: LayerInfoTab -> a
  } ->
  Layer ->
  { tab :: LayerInfoTab | b } ->
  HH.ComponentHTML a (Slot Number) Aff
layerInfo actions { name, drawPath: drawPath', fill, stroke, attr } { tab } =
  HH.div
    [ HP.class_ $ HH.ClassName "layer-info" ]
    [ HH.div [ HP.class_ $ HH.ClassName "layer-profile" ]
        [ HH.input
            [ HP.value name
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
            tab
            actions.selectTab
    , HH.section
        [ HP.class_ $ HH.ClassName "layer-info-tab-contents" ]
        [ case tab of
            PathStylesTab -> layerStyles actions fill stroke
            PathCommandsTab -> drawPath { editCommand: actions.editCommand } drawPath'
            AttrTab -> layerAttr actions attr
        ]
    ]
