module SvgEditor.View.LayerList
  ( layerList
  ) where

import Prelude
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import SvgEditor.Layer (Layer)

layerList ::
  forall a b.
  { selectLayer :: Int -> b
  , editLayer :: Int -> (Layer -> Layer) -> b
  , addLayer :: b
  } ->
  Array Layer -> Int -> HH.HTML a b
layerList actions layers selectedLayer =
  HH.div_
    [ HH.ul_ $ layers
        # map \{ id, name, show } ->
            HH.li_
              [ HH.div
                  [ HP.class_ $ HH.ClassName if id == selectedLayer then "selected" else "" ]
                  [ HH.p
                      [ HE.onClick \_ -> actions.selectLayer id ]
                      [ HH.text name ]
                  , HH.button
                      [ HE.onClick \_ -> actions.editLayer id _ { show = not show } ]
                      [ HH.text $ if show then "hide" else "show" ]
                  ]
              ]
    , HH.button
        [ HE.onClick \_ -> actions.addLayer ]
        [ HH.text "add layer" ]
    ]
