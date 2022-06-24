module SvgEditor.View.DrawPath
  ( drawPath
  ) where

import Prelude
import Data.Number (fromString)
import Data.Maybe (maybe)
import Data.Tuple (Tuple(..))
import Data.Array (mapWithIndex)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import SvgEditor.PathCommand (PathCommand, commandName, points)

drawPath ::
  forall a b.
  { editCommand :: Int -> PathCommand -> b, noop :: b } ->
  Array PathCommand -> HH.HTML a b
drawPath actions pathCommands =
  HH.ul
    [ HP.class_ $ HH.ClassName "draw-path-commands" ]
    $ pathCommands
    # mapWithIndex \i pathCommand ->
        HH.li_
          [ HH.text $ commandName pathCommand
          , HH.div_ $ points pathCommand
              # map \(Tuple v updateV) -> point i v updateV
          ]
  where
  point i v updateV =
    HH.div_
      [ HH.input
          [ HP.value $ show v.x
          , HE.onValueInput $ handleEditVec \x -> v { x = x }
          , HP.class_ $ HH.ClassName "number-input"
          ]
      , HH.input
          [ HP.value $ show v.y
          , HE.onValueInput $ handleEditVec \y -> v { y = y }
          , HP.class_ $ HH.ClassName "number-input"
          ]
      ]
    where
    handleEditVec f = fromString >>> maybe actions.noop (actions.editCommand i <<< updateV <<< f)
