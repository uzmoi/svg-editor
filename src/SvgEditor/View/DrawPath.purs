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
  HH.div_
    [ HH.ul_ $ pathCommands
        # mapWithIndex \i pathCommand ->
            HH.li_
              [ HH.text $ commandName pathCommand
              , HH.div_ $ points pathCommand
                  # map \(Tuple v updateV) -> point i v updateV
              ]
    ]
  where
  point i v updateV =
    HH.div_
      [ HH.input
          [ HP.value $ show v.x
          , HE.onValueInput $ handleEditVec \x -> v { x = x }
          ]
      , HH.input
          [ HP.value $ show v.y
          , HE.onValueInput $ handleEditVec \y -> v { y = y }
          ]
      ]
    where
    handleEditVec f = fromString >>> maybe actions.noop (actions.editCommand i <<< updateV <<< f)
