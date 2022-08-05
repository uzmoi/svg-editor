module SvgEditor.View.LayerInfo.PathCommand
  ( pathCommandInfo
  ) where

import Prelude
import Data.Tuple (Tuple(..))
import Data.Array (mapWithIndex)
import Effect.Aff (Aff)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import SvgEditor.Vec (Vec2(..))
import SvgEditor.PathCommand (PathCommand, commandName, points)
import SvgEditor.View.NumberInput (numberInput)
import SvgEditor.View.InputControl (Slot)

pathCommandInfo ::
  forall a.
  { editCommand :: Int -> PathCommand -> a } ->
  Array PathCommand -> HH.ComponentHTML a (Slot Number) Aff
pathCommandInfo actions pathCommands =
  HH.ul
    [ HP.class_ $ HH.ClassName "draw-path-commands" ]
    $ pathCommands
    # mapWithIndex \i pathCommand ->
        HH.li_
          [ HH.text $ commandName pathCommand
          , HH.div_ $ points pathCommand
              # mapWithIndex \j (Tuple (Vec2 v) updateV) ->
                  let
                    key = "draw-path." <> show i <> "." <> show j

                    handleEditVec f = actions.editCommand i <<< updateV <<< f
                  in
                    HH.div_
                      [ numberInput (key <> ".x") v.x $ handleEditVec \x -> Vec2 v { x = x }
                      , numberInput (key <> ".y") v.y $ handleEditVec \y -> Vec2 v { y = y }
                      ]
          ]
