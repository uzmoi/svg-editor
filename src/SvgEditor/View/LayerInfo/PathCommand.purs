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
import SvgEditor.PathCommand (commandName, points)
import SvgEditor.PathCommandBlock (PathCommandBlock)
import SvgEditor.View.NumberInput (numberInput)
import SvgEditor.View.InputControl (Slot)

pathCommandInfo ::
  forall a.
  { editCommand :: PathCommandBlock -> a } ->
  Array PathCommandBlock ->
  HH.ComponentHTML a (Slot Number) Aff
pathCommandInfo actions pathCommands =
  HH.ul
    [ HP.class_ $ HH.ClassName "draw-path-commands" ]
    $ pathCommands
    # map \cblock ->
        HH.li_
          [ HH.text $ commandName cblock.command
          , HH.div_ $ points cblock.command
              # mapWithIndex \i (Tuple (Vec2 v) updateV) ->
                  let
                    key = "draw-path." <> cblock.id <> "." <> show i

                    handleEditVec f x = actions.editCommand cblock { command = updateV $ f x }
                  in
                    HH.div_
                      [ numberInput (key <> ".x") v.x $ handleEditVec \x -> Vec2 v { x = x }
                      , numberInput (key <> ".y") v.y $ handleEditVec \y -> Vec2 v { y = y }
                      ]
          ]
