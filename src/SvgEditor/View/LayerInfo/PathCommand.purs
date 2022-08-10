module SvgEditor.View.LayerInfo.PathCommand
  ( pathCommandInfo
  ) where

import Prelude
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple(..))
import Data.Array (mapWithIndex, findIndex, elem)
import Effect.Aff (Aff)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import SvgEditor.Vec (Vec2(..))
import SvgEditor.PathCommand (commandName, points)
import SvgEditor.PathCommandBlock (PathCommandBlock)
import SvgEditor.View.NumberInput (numberInput)
import SvgEditor.View.InputControl (Slot)

pathCommandInfo ::
  forall a b c.
  { editCommand :: PathCommandBlock -> a
  , addCommand :: Int -> a
  , selectCommand :: String -> a
  | b
  } ->
  Array PathCommandBlock ->
  { commands :: Array String | c } ->
  HH.ComponentHTML a (Slot Number) Aff
pathCommandInfo actions pathCommands selected =
  HH.ul
    [ HP.class_ $ HH.ClassName "draw-path-commands" ]
    $ pathCommands
    # map \cblock ->
        HH.li_
          [ commandInfo actions cblock selected
          , HH.button
              [ HE.onClick \_ ->
                  actions.addCommand $ 1
                    + ( fromMaybe 0 $ pathCommands
                          # findIndex (_.id >>> (==) cblock.id)
                      )
              ]
              [ HH.text "add command" ]
          ]

commandInfo ::
  forall a b c.
  { editCommand :: PathCommandBlock -> a
  , selectCommand :: String -> a
  | b
  } ->
  PathCommandBlock ->
  { commands :: Array String | c } ->
  HH.ComponentHTML a (Slot Number) Aff
commandInfo actions cblock selected =
  HH.div
    [ HP.classes
        $ [ HH.ClassName "command" ]
        <> (if elem cblock.id selected.commands then [ HH.ClassName "selected" ] else [])
    ]
    [ HH.p
        [ HE.onClick \_ -> actions.selectCommand cblock.id ]
        [ HH.text $ commandName cblock.command ]
    , HH.ul
        [ HP.class_ $ HH.ClassName "points" ]
        $ points cblock.command
        # mapWithIndex \i (Tuple (Vec2 v) updateV) ->
            let
              key = "draw-path." <> cblock.id <> "." <> show i

              handleEditVec f x = actions.editCommand cblock { command = updateV $ f x }
            in
              HH.li_
                [ numberInput (key <> ".x") v.x $ handleEditVec \x -> Vec2 v { x = x }
                , numberInput (key <> ".y") v.y $ handleEditVec \y -> Vec2 v { y = y }
                ]
    ]
