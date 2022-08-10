module SvgEditor.View.Canvas.Overlay
  ( overlayLines
  , overlayPoints
  ) where

import Prelude
import Data.Array (concat, mapWithIndex)
import Data.Tuple (Tuple(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Svg.Elements as HSE
import Halogen.Svg.Attributes as HSA
import Halogen.Svg.Attributes (Color(..))
import SvgEditor.Vec (Vec2(..))
import SvgEditor.PathCommand (PathCommand(..), PathCommandContext, points, startingPoint, pathCommandContext, toHalogenPathCommand, commandName)
import SvgEditor.PathCommandBlock (PathCommandBlock)

overlayPoint ::
  forall a b.
  ((Vec2 Number -> PathCommand) -> b) ->
  Number ->
  Tuple (Vec2 Number) (Vec2 Number -> PathCommand) ->
  HH.HTML a b
overlayPoint f size (Tuple (Vec2 v) updateV) =
  HSE.circle
    [ HSA.cx v.x
    , HSA.cy v.y
    , HSA.r $ size / 1.5
    , HSA.fill $ Named "black"
    , HE.onMouseDown \_ -> f updateV
    ]

overlayPoints ::
  forall a b.
  (String -> (Vec2 Number -> PathCommand) -> b) ->
  Number -> Array PathCommandBlock -> Array (HH.HTML a b)
overlayPoints f size =
  concat
    <<< map \cblock ->
        points cblock.command # map (overlayPoint (f cblock.id) size)

overlayLine :: forall a b. b -> Number -> Array PathCommand -> HH.HTML a b
overlayLine f size commands =
  HSE.path
    [ HSA.d $ toHalogenPathCommand <$> commands
    , HSA.fillOpacity 0.0
    , HSA.stroke $ Named "black"
    , HSA.strokeWidth $ size / 2.0
    , HSA.strokeDashArray $ (show $ size * 3.0) <> " " <> show size
    , HE.onClick \_ -> f
    ]

overlayLines :: forall a b. (Int -> b) -> Number -> Array PathCommandBlock -> Array (HH.HTML a b)
overlayLines f size drawPath =
  overlayCommandPath <$> pathCommandContext (_.command <$> drawPath)
    # mapWithIndex \i { command, overlayPath } ->
        HSE.g [ HSA.class_ $ HH.ClassName $ commandName command ]
          $ overlayLine (f $ i + 1) size
          <$> overlayPath

overlayCommandPath ::
  PathCommandContext ->
  { command :: PathCommand
  , overlayPath :: Array (Array PathCommand)
  }
overlayCommandPath ctx =
  { command: ctx.command
  , overlayPath:
      case ctx.command of
        Move _ -> []
        Line v -> [ [ Move pos, Line v ] ]
        Bez2' v1 ->
          let
            pos1 = case ctx.prev of
              Bez2 v1' v2' -> v1' - v2'
              _ -> zero
          in
            [ [ Move pos, Bez2 (pos - pos1) v1 ] ]
        Bez2 v1 v2 ->
          [ [ Move pos, Line v1, Line v2 ]
          , [ Move pos, Bez2 v1 v2 ]
          ]
        Bez3' v1 v2 ->
          let
            pos1 = case ctx.prev of
              Bez3 _ v2' v3' -> v2' - v3'
              _ -> zero
          in
            [ [ Move v1, Line v2 ]
            , [ Move pos, Bez3 (pos - pos1) v1 v2 ]
            ]
        Bez3 v1 v2 v3 ->
          [ [ Move pos, Line v1 ]
          , [ Move v2, Line v3 ]
          , [ Move pos, Bez3 v1 v2 v3 ]
          ]
        Close -> [ [ Move pos, Line ctx.origin ] ]
  }
  where
  pos = startingPoint ctx
