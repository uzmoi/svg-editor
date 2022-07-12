module SvgEditor.PathCommand
  ( PathCommand(..)
  , PathCommandType(..)
  , Pos(..)
  , commandName
  , nextPoint
  , pathCommand
  , points
  , toHalogenPathCommand
  ) where

import Prelude
import Data.Tuple (Tuple(..))
import Halogen.Svg.Attributes.Path as SP
import SvgEditor.Vec (Vec2(..), vec2)

data PathCommandType
  = M
  | L
  | C
  | S
  | Q
  | T
  -- | A
  | Z

instance showPathCommandType :: Show PathCommandType where
  show = case _ of
    M -> "M"
    L -> "L"
    C -> "C"
    S -> "S"
    Q -> "Q"
    T -> "T"
    Z -> "Z"

derive instance eqPathCommandType :: Eq PathCommandType

pathCommand :: PathCommandType -> Vec2 Number -> PathCommand
pathCommand = case _ of
  M -> Move Abs
  L -> Line Abs
  C -> \v -> Bez3 Abs v v v
  S -> \v -> Bez3' Abs v v
  Q -> \v -> Bez2 Abs v v
  T -> Bez2' Abs
  Z -> \_ -> Close

data Pos
  = Rel
  | Abs

data PathCommand
  = Move Pos (Vec2 Number)
  | Line Pos (Vec2 Number)
  | Bez3 Pos (Vec2 Number) (Vec2 Number) (Vec2 Number)
  | Bez3' Pos (Vec2 Number) (Vec2 Number)
  | Bez2 Pos (Vec2 Number) (Vec2 Number)
  | Bez2' Pos (Vec2 Number)
  -- | Arc Pos Number Boolean Boolean (Vec2 Number)
  | Close

commandName :: PathCommand -> String
commandName = case _ of
  Move _ _ -> "Move"
  Line _ _ -> "Line"
  Bez3 _ _ _ _ -> "Bezier3"
  Bez3' _ _ _ -> "Bezier3"
  Bez2 _ _ _ -> "Bezier2"
  Bez2' _ _ -> "Bezier2"
  Close -> "Close"

nextPoint :: PathCommand -> Vec2 Number
nextPoint = case _ of
  Move _ v -> v
  Line _ v -> v
  Bez3 _ _ _ v -> v
  Bez3' _ _ v -> v
  Bez2 _ _ v -> v
  Bez2' _ v -> v
  Close -> zero

points :: PathCommand -> Array (Tuple (Vec2 Number) (Vec2 Number -> PathCommand))
points = case _ of
  Move ref v -> [ Tuple v $ Move ref ]
  Line ref v -> [ Tuple v $ Line ref ]
  Bez3 ref v1 v2 v3 ->
    [ Tuple v1 \w -> Bez3 ref w v2 v3
    , Tuple v2 \w -> Bez3 ref v1 w v3
    , Tuple v3 \w -> Bez3 ref v1 v2 w
    ]
  Bez3' ref v1 v2 ->
    [ Tuple v1 \w -> Bez3' ref w v2
    , Tuple v2 \w -> Bez3' ref v1 w
    ]
  Bez2 ref v1 v2 ->
    [ Tuple v1 \w -> Bez2 ref w v2
    , Tuple v2 \w -> Bez2 ref v1 w
    ]
  Bez2' ref v1 -> [ Tuple v1 $ Bez2' ref ]
  Close -> []

toHalogenPathCommand :: PathCommand -> SP.PathCommand
toHalogenPathCommand = case _ of
  Move ref v -> SP.m (toHalogenPos ref) `vec2` v
  Line ref v -> SP.l (toHalogenPos ref) `vec2` v
  Bez3 ref v1 v2 v3 -> SP.c (toHalogenPos ref) `vec2` v1 `vec2` v2 `vec2` v3
  Bez3' ref v1 v2 -> SP.s (toHalogenPos ref) `vec2` v1 `vec2` v2
  Bez2 ref v1 v2 -> SP.q (toHalogenPos ref) `vec2` v1 `vec2` v2
  Bez2' ref v1 -> SP.t (toHalogenPos ref) `vec2` v1
  Close -> SP.z

toHalogenPos :: Pos -> SP.CommandPositionReference
toHalogenPos = case _ of
  Abs -> SP.Abs
  Rel -> SP.Rel
