module SvgEditor.PathCommand
  ( PathCommand(..)
  , PathCommandType(..)
  , Pos(..)
  , Vec2
  , commandName
  , nextPoint
  , pathCommand
  , points
  , toHalogenPathCommand
  ) where

import Prelude
import Data.Tuple (Tuple(..))
import Halogen.Svg.Attributes.Path as SP

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

pathCommand :: PathCommandType -> Vec2 -> PathCommand
pathCommand = case _ of
  M -> Move Abs
  L -> Line Abs
  C -> \v -> Bez3 Abs v v v
  S -> \v -> Bez3' Abs v v
  Q -> \v -> Bez2 Abs v v
  T -> Bez2' Abs
  Z -> \_ -> Close

type Vec2
  = { x :: Number, y :: Number }

data Pos
  = Rel
  | Abs

data PathCommand
  = Move Pos Vec2
  | Line Pos Vec2
  | Bez3 Pos Vec2 Vec2 Vec2
  | Bez3' Pos Vec2 Vec2
  | Bez2 Pos Vec2 Vec2
  | Bez2' Pos Vec2
  -- | Arc Pos Number Boolean Boolean Vec2
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

nextPoint :: PathCommand -> Vec2
nextPoint = case _ of
  Move _ v -> v
  Line _ v -> v
  Bez3 _ _ _ v -> v
  Bez3' _ _ v -> v
  Bez2 _ _ v -> v
  Bez2' _ v -> v
  Close -> { x: 0.0, y: 0.0 }

points :: PathCommand -> Array (Tuple Vec2 (Vec2 -> PathCommand))
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

applyVec :: forall r. (Number -> Number -> r) -> Vec2 -> r
applyVec f v = f v.x v.y

toHalogenPathCommand :: PathCommand -> SP.PathCommand
toHalogenPathCommand = case _ of
  Move ref v -> SP.m (toHalogenPos ref) `applyVec` v
  Line ref v -> SP.l (toHalogenPos ref) `applyVec` v
  Bez3 ref v1 v2 v3 -> SP.c (toHalogenPos ref) `applyVec` v1 `applyVec` v2 `applyVec` v3
  Bez3' ref v1 v2 -> SP.s (toHalogenPos ref) `applyVec` v1 `applyVec` v2
  Bez2 ref v1 v2 -> SP.q (toHalogenPos ref) `applyVec` v1 `applyVec` v2
  Bez2' ref v1 -> SP.t (toHalogenPos ref) `applyVec` v1
  Close -> SP.z

toHalogenPos :: Pos -> SP.CommandPositionReference
toHalogenPos = case _ of
  Abs -> SP.Abs
  Rel -> SP.Rel
