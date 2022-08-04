module SvgEditor.PathCommand
  ( PathCommand(..)
  , PathCommandType(..)
  , commandName
  , nextPoint
  , pathCommand
  , points
  , toHalogenPathCommand
  ) where

import Prelude
import Data.Tuple (Tuple(..))
import Halogen.Svg.Attributes.Path as SP
import SvgEditor.Vec (Vec2, vec2)

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
  M -> Move
  L -> Line
  C -> \v -> Bez3 v v v
  S -> \v -> Bez3' v v
  Q -> \v -> Bez2 v v
  T -> Bez2'
  Z -> \_ -> Close

data PathCommand
  = Move (Vec2 Number)
  | Line (Vec2 Number)
  | Bez3 (Vec2 Number) (Vec2 Number) (Vec2 Number)
  | Bez3' (Vec2 Number) (Vec2 Number)
  | Bez2 (Vec2 Number) (Vec2 Number)
  | Bez2' (Vec2 Number)
  -- | Arc Number Boolean Boolean (Vec2 Number)
  | Close

commandName :: PathCommand -> String
commandName = case _ of
  Move _ -> "Move"
  Line _ -> "Line"
  Bez3 _ _ _ -> "Bezier3"
  Bez3' _ _ -> "Bezier3"
  Bez2 _ _ -> "Bezier2"
  Bez2' _ -> "Bezier2"
  Close -> "Close"

nextPoint :: PathCommand -> Vec2 Number
nextPoint = case _ of
  Move v -> v
  Line v -> v
  Bez3 _ _ v -> v
  Bez3' _ v -> v
  Bez2 _ v -> v
  Bez2' v -> v
  Close -> zero

points :: PathCommand -> Array (Tuple (Vec2 Number) (Vec2 Number -> PathCommand))
points = case _ of
  Move v -> [ Tuple v Move ]
  Line v -> [ Tuple v Line ]
  Bez3 v1 v2 v3 ->
    [ Tuple v1 \w -> Bez3 w v2 v3
    , Tuple v2 \w -> Bez3 v1 w v3
    , Tuple v3 \w -> Bez3 v1 v2 w
    ]
  Bez3' v1 v2 ->
    [ Tuple v1 \w -> Bez3' w v2
    , Tuple v2 \w -> Bez3' v1 w
    ]
  Bez2 v1 v2 ->
    [ Tuple v1 \w -> Bez2 w v2
    , Tuple v2 \w -> Bez2 v1 w
    ]
  Bez2' v1 -> [ Tuple v1 $ Bez2' ]
  Close -> []

toHalogenPathCommand :: PathCommand -> SP.PathCommand
toHalogenPathCommand = case _ of
  Move v -> SP.m SP.Abs `vec2` v
  Line v -> SP.l SP.Abs `vec2` v
  Bez3 v1 v2 v3 -> SP.c SP.Abs `vec2` v1 `vec2` v2 `vec2` v3
  Bez3' v1 v2 -> SP.s SP.Abs `vec2` v1 `vec2` v2
  Bez2 v1 v2 -> SP.q SP.Abs `vec2` v1 `vec2` v2
  Bez2' v1 -> SP.t SP.Abs `vec2` v1
  Close -> SP.z
