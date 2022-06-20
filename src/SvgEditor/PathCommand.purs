module SvgEditor.PathCommand
  ( PathCommand(..)
  , Pos(..)
  , Vec2
  , nextPoint
  , points
  , toHalogenPathCommand
  ) where

import Data.Tuple (Tuple(..))
import Halogen.Svg.Attributes.Path as SP

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
  Move ref v -> [ Tuple v \w -> Move ref w ]
  Line ref v -> [ Tuple v \w -> Line ref w ]
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
  Bez2' ref v1 -> [ Tuple v1 \w -> Bez2' ref w ]
  Close -> []

toHalogenPathCommand :: PathCommand -> SP.PathCommand
toHalogenPathCommand = case _ of
  Move ref v -> SP.m (toHalogenPos ref) v.x v.y
  Line ref v -> SP.l (toHalogenPos ref) v.x v.y
  Bez3 ref v1 v2 v3 -> SP.c (toHalogenPos ref) v1.x v1.y v2.x v2.y v3.x v3.y
  Bez3' ref v1 v2 -> SP.s (toHalogenPos ref) v1.x v1.y v2.x v2.y
  Bez2 ref v1 v2 -> SP.q (toHalogenPos ref) v1.x v1.y v2.x v2.y
  Bez2' ref v1 -> SP.t (toHalogenPos ref) v1.x v1.y
  Close -> SP.z

toHalogenPos :: Pos -> SP.CommandPositionReference
toHalogenPos = case _ of
  Abs -> SP.Abs
  Rel -> SP.Rel
