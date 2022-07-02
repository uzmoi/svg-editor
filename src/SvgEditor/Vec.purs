module SvgEditor.Vec
  ( Vec2(..)
  , VecRecord
  , vec2
  , toRecord
  ) where

import Prelude
import Control.Apply (lift2)
import Unsafe.Coerce (unsafeCoerce)

type VecRecord a
  = { x :: a, y :: a }

newtype Vec2 a
  = Vec2 (VecRecord a)

toRecord :: forall a. Vec2 a -> VecRecord a
toRecord = unsafeCoerce

vec2 :: forall a b. (a -> a -> b) -> Vec2 a -> b
vec2 f (Vec2 v) = f v.x v.y

instance functorVec :: Functor Vec2 where
  map f (Vec2 v) = Vec2 { x: f v.x, y: f v.y }

instance applyVec :: Apply Vec2 where
  apply (Vec2 f) (Vec2 v) = Vec2 { x: f.x v.x, y: f.y v.y }

instance semiringVec :: Semiring a => Semiring (Vec2 a) where
  add = lift2 (+)
  zero = Vec2 { x: zero, y: zero }
  mul = lift2 (*)
  one = Vec2 { x: one, y: one }

instance ringVec :: Ring a => Ring (Vec2 a) where
  sub = lift2 (-)
