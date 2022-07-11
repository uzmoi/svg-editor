module SvgEditor.Vec
  ( Vec2(..)
  , VecRecord
  , square
  , toRecord
  , vec2
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

square :: forall a. a -> Vec2 a
square x = Vec2 { x, y: x }

instance functorVec :: Functor Vec2 where
  map f (Vec2 v) = Vec2 { x: f v.x, y: f v.y }

instance applyVec :: Apply Vec2 where
  apply (Vec2 f) (Vec2 v) = Vec2 { x: f.x v.x, y: f.y v.y }

instance semiringVec :: Semiring a => Semiring (Vec2 a) where
  add = lift2 (+)
  zero = square zero
  mul = lift2 (*)
  one = square one

instance ringVec :: Ring a => Ring (Vec2 a) where
  sub = lift2 (-)
