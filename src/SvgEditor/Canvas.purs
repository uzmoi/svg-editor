module SvgEditor.Canvas
  ( Bounds
  , Canvas
  ) where

type Bounds
  = { top :: Number
    , bottom :: Number
    , left :: Number
    , right :: Number
    }

type Canvas
  = { viewBox :: Bounds }
