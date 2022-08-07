module SvgEditor.Define where

import Prelude
import Color (Color, toHexString)
import Data.Array (foldl)

data GradientMode
  = Linear
  | Radial

instance showGradientMode :: Show GradientMode where
  show = case _ of
    Linear -> "Linear"
    Radial -> "Radial"

type GradientStop
  = { offset :: Int
    , color :: Color
    , opacity :: Number
    }

type Gradient
  = { mode :: GradientMode
    , stops :: Array GradientStop
    }

gradientHash :: Gradient -> String
gradientHash gradient =
  gradient.stops
    # foldl
        ( \hash stop ->
            hash
              <> show stop.offset
              <> toHexString stop.color
              <> show stop.opacity
        )
        (show gradient.mode)
