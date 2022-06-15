module SvgEditor.Layer
  ( Fill
  , FillRule(..)
  , Layer
  , Stroke
  , fillRule
  ) where

import Prelude
import Halogen.HTML as H
import Halogen.HTML.Properties (IProp, attr)
import Halogen.Svg.Attributes (Color, StrokeLineCap, StrokeLineJoin)
import SvgEditor.PathCommand (PathCommand)

type Layer
  = { id :: Int
    , name :: String
    , show :: Boolean
    , drawPath :: Array PathCommand
    , fill :: Fill
    , stroke :: Stroke
    }

type Fill
  = { color :: Color
    , opacity :: Number
    , rule :: FillRule
    }

fillRule :: forall r i. FillRule -> IProp r i
fillRule = attr (H.AttrName "fill-rule") <<< show

data FillRule
  = NonZero
  | EvenOdd

derive instance eqFillRule :: Eq FillRule

instance showFillRule :: Show FillRule where
  show NonZero = "nonzero"
  show EvenOdd = "evenodd"

type Stroke
  = { color :: Color
    , opacity :: Number
    , width :: Number
    , dashOffset :: Number
    , dashArray :: String
    , lineCap :: StrokeLineCap
    , lineJoin :: StrokeLineJoin
    , miterLimit :: Number
    }
