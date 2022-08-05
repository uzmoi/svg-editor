module SvgEditor.Layer
  ( Fill
  , FillRule(..)
  , Layer
  , Attr
  , Stroke
  , defaultFill
  , defaultAttr
  , defaultStroke
  , fillRule
  , layer
  ) where

import Prelude
import Halogen.HTML as H
import Halogen.HTML.Properties (IProp, attr)
import Halogen.Svg.Attributes.StrokeLineCap (StrokeLineCap(..))
import Halogen.Svg.Attributes.StrokeLineJoin (StrokeLineJoin(..))
import SvgEditor.PathCommandBlock (PathCommandBlock)

type Layer
  = { id :: Int
    , name :: String
    , show :: Boolean
    , drawPath :: Array PathCommandBlock
    , fill :: Fill
    , stroke :: Stroke
    , attr :: Attr
    }

layer :: Int -> Array PathCommandBlock -> Layer
layer id drawPath =
  { id
  , name: "Layer"
  , show: true
  , drawPath
  , fill: defaultFill
  , stroke: defaultStroke
  , attr: defaultAttr
  }

-- Attr
type Attr
  = { id :: String
    , class :: String
    }

defaultAttr :: Attr
defaultAttr =
  { id: ""
  , class: ""
  }

-- Fill
type Fill
  = { paint :: String
    , opacity :: Number
    , rule :: FillRule
    }

defaultFill :: Fill
defaultFill =
  { paint: "black"
  , opacity: 1.0
  , rule: NonZero
  }

-- fillRule
fillRule :: forall r i. FillRule -> IProp r i
fillRule = attr (H.AttrName "fill-rule") <<< show

data FillRule
  = NonZero
  | EvenOdd

derive instance eqFillRule :: Eq FillRule

instance showFillRule :: Show FillRule where
  show NonZero = "nonzero"
  show EvenOdd = "evenodd"

-- Stroke
type Stroke
  = { paint :: String
    , opacity :: Number
    , width :: Number
    , dashOffset :: Number
    , dashArray :: String
    , lineCap :: StrokeLineCap
    , lineJoin :: StrokeLineJoin
    , miterLimit :: Number
    }

defaultStroke :: Stroke
defaultStroke =
  { paint: "none"
  , opacity: 1.0
  , width: 1.0
  , dashOffset: 0.0
  , dashArray: ""
  , lineCap: LineCapButt
  , lineJoin: LineJoinMiter
  , miterLimit: 4.0
  }
