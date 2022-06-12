module SvgEditor.View.Root (appRoot) where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.Svg.Elements as HSE
import Halogen.Svg.Attributes.Path as SP
import Effect.Aff (Aff)
import SvgEditor.View.Layer (layer)
import SvgEditor.View.Canvas (canvasProps)

type Params
  = Unit

appRoot :: forall query message. H.Component query Params message Aff
appRoot =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  initialState _ =
    { canvas:
        { viewBox:
            { top: 0.0
            , bottom: 100.0
            , left: 0.0
            , right: 100.0
            }
        }
    , layers:
        [ { name: "Layer"
          , drawPath:
              [ SP.m SP.Abs 0.0 0.0
              , SP.l SP.Abs 100.0 100.0
              , SP.z
              ]
          }
        ]
    }

  render { canvas, layers } =
    HH.div_
      [ HSE.svg (canvasProps canvas) $ map layer layers
      ]

  handleAction = case _ of
    _ -> H.modify_ \x -> x
