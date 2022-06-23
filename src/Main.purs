module Main (main) where

import Prelude
import Control.Monad.Error.Class (throwError)
import Effect (Effect)
import Effect.Exception (error)
import Data.Maybe (maybe)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Effect.Aff (Aff)
import Web.DOM.ParentNode (QuerySelector(..))
import Web.HTML.HTMLElement (HTMLElement)
import SvgEditor.View.Root (appRoot)

awaitSelect :: QuerySelector -> Aff HTMLElement
awaitSelect querySelector@(QuerySelector str) = do
  HA.awaitLoad
  element <- HA.selectElement $ querySelector
  maybe (throwError $ error $ "Could not find '" <> str <> "'") pure element

main :: Effect Unit
main =
  HA.runHalogenAff do
    appEl <- awaitSelect $ QuerySelector "#app"
    runUI appRoot unit appEl
