module SvgEditor.ReadFile
  ( readAsArrayBuffer
  , readAsDataURL
  , readAsText
  ) where

import Prelude
import Control.Monad.Except (runExcept)
import Data.Maybe (maybe)
import Data.Either (Either(..), either)
import Data.List.NonEmpty (head)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Effect (Effect)
import Effect.Aff (Aff, makeAff, effectCanceler)
import Effect.Exception (Error, error)
import Foreign as Foreign
import Web.File.Blob (Blob)
import Web.File.FileReader as FileReader
import Web.Event.EventTarget (eventListener, addEventListener, removeEventListener)
import Web.XHR.EventTypes as ET
import Web.XHR.ProgressEvent (fromEvent, total, loaded)

readAsText :: Blob -> (Number -> Number -> Effect Unit) -> Aff String
readAsText = readVia FileReader.readAsText Foreign.readString

readAsDataURL :: Blob -> (Number -> Number -> Effect Unit) -> Aff String
readAsDataURL = readVia FileReader.readAsDataURL Foreign.readString

readAsArrayBuffer :: Blob -> (Number -> Number -> Effect Unit) -> Aff ArrayBuffer
readAsArrayBuffer = readVia FileReader.readAsArrayBuffer readArrayBuffer
  where
  readArrayBuffer = Foreign.unsafeReadTagged "ArrayBuffer"

readVia ::
  forall a.
  (Blob -> FileReader.FileReader -> Effect Unit) ->
  (Foreign.Foreign -> Foreign.F a) ->
  Blob ->
  (Number -> Number -> Effect Unit) ->
  Aff a
readVia read readResult blob progress =
  makeAff \resolve -> do
    reader <- FileReader.fileReader
    handleLoad <-
      eventListener \_ ->
        FileReader.result reader
          >>= readResult
          >>> runExcept
          >>> either (Left <<< error <<< Foreign.renderForeignError <<< head) Right
          >>> resolve
    let
      unsafeErrorFromForeign :: Foreign.Foreign -> Error
      unsafeErrorFromForeign = Foreign.unsafeFromForeign
    handleError <-
      eventListener \_ ->
        FileReader.error reader
          >>= (unsafeErrorFromForeign >>> Left >>> resolve)
    handleProgress <-
      eventListener $ fromEvent
        >>> maybe (pure unit) \progressEvent ->
            progress (total progressEvent) (loaded progressEvent)
    let
      readerET = reader # FileReader.toEventTarget

      updateEventListener update =
        update ET.load handleLoad false readerET
          <> update ET.error handleError false readerET
          <> update ET.progress handleProgress false readerET
    updateEventListener addEventListener
    reader # read blob
    pure $ effectCanceler
      $ FileReader.abort reader
      <> updateEventListener removeEventListener
