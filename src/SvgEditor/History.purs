module SvgEditor.History
  ( History
  , history
  , present
  , next
  , update
  , undo
  , redo
  , canUndo
  , canRedo
  ) where

import Prelude
import Data.List (List(..), uncons, null, (:))
import Data.Maybe (Maybe)

newtype History a
  = History { past :: List a, future :: List a, present :: a }

history :: forall a. a -> History a
history present = History { past: Nil, future: Nil, present }

present :: forall a. History a -> a
present (History history) = history.present

canUndo :: forall a. History a -> Boolean
canUndo (History history) = not $ null history.past

canRedo :: forall a. History a -> Boolean
canRedo (History history) = not $ null history.future

next :: forall a. (a -> a) -> History a -> History a
next f (History history) =
  History
    { past: history.present : history.past
    , future: Nil
    , present: f history.present
    }

update :: forall a. (a -> a) -> History a -> History a
update f (History history) =
  History
    { past: history.past
    , future: Nil
    , present: f history.present
    }

undo :: forall a. History a -> Maybe (History a)
undo (History history) =
  uncons history.past
    # map \{ head, tail } ->
        History
          { past: tail
          , future: history.present : history.future
          , present: head
          }

redo :: forall a. History a -> Maybe (History a)
redo (History history) =
  uncons history.future
    # map \{ head, tail } ->
        History
          { past: history.present : history.past
          , future: tail
          , present: head
          }
