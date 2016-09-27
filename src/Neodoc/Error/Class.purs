module Neodoc.Error.Class where

import Data.Either (Either)
import Data.Bifunctor (lmap)
import Neodoc.Error (NeodocError)

class ToNeodocError e where
  toNeodocError :: e -> NeodocError

capture :: ∀ e a. (ToNeodocError e) => Either e a -> Either NeodocError a
capture = lmap toNeodocError
