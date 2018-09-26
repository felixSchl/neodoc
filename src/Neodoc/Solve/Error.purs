module Neodoc.Solve.Error where

import Prelude
import Data.Generic.Rep
import Data.Either (Either(..))
import Data.Pretty (class Pretty, pretty)
import Neodoc.Error (NeodocError(..))
import Neodoc.Error.Class (class ToNeodocError)

newtype SolveError = SolveError String

derive instance genericSolveError :: Generic SolveError

instance showSolveError :: Show SolveError where
  show = gShow

instance prettySolveError :: Pretty SolveError where
  pretty (SolveError s) = s

instance toNeodocErrorSolveError :: ToNeodocError SolveError where
  toNeodocError (SolveError s) = SpecSolveError s

fail :: ∀ a. String -> Either SolveError a
fail = Left <<< SolveError
