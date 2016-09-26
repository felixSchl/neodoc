module Neodoc.Transform.SolveError where

import Prelude
import Data.Either (Either(..))

newtype SolveError = SolveError String

instance showSolveError :: Show SolveError where
  show (SolveError s) = "SolveError " <> show s

fail :: ∀ a. String -> Either SolveError a
fail = Left <<< SolveError
