module Neodoc.Transform.SolveError where

import Prelude
import Data.Either (Either(..))

newtype SolveError = SolveError String

fail :: ∀ a. String -> Either SolveError a
fail = Left <<< SolveError
