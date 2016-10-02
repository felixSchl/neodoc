module Neodoc.Solve.Type where

import Prelude

import Data.List (List(..), (:))
import Data.Either (Either(..))
import Control.Monad.Except.Trans
import Control.Monad.State (State, evalState)

import Neodoc.Data.Description
import Neodoc.Solve.Error

type SolveState = {
  descriptions :: List Description
}

type Solver a = ExceptT SolveError (State SolveState) a

fail :: ∀ a. String -> Solver a
fail = throwError <<< SolveError
