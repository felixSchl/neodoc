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

type Solver a = EitherT SolveState (Either SolveError) a

runSolver
  :: ∀ a
   . List Description
  -> Solver a
  -> Either SolveError a
runSolver descriptions = flip evalStateT { descriptions }

fail :: ∀ a. String -> Solver a
fail = throwError <<< SolveError
