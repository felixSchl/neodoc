-- | Transform a `PreSolvedLayout` into a `SolvedLayout`
-- |
-- | This transform expands `[option-...]` reference tags into real options.

module Neodoc.Solve.Solve where

import Neodoc.Spec
import Neodoc.Solve.PreSolve
import Neodoc.Data.SolvedLayout
import Neodoc.Solve.Error
import Data.Either (Either)

solve
  :: Spec PreSolvedLayout
  -> Either SolveError (Spec SolvedLayout)
solve _ = fail "..."
