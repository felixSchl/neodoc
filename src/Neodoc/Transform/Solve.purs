-- | Transform a `PreSolvedLayout` into a `SolvedLayout`
-- |
-- | This transform expands `[option-...]` reference tags into real options.

module Neodoc.Transform.Solve where

import Neodoc.Spec
import Neodoc.Transform.PreSolve
import Neodoc.Data.SolvedLayout
import Neodoc.Transform.SolveError
import Data.Either (Either)

solve
  :: Spec PreSolvedLayout
  -> Either SolveError (Spec SolvedLayout)
solve _ = fail "..."
