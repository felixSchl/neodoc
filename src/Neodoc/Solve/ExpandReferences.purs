-- | Transform a `PreSolvedLayout` into a `SolvedLayout`
-- |
-- | This transform expands `[option-...]` reference tags into real options.

module Neodoc.Solve.Solve where

import Neodoc.Spec
import Neodoc.Data.SolvedLayout
import Neodoc.Solve.Error
import Neodoc.Solve.ExpandOptions
import Data.Either (Either)

solve
  :: Spec ExpandedOptionsLayout
  -> Either SolveError (Spec SolvedLayout)
solve _ = fail "..."
