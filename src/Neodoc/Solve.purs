module Neodoc.Solve where

import Data.Either (Either(..), either)
import Neodoc.Spec
import Neodoc.Spec as Spec
import Neodoc.Solve.Error
import Neodoc.Data.UsageLayout
import Neodoc.Data.SolvedLayout

solve
  :: Spec UsageLayout
  -> Either SolveError (Spec SolvedLayout)
solve _ = fail "not implemented"
