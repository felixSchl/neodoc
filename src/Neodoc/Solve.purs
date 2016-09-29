module Neodoc.Solve where

import Prelude
import Debug.Trace
import Data.Either (Either(..), either)
import Neodoc.Spec
import Neodoc.Spec as Spec
import Neodoc.Data.UsageLayout
import Neodoc.Data.SolvedLayout
import Neodoc.Solve.Error
import Neodoc.Solve.SmartOptions
import Neodoc.Solve.ExpandOptions
import Neodoc.Solve.ExpandReferences

solve
  :: Spec UsageLayout
  -> Either SolveError (Spec SolvedLayout)
solve x = pure x
  >>= smartOptions
  >>= expandOptions
  >>= expandReferences >>= (\x -> traceShowA x *> pure x)
