module Neodoc.Solve where

import Prelude
import Debug.Trace
import Data.Either (Either(..), either)
import Neodoc.Spec
import Neodoc.Spec as Spec
import Neodoc.Data.UsageLayout
import Neodoc.Data.SolvedLayout
import Neodoc.Solve.Error
import Neodoc.Solve.SmartOptions as Solve
import Neodoc.Solve.ExpandOptions as Solve
import Neodoc.Solve.ExpandReferences as Solve

solve
  :: { smartOptions :: Boolean }
  -> Spec UsageLayout
  -> Either SolveError (Spec SolvedLayout)
solve { smartOptions } spec = do
  (if smartOptions then Solve.smartOptions spec else pure spec)
    >>= Solve.expandOptions
    >>= Solve.expandReferences
