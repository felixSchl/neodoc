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

type SolveOptions r = {
  smartOptions :: Boolean
  | r
}

solve
  :: ∀ r
   .  SolveOptions r
  -> Spec UsageLayout
  -> Either SolveError (Spec SolvedLayout)
solve { smartOptions } spec = pure spec
    >>= (if smartOptions then Solve.smartOptions else pure)
    >>= Solve.expandOptions
    >>= Solve.expandReferences
