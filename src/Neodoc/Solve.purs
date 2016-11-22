module Neodoc.Solve where

import Prelude
import Debug.Trace
import Debug.Profile
import Neodoc.Spec
import Neodoc.Data.UsageLayout
import Neodoc.Data.SolvedLayout
import Neodoc.Solve.Error
import Data.List as List
import Neodoc.Solve.Canonicalise as Solve
import Neodoc.Solve.ExpandOptions as Solve
import Neodoc.Solve.ExpandReferences as Solve
import Neodoc.Solve.SmartOptions as Solve
import Neodoc.Spec as Spec
import Data.Either (Either(..), either)
import Data.Foldable (class Foldable)
import Data.List (List(..))
import Data.Traversable (for)
import Neodoc.OptionAlias (OptionAlias)

type SolveOptions r = {
  smartOptions :: Boolean
  | r
}

solve'
  :: ∀ r
   . SolveOptions r
  -> List (Spec UsageLayout  -> Either SolveError (Spec UsageLayout))
  -> List (Spec SolvedLayout -> Either SolveError (Spec SolvedLayout))
  -> Spec UsageLayout
  -> Either SolveError (Spec SolvedLayout)
solve' { smartOptions } usageTs solvedTs =
      (if smartOptions then Solve.smartOptions else pure)
  >=> flip (List.foldM (#)) usageTs
  >=> Solve.expandOptions
  >=> Solve.expandReferences
  >=> Solve.canonicalise
  >=> flip (List.foldM (#)) solvedTs

solve
  :: ∀ r
   . SolveOptions r
  -> Spec UsageLayout
  -> Either SolveError (Spec SolvedLayout)
solve opts = solve' opts Nil Nil
