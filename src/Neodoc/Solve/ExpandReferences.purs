-- | Transform a `ExpandedOptionsLayout` into a `SolvedLayout`
-- |
-- | This transform expands `[option-...]` reference tags into real options.

module Neodoc.Solve.ExpandReferences where

import Prelude

import Data.Traversable (traverse, for)

import Neodoc.Spec
import Neodoc.Data.SolvedLayout
import Neodoc.Data.SolvedLayout as Solved
import Neodoc.Solve.Error
import Neodoc.Solve.ExpandOptions
import Data.Either (Either)

import Partial.Unsafe (unsafePartial)

expandReferences
  :: Spec ExpandedOptionsLayout
  -> Either SolveError (Spec SolvedLayout)
expandReferences (Spec { program, layouts, descriptions }) = do
  let layouts' = unsafePartial $ ((fakeSolve <$> _) <$> _) <$> layouts
  pure (Spec { program, layouts: layouts', descriptions })

  where
  fakeSolve
    :: Partial
    => ExpandedOptionsLayout
    -> SolvedLayout
  fakeSolve x = x <#> case _ of
                            SolvedArg x -> x
                            ReferenceArg _ -> (Solved.Command "foo" false {- ... just fake one ... -})
