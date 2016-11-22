module Neodoc.Solve.AddMissingDescriptions where

import Prelude
import Data.List (List(..), (:))
import Data.List.Extra as List
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.NonEmpty ((:|))
import Data.NonEmpty.Extra as NonEmpty
import Data.Traversable (for_, traverse)
import Data.Foldable (any)
import Control.Monad.State (evalState, execState)
import Control.Monad.State as State
import Partial.Unsafe
import Neodoc.OptionAlias as OptionAlias
import Neodoc.Data.Description
import Neodoc.Data.OptionArgument
import Neodoc.Data.Layout
import Neodoc.Data.SolvedLayout
import Neodoc.Data.SolvedLayout as Solved
import Neodoc.Spec
import Neodoc.Spec as Spec
import Neodoc.Solve.Error

-- Add missing option descriptions
-- Note: this does *not* fill in option negatives. filling in negatives is only
--       performed during the final evaluation / reduction step.
addMissingDescriptions
  :: Spec SolvedLayout
  -> Either SolveError (Spec SolvedLayout)
addMissingDescriptions (Spec (spec@{ layouts, descriptions })) = do
  Right (Spec $ spec { descriptions = expandDescriptions layouts descriptions })

  where
  expandDescriptions layouts = execState do
    let solveLayout = case _ of
          (Group _ _ xs) -> for_ xs (traverse solveLayout)
          (Elem x) -> case x of
            -- only consider flags and semi-flags
            Solved.Option a mA _
              | maybe true (isOptionArgumentOptional) mA -> do
                descs <- State.get
                -- check if there's a description for this option
                let posAlias = OptionAlias.setNegative false a
                    negAlias = OptionAlias.setNegative true  a
                case List.first (match negAlias posAlias) descs of
                  -- ignore: it already exists in some form.
                  -- Again we do not bother to expand all positive/negatives
                  -- here.
                  Just _  -> pure unit
                  Nothing ->
                    let newDesc = OptionDescription (posAlias :| Nil)
                                                    false
                                                    mA
                                                    Nothing
                                                    Nothing
                      in State.modify (newDesc : _)
            _ -> pure unit
    for_ layouts (traverse (traverse solveLayout))

  match negAlias posAlias (OptionDescription aliases _ _ _ _)
    = any (\a -> a == negAlias || a == posAlias) aliases
  match _ _ _ = false
