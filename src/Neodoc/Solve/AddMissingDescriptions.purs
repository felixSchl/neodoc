module Neodoc.Solve.AddMissingDescriptions where

import Prelude
import Debug.Trace
import Data.Pretty
import Data.List (List(..), (:), findIndex, nub)
import Data.List as List
import Data.List.Extra as List
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe, fromJust)
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

-- Add missing option descriptions.
addMissingDescriptions
  :: Boolean
  -> Spec SolvedLayout
  -> Either SolveError (Spec SolvedLayout)
addMissingDescriptions implyNegs (Spec (spec@{ layouts, descriptions })) = do
  Right (Spec $ spec {
    descriptions = expandDescriptions implyNegs layouts descriptions
  })

  where
  expandDescriptions implyNegs layouts = execState do
    let solveLayout = case _ of
          (Group _ _ xs) -> for_ xs (traverse solveLayout)
          (Elem x) -> case x of
            -- only consider flags and semi-flags
            Solved.Option a mA _
              | maybe true (isOptionArgumentOptional) mA -> do
                descs <- State.get
                -- check if there's a description for this option
                let cands = if implyNegs then OptionAlias.setNegative false a
                                                : OptionAlias.setNegative true a
                                                  : Nil
                                        else a : Nil
                case findIndex (match cands) descs of
                  Just i ->
                    let descs' =
                          unsafePartial $ fromJust $ flip (List.modifyAt i) descs
                            case _ of
                              OptionDescription as r mA mD mE ->
                                let as' = unsafePartial $ NonEmpty.fromList'
                                            $ nub
                                            $ cands <> List.fromFoldable as
                                  in OptionDescription as' r mA mD mE
                              d -> d
                     in State.modify (const descs')
                  Nothing ->
                    let as' = if implyNegs then OptionAlias.setNegative false a
                                                :| OptionAlias.setNegative true a
                                                  : Nil
                                        else a :| Nil
                        newDesc = OptionDescription as' false mA Nothing Nothing
                     in State.modify (newDesc : _)
            _ -> pure unit
    for_ layouts (traverse (traverse solveLayout))

  match cands (OptionDescription aliases _ _ _ _) = any (\a -> any (_ == a) cands) aliases
  match _ _ = false
