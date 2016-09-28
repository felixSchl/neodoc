module Neodoc.Solve.SmartOptions where

import Prelude

import Data.List (List(..), (:))
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Data.Maybe (Maybe(..))
import Data.Either (Either(..), either)
import Data.NonEmpty (NonEmpty, (:|))
import Data.Traversable (for, traverse)

import Neodoc.Spec
import Neodoc.Spec as Spec
import Neodoc.Data.Layout
import Neodoc.Data.Layout as Layout
import Neodoc.Solve.Error
import Neodoc.Solve.ExpandOptions
import Neodoc.Solve.Traverse

smartOptions
  :: Spec ExpandedOptionsLayout
  -> Either SolveError (Spec ExpandedOptionsLayout)
smartOptions (Spec { program, layouts, descriptions }) = do
  layouts <- for layouts (traverse smartOptionsOnBranch)
  pure (Spec { program, layouts, descriptions })

  where
  smartOptionsOnBranch
    :: Layout.Branch ExpandedOptionsLayoutArg
    -> Either SolveError (Layout.Branch ExpandedOptionsLayoutArg)
  smartOptionsOnBranch branch = zipTraverseM' solveAdjacent branch

  solveAdjacent
    :: ExpandedOptionsLayout
    -> Maybe ExpandedOptionsLayout
    -> Either SolveError (Tuple ExpandedOptionsLayout (Maybe ExpandedOptionsLayout))
  solveAdjacent layout mAdjLayout = go layout mAdjLayout
    where
    untouched = pure $ layout /\ mAdjLayout
    go _ _ = untouched
