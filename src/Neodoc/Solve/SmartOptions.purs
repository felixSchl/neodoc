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
import Neodoc.Data.SolvedLayout as Solved
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
  smartOptionsOnBranch branch = traverse go branch
    where
    -- Check for groups 
    go g@(Group o r (((Solved.Option oN oMA oR) :| x) :| Nil))
      = case x of
          _ -> fail "..."

    go x = pure x
