module Neodoc.Data.LayoutConversion where

import Prelude
import Data.NonEmpty (NonEmpty, (:|))
import Data.NonEmpty.Extra (concat, toList) as NonEmpty
import Data.Traversable (for, traverse)
import Data.List (List(..), catMaybes, fromFoldable, (:))
import Data.Maybe (Maybe(..))
import Control.Monad.State
import Control.Monad.State as State
import Neodoc.Data.Indexed
import Neodoc.Data.Layout
import Neodoc.Data.EmptyableLayout
import Neodoc.Data.IndexedLayout

toStrictLayout
  :: ∀ a
   . EmptyableLayout a
  -> Maybe (Layout a)
toStrictLayout (EmptyableElem x) = Just (Elem x)
toStrictLayout (EmptyableGroup o r branches) =
  let branches' = catMaybes $ toStrictBranch <$> branches
   in case branches' of
        x : xs -> Just (Group o r (x :| xs))
        Nil    -> Nothing

toStrictBranch
  :: ∀ a
   . EmptyableBranch a
  -> Maybe (Branch a)
toStrictBranch branch =
  let elems = catMaybes $ toStrictLayout <$> branch
  in case elems of
        x : xs -> Just (x :| xs)
        Nil    -> Nothing

toEmptyableLayout
  :: ∀ a
   . Layout a
  -> EmptyableLayout a
toEmptyableLayout (Elem x) = EmptyableElem x
toEmptyableLayout (Group o r xs)
  = EmptyableGroup o r $ NonEmpty.toList
                       $ toEmptyableBranch <$> xs

toEmptyableBranch
  :: ∀ a
   . Branch a
  -> EmptyableBranch a
toEmptyableBranch xs = NonEmpty.toList $ toEmptyableLayout <$> xs

-- Flatten a branch into a single list of elements, i.e.:
--
--      -a (-b | -c)
--
-- would become:
--
--      -a -b -c
--
flattenBranch
  :: ∀ a
   . NonEmpty List (Layout a)
  -> NonEmpty List a
flattenBranch branch = NonEmpty.concat $ flattenLayout <$> branch
  where
  flattenLayout (Elem x) = x :| Nil
  flattenLayout (Group _ _ branches)
    = NonEmpty.concat $ NonEmpty.concat $ (flattenLayout <$> _) <$> branches

-- Assign a numerical index to each element in a layout on the given branch,
-- as if the branch was completely flat, from left to right, i.e.:
--
--      -a (-b | -c)
--
-- would become:
--
--      (1:-a) ((2:-b) | (3:-c))
--
indexBranch
  :: ∀ a
   . NonEmpty List (Layout a)
  -> NonEmpty List (IndexedLayout a)
indexBranch branch = flip evalState 0 $ for branch indexLayout
  where
  indexLayout (Group o r xs) = Group o r <$> do
    for xs (traverse indexLayout)
  indexLayout (Elem x) = do
    i <- State.get
    State.put (i + 1)
    pure (Elem (Indexed i x))
