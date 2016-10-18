module Data.Foldable.Extra where

import Prelude
import Data.Maybe
import Data.Foldable

-- | Try to find an element in a data structure which satisfies a predicate mapping.
findMap :: forall a b f. Foldable f => (a -> Maybe b) -> f a -> Maybe b
findMap p = foldl go Nothing
  where
  go Nothing x = p x
  go r _ = r
