module Neodoc.Solve.Traverse where

import Prelude
import Debug.Trace
import Data.Bifunctor (lmap)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty, (:|))
import Data.NonEmpty.Extra as NonEmpty
import Data.List (List(..), (:))
import Partial.Unsafe

-- Traverse a non-empty list, calling a function on each element and it's
-- potential neighbor. The function is ought to return a tuple of elements
-- to inject and the neighbor if it was not consumed.
zipTraverseM
  :: ∀ a b m
   . (Monad m, Show a, Show b)
  => (a -> Maybe a -> m (Tuple (NonEmpty List b) (Maybe a)))
  -> NonEmpty List a
  -> m (NonEmpty List b)
zipTraverseM f = go
  where
    go (x :| Nil) = do
      (a :| as) /\ ma <- f x Nothing
      case ma of
        Nothing -> pure (a :| as)
        Just y  -> do
          b :| bs <- go (y :| Nil)
          pure (a :| as <> (b : bs)) -- note: `as` won't be run again

    go (x :| y : xss) = do
      (a :| as) /\ ma <- f x (Just y)

      case ma of
        Nothing ->
          case xss of
            u : us -> do
              b :| bs <- go (u :| us)
              pure (a :| as <> (b : bs)) -- note: `as` won't be run again
            _ -> pure (a :| as)
        Just z  -> do
          b :| bs <- go (z :| xss)
          pure (a :| as <> (b : bs)) -- note: `as` won't be run again

zipTraverseM'
  :: ∀ a b m
   . (Monad m, Show a, Show b)
  => (a -> Maybe a -> m (Tuple b (Maybe a)))
  -> NonEmpty List a
  -> m (NonEmpty List b)
zipTraverseM' f = zipTraverseM \x y -> lmap (_ :| Nil) <$> f x y
