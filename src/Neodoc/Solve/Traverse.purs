module Neodoc.Solve.Traverse where

import Prelude
import Data.Bifunctor (lmap)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty, (:|))
import Data.List (List(..), (:))


zipTraverseM
  :: ∀ a b m
   . (Monad m)
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
        Nothing -> pure (a :| as)
        Just z  -> do
          b :| bs <- go (z :| xss)
          pure (a :| as <> (b : bs)) -- note: `as` won't be run again

zipTraverseM'
  :: ∀ a b m
   . (Monad m)
  => (a -> Maybe a -> m (Tuple b (Maybe a)))
  -> NonEmpty List a
  -> m (NonEmpty List b)
zipTraverseM' f = zipTraverseM \x y -> lmap (_ :| Nil) <$> f x y
