module Data.NonEmpty.Extra where

import Prelude
import Data.Monoid
import Data.Maybe
import Data.List
import Data.List as List
import Data.NonEmpty

concat
  :: ∀ a
   . NonEmpty List (NonEmpty List a)
  -> NonEmpty List a
concat ((x :| xs) :| ys) = x :| xs <> (List.concat $ toList <$> ys)

append
  :: ∀ f a
   . (Semigroup (f a), Applicative f)
  => NonEmpty f a
  -> NonEmpty f a
  -> NonEmpty f a
append (x :| xs) (y :| ys) = x :| xs <> pure y <> ys

toList
  :: ∀ a
   . NonEmpty List a
  -> List a
toList (x :| xs) = x : xs

fromList
  :: ∀ a
   . List a
  -> Maybe (NonEmpty List a)
fromList (x:xs) = Just (x :| xs)
fromList Nil = Nothing
