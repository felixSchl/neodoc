module Data.NonEmpty.Extra where

import Prelude
import Data.Monoid
import Data.List
import Data.List as List
import Data.NonEmpty

concat
  :: ∀ a
   . NonEmpty List (NonEmpty List a)
  -> NonEmpty List a
concat ((x :| xs) :| ys) = x :| xs <> (List.concat $ fromFoldable <$> ys)

append
  :: ∀ f a
   . (Semigroup (f a), Applicative f)
  => NonEmpty f a
  -> NonEmpty f a
  -> NonEmpty f a
append (x :| xs) (y :| ys) = x :| xs <> pure y <> ys
