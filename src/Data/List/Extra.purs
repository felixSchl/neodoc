module Data.List.Extra where

import Prelude
import Data.Bifunctor (lmap)
import Data.Maybe (Maybe(..))
import Data.List ((:), List(Nil), reverse)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))

spanMap :: ∀ a b. (a -> Maybe b) -> List a -> Tuple (List b) (List a)
spanMap f = lmap reverse <<< go Nil
  where
  go bs Nil = bs /\ Nil
  go bs (x:xs) = case f x of
                    Just b  -> go (b:bs) xs
                    Nothing -> bs /\ (x:xs)
