module Data.List.Extra where

import Prelude
import Data.Maybe (Maybe(..))
import Data.List ((:), List(Nil))
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))

spanMap :: ∀ a b. (a -> Maybe b) -> List a -> Tuple (List b) (List a)
spanMap f = go Nil Nil
  where
  go Nil bs as = bs /\ as
  go (x:xs) bs as = case f x of
                         Just b  -> go xs (b : bs) as
                         Nothing -> bs /\ (x : xs <> as)
