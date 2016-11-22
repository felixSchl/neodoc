module Data.List.Extra where

import Data.Maybe (Maybe(..))
import Data.List (List(..), (:))

first :: ∀ a. (a -> Boolean) -> List a -> Maybe a
first f (x:xs) = if f x then Just x else first f xs
first _ Nil = Nothing
