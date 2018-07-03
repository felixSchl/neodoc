-- | Functions for working with PureScripts builtin `Number` type.
module Data.Number.Backport
  ( fromString
  , nan
  , isNaN
  , infinity
  , isFinite
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Global as G

-- | Attempt to parse a `Number` using JavaScripts `parseFloat`. Returns
-- | `Nothing` if the parse fails or if the result is not a finite number.
-- |
-- | Example:
-- | ```purs
-- | > fromString "123"
-- | (Just 123.0)
-- |
-- | > fromString "12.34"
-- | (Just 12.34)
-- |
-- | > fromString "1e4"
-- | (Just 10000.0)
-- |
-- | > fromString "1.2e4"
-- | (Just 12000.0)
-- |
-- | > fromString "bad"
-- | Nothing
-- | ```
-- |
-- | Note that `parseFloat` allows for trailing non-digit characters and
-- | whitespace as a prefix:
-- | ```
-- | > fromString "  1.2 ??"
-- | (Just 1.2)
-- | ```
fromString ∷ String → Maybe Number
fromString = G.readFloat >>> check
  where
    check num | isFinite num = Just num
              | otherwise    = Nothing

-- | Not a number (NaN).
nan ∷ Number
nan = G.nan

-- | Test whether a `Number` is NaN.
isNaN ∷ Number → Boolean
isNaN = G.isNaN

-- | Positive infinity.
infinity ∷ Number
infinity = G.infinity

-- | Test whether a number is finite.
isFinite ∷ Number → Boolean
isFinite = G.isFinite
