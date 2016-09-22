module Data.Pretty where

import Data.NonEmpty (NonEmpty)

class Pretty a where
  pretty :: a -> String
