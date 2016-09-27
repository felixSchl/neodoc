module Data.Pretty where

import Prelude
import Data.Tuple
import Data.List
import Data.Foldable (intercalate)
import Data.Pretty (class Pretty, pretty)
import Data.NonEmpty (NonEmpty)

class Pretty a where
  pretty :: a -> String

instance prettyTuple :: (Pretty a, Pretty b) => Pretty (Tuple a b) where
  pretty (Tuple a b) = pretty a <> " => " <> pretty b

instance prettyList :: (Pretty a) => Pretty (List a) where
  pretty as = intercalate ", " $ pretty <$> as
