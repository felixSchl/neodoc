module Data.Pretty where

import Prelude
import Data.Tuple
import Data.List
import Data.Foldable (class Foldable, intercalate)
import Data.Pretty (class Pretty, pretty)
import Data.NonEmpty (NonEmpty)

class Pretty a where
  pretty :: a -> String

instance prettyTuple :: (Pretty a, Pretty b) => Pretty (Tuple a b) where
  pretty (Tuple a b) = pretty a <> " => " <> pretty b

instance prettyList :: (Pretty a) => Pretty (List a) where
  pretty as = intercalate ", " $ pretty <$> as

instance prettyNonEmpty :: (Pretty a, Functor f, Foldable f) => Pretty (NonEmpty f a) where
  pretty as = intercalate ", " $ pretty <$> as
