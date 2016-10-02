module Data.Pretty where

import Prelude
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Data.List (List)
import Data.Map (Map)
import Data.Map as Map
import Data.Either
import Data.StrMap (StrMap)
import Data.StrMap as StrMap
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

instance prettyMap :: (Pretty k, Pretty v) => Pretty (Map k v) where
  pretty kvs = intercalate ", " $ Map.toList kvs <#> \(k /\ v) ->
                  pretty k <> " => " <> pretty v

instance prettyStrMap :: (Pretty v) => Pretty (StrMap v) where
  pretty kvs = intercalate ", " $ StrMap.toList kvs <#> \(k /\ v) ->
                  k <> " => " <> pretty v

instance prettyEither :: (Pretty e, Pretty a) => Pretty (Either e a) where
  pretty (Left e) = pretty e
  pretty (Right a) = pretty a

instance prettyString :: Pretty String where
  pretty s = s
