module Neodoc.Data.Indexed where

import Prelude
import Data.Function (on)
import Data.Tuple.Nested ((/\))
import Data.Map as Map
import Data.Map (Map())
import Data.List (List())
import Data.Tuple (Tuple())
import Data.Pretty (class Pretty, pretty)
import Data.Foldable (intercalate)

-- | Auxiliary data structure for permutation parsing to preserve the original
-- | order of arguments.
data Indexed a = Indexed Int a

derive instance eqIndexed :: (Eq a) => Eq (Indexed a)
derive instance ordIndexed :: (Ord a) => Ord (Indexed a)

instance showIndexed :: (Show a) => Show (Indexed a) where
  show (Indexed n a) = "Indexed " <> show n <> " " <> show a

instance prettyIndexed :: (Pretty a) => Pretty (Indexed a) where
  pretty (Indexed n a) = "#" <> show n <> ": " <> pretty a

instance prettyIndexed' :: (Show a) => Pretty (Indexed a) where
  pretty (Indexed n a) = "#" <> show n <> ": " <> show a

getIndexedElem :: ∀ a. Indexed a -> a
getIndexedElem (Indexed _ x) = x

getIndex :: ∀ a. Indexed a -> Int
getIndex (Indexed ix _) = ix

toMap :: ∀ a. List (Indexed a) -> Map Int a
toMap xs = Map.fromFoldable $ xs <#> \(Indexed ix x) -> ix /\ x
