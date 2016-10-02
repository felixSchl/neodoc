module Neodoc.Data.Indexed where

import Prelude
import Data.Function (on)
import Data.Tuple.Nested ((/\))
import Data.Map as Map
import Data.Map (Map())
import Data.List (List())
import Data.Tuple (Tuple())
import Text.Parsing.Parser (ParserT())
import Control.Monad.Transformerless.RWS (RWS())
import Text.Parsing.Parser (PState(), Result()) as P
import Data.Pretty (class Pretty, pretty)
import Data.Foldable (intercalate)

-- | Auxiliary data structure for permutation parsing to preserve the original
-- | order of arguments.
data Indexed a = Indexed Int a

instance eqIndexed :: (Eq a) => Eq (Indexed a) where
  eq (Indexed n a) (Indexed n' a') = n == n' && a == a'

instance showIndexed :: (Show a) => Show (Indexed a) where
  show (Indexed n a) = "Indexed " <> show n <> " " <> show a

instance prettyIndexed :: (Pretty a) => Pretty (Indexed a) where
  pretty (Indexed n a) = "#" <> show n <> ": " <> pretty a

instance prettyIndexed' :: (Show a) => Pretty (Indexed a) where
  pretty (Indexed n a) = "#" <> show n <> ": " <> show a

-- | Note: use the tuple semantics for comparisons.
-- | Refer to the Ord instance of tuples for an explanation.
instance ordIndexed :: (Ord a) => Ord (Indexed a) where
  compare = compare `on` \(Indexed n a) -> n /\ a

getIndexedElem :: ∀ a. Indexed a -> a
getIndexedElem (Indexed _ x) = x

getIndex :: ∀ a. Indexed a -> Int
getIndex (Indexed ix _) = ix

toMap :: ∀ a. List (Indexed a) -> Map Int a
toMap xs = Map.fromFoldable $ xs <#> \(Indexed ix x) -> ix /\ x
