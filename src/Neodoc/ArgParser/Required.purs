module Neodoc.ArgParser.Required where

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

-- | Auxiliary data structure for permutation parsing
data Required a = Required a | Optional a

instance eqRequired :: (Eq a) => Eq (Required a) where
  eq (Required a) (Required a') = a == a'
  eq (Optional a) (Optional a') = a == a'
  eq _            _             = false

-- XXX: this is provisionary
instance ordRequired :: (Ord a) => Ord (Required a) where
  compare (Required _) (Optional _) = GT
  compare (Optional _) (Required _) = LT
  compare a b = compare (unRequired a) (unRequired b)

instance showRequired :: (Show a) => Show (Required a) where
  show (Required a) = "Required " <> show a
  show (Optional a) = "Optional " <> show a

instance pretty :: (Pretty a) => Pretty (Required a) where
  pretty (Required x) = "R(" <> pretty x <> ")"
  pretty (Optional x) = "O(" <> pretty x <> ")"

unRequired :: ∀ a. Required a -> a
unRequired (Required a) = a
unRequired (Optional a) = a

isRequired :: ∀ a. Required a -> Boolean
isRequired (Required _) = true
isRequired _            = false

toOptional :: ∀ a. Required a -> Required a
toOptional (Required a) = Optional a
toOptional (Optional a) = Optional a
