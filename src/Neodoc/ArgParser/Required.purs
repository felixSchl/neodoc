module Neodoc.ArgParser.Required where

import Prelude
import Data.Generic.Rep
import Data.Generic.Rep.Show (genericShow)
import Data.Tuple.Nested ((/\))
import Data.Map as Map
import Data.Map (Map())
import Data.List (List())
import Data.Tuple (Tuple())
import Data.Pretty (class Pretty, pretty)
import Data.Foldable (intercalate)

-- | Auxiliary data structure for permutation parsing
data Required  a
  = Required   a
  | Optional   a
  | Superflous a

derive instance eqRequired :: (Eq a) => Eq (Required a)
derive instance ordRequired :: (Ord a) => Ord (Required a)
derive instance genericRequired :: Generic (Required a) _

instance showRequired :: Show a => Show (Required a) where
  show = genericShow

instance prettyRequired :: (Pretty a) => Pretty (Required a) where
  pretty (Required   x) = "R(" <> pretty x <> ")"
  pretty (Optional   x) = "O(" <> pretty x <> ")"
  pretty (Superflous x) = "S(" <> pretty x <> ")"

unRequired :: ∀ a. Required a -> a
unRequired (Required   a) = a
unRequired (Optional   a) = a
unRequired (Superflous a) = a

isRequired :: ∀ a. Required a -> Boolean
isRequired (Required _) = true
isRequired _            = false

isOptional :: ∀ a. Required a -> Boolean
isOptional (Optional _) = true
isOptional _            = false

isSuperflous :: ∀ a. Required a -> Boolean
isSuperflous (Superflous _) = true
isSuperflous _              = false

toOptional :: ∀ a. Required a -> Required a
toOptional x = Optional (unRequired x)

toSuperflous :: ∀ a. Required a -> Required a
toSuperflous x = Superflous (unRequired x)
