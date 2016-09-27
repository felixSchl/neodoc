-- An internal wrapper data structure to uniquely identify and preserve
-- arguments during reduction.

module Neodoc.Evaluate.Key where

import Prelude
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Map as Map
import Data.Pretty (class Pretty, pretty)
import Control.Alt (alt)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Data.List (singleton, (:), fromFoldable)
import Data.String (singleton) as String
import Data.Foldable (intercalate)
import Data.Array (fromFoldable) as Array
import Data.Function (on)
import Data.String.Ext ((^=), (~~))
import Data.Set (Set)
import Data.Set as Set

import Neodoc.ArgKey (ArgKey(..))
import Neodoc.ArgKey.Class (toArgKey)
import Neodoc.Data.Description (Description(..))
import Neodoc.Data.SolvedLayout (SolvedLayoutArg(..))
import Neodoc.Evaluate.Annotate

-- A key uniquely identify an argument, which in turn may have multiple keys
-- to refer to it.
data Key a = Key (Set ArgKey) a

toKey :: WithDescription SolvedLayoutArg -> Key SolvedLayoutArg
toKey (x /\ mDesc) = Key (Set.fromFoldable $ go x) x
  where
  go (Option a _ _) = OptionKey <$> fromMaybe (singleton a) do
    desc <- mDesc
    case desc of
      (OptionDescription as _ _ _ _) -> Just $ a : fromFoldable as
      _ -> Nothing
  go _ = singleton $ toArgKey x


instance showKey :: (Show a) => Show (Key a) where
  show (Key keys a) = "Key " <> show keys <> " " <> show a

instance prettyKey :: (Pretty a) => Pretty (Key a) where
  pretty (Key keys a) = label <> " => " <> pretty a
    where label = pretty $ fromFoldable keys

instance eqKey :: Eq (Key a) where
  -- note: only compare the actual keys, not the payload
  eq (Key keys a) (Key keys' a') = eq keys keys'

instance ordKey :: Ord (Key a) where
  -- note: only compare the actual keys, not the payload
  compare (Key keys _) (Key keys' _) = compare keys keys'
