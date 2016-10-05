-- An internal wrapper data structure to uniquely identify and preserve
-- arguments during reduction.

module Neodoc.Evaluate.Key where

import Prelude
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Map as Map
import Data.Pretty (class Pretty, pretty)
import Data.Array as Array
import Data.List (List)
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
import Neodoc.OptionAlias as OptionAlias
import Neodoc.Data.Description (Description(..))
import Neodoc.Data.SolvedLayout (SolvedLayoutArg(..))
import Neodoc.Evaluate.Annotate

-- A key uniquely identify an argument, which in turn may have multiple keys
-- to refer to it.
newtype Key = Key (Set ArgKey)

derive instance eqKey :: Eq Key
derive instance ordKey :: Ord Key

toKey :: WithDescription SolvedLayoutArg -> Key
toKey (x /\ mDesc) = Key (Set.fromFoldable $ go x)
  where
  go (Option a _ _) = OptionKey <$> fromMaybe (singleton a) do
    desc <- mDesc
    case desc of
      (OptionDescription as _ _ _ _) -> Just $ a : fromFoldable as
      _ -> Nothing
  go _ = singleton $ toArgKey x


instance showKey :: (Show a) => Show Key where
  show (Key keys) = "Key " <> show keys

instance prettyKey :: (Pretty a) => Pretty Key where
  pretty (Key keys) = pretty $ fromFoldable keys

-- | Derive a strin based key
toStrKeys :: Key -> List String
toStrKeys (Key keys) = go <$> (fromFoldable keys)
  where
    go (PositionalKey n)                  = n
    go (CommandKey    n)                  = n
    go (OptionKey  (OptionAlias.Long n))  = "--" <> n
    go (OptionKey  (OptionAlias.Short c)) = "-"  <> String.singleton c
    go EOAKey                             = "--"
    go StdinKey                           = "-"
