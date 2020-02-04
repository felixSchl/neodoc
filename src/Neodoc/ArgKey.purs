module Neodoc.ArgKey where

import Prelude (class Eq, class Ord, class Show, eq, map, show)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Map as Map
import Data.Pretty (class Pretty, pretty)
import Data.String as String
import Data.Tuple (Tuple(..))
import Neodoc.OptionAlias (OptionAlias)
import Neodoc.Value

data ArgKey
  = PositionalKey String
  | CommandKey    String
  | OptionKey     OptionAlias
  | EOAKey
  | StdinKey

derive instance ordArgKey :: Ord ArgKey
derive instance genericArgKey :: Generic ArgKey _

instance eqArgKey :: Eq ArgKey where
  -- XXX: we have to call `String.toUpper` over and over again. Can we cache this?
  eq (PositionalKey n) (PositionalKey n') = eq (String.toUpper n) (String.toUpper n')
  eq (CommandKey n) (CommandKey n') = eq n n'
  eq (OptionKey a) (OptionKey a') = eq a a'
  eq EOAKey EOAKey = true
  eq StdinKey StdinKey = true
  eq _ _ = false

instance showArgKey :: Show ArgKey where
  show = genericShow

instance prettyArgKey :: Pretty ArgKey where
  pretty (PositionalKey n) = n
  pretty (CommandKey    n) = n
  pretty (OptionKey     a) = pretty a
  pretty (EOAKey         ) = "--"
  pretty (StdinKey       ) = "-"


argKeyMapToString :: Map.Map ArgKey Value -> Map.Map String Value
argKeyMapToString theMap =
  Map.fromFoldable (map
    (\(Tuple key val) -> Tuple (show key) val)
    (Map.toUnfoldable theMap) :: Array (Tuple String Value))


stringMapToArgKey :: Map.Map String Value -> Map.Map ArgKey Value
stringMapToArgKey theMap =
  Map.fromFoldable (map
   (\(Tuple key val) -> Tuple (CommandKey key) val)
   (Map.toUnfoldable theMap) :: Array (Tuple ArgKey Value))
