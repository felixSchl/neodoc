module Neodoc.ArgKey where

import Prelude
import Data.Generic
import Data.Pretty (class Pretty, pretty)
import Data.String as String
import Neodoc.OptionAlias (OptionAlias)

data ArgKey
  = PositionalKey String
  | CommandKey    String
  | OptionKey     OptionAlias
  | EOAKey
  | StdinKey

derive instance ordArgKey :: Ord ArgKey
derive instance genericArgKey :: Generic ArgKey

instance eqArgKey :: Eq ArgKey where
  -- XXX: we have to call `String.toUpper` over and over again. Can we cache this?
  eq (PositionalKey n) (PositionalKey n') = eq (String.toUpper n) (String.toUpper n')
  eq (CommandKey n) (CommandKey n') = eq n n'
  eq (OptionKey a) (OptionKey a') = eq a a'
  eq EOAKey EOAKey = true
  eq StdinKey StdinKey = true
  eq _ _ = false

instance showArgKey :: Show ArgKey where
  show = gShow

instance prettyArgKey :: Pretty ArgKey where
  pretty (PositionalKey n) = n
  pretty (CommandKey    n) = n
  pretty (OptionKey     a) = pretty a
  pretty (EOAKey         ) = "--"
  pretty (StdinKey       ) = "-"
