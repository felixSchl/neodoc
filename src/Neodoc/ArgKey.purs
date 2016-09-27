module Neodoc.ArgKey where

import Prelude
import Data.Pretty (class Pretty, pretty)
import Neodoc.OptionAlias (OptionAlias)

data ArgKey
  = PositionalKey String
  | CommandKey    String
  | OptionKey     OptionAlias
  | EOAKey
  | StdinKey

instance showArgKey :: Show ArgKey where
  show (PositionalKey n) = "PositionalKey " <> show n
  show (CommandKey    n) = "CommandKey " <> show n
  show (OptionKey     a) = "OptionKey " <> show a
  show (EOAKey         ) = "EOAKey"
  show (StdinKey       ) = "StdinKey"

instance prettyArgKey :: Pretty ArgKey where
  pretty (PositionalKey n) = n
  pretty (CommandKey    n) = n
  pretty (OptionKey     a) = pretty a
  pretty (EOAKey         ) = "--"
  pretty (StdinKey       ) = "-"
