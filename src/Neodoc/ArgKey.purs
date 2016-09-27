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

instance eqArgKey :: Eq ArgKey where
  eq (PositionalKey n) (PositionalKey n') = eq n n'
  eq (CommandKey n) (CommandKey n') = eq n n'
  eq (OptionKey a) (OptionKey a') = eq a a'
  eq EOAKey EOAKey = true
  eq StdinKey StdinKey = true
  eq _ _ = false

instance ordArgKey :: Ord ArgKey where
  compare (PositionalKey n) (PositionalKey n') = compare n n'
  compare (CommandKey n) (CommandKey n') = compare n n'
  compare (OptionKey a) (OptionKey a') = compare a a'
  compare EOAKey EOAKey = EQ
  compare StdinKey StdinKey = EQ

  -- note: this list must be carefully updated

  -- By PositionalKey
  compare (PositionalKey _) (CommandKey _) = GT
  compare (PositionalKey _) (OptionKey  _) = GT
  compare (PositionalKey _) EOAKey         = GT
  compare (PositionalKey _) StdinKey       = GT
  compare (CommandKey _) (PositionalKey _) = LT
  compare (OptionKey _)  (PositionalKey _) = LT
  compare EOAKey         (PositionalKey _) = LT
  compare StdinKey       (PositionalKey _) = LT

  -- By CommandKey
  compare (CommandKey _) (OptionKey  _) = GT
  compare (CommandKey _) EOAKey         = GT
  compare (CommandKey _) StdinKey       = GT
  compare (OptionKey _)  (CommandKey _) = LT
  compare EOAKey         (CommandKey _) = LT
  compare StdinKey       (CommandKey _) = LT

  -- By OptionKey
  compare (OptionKey _) EOAKey        = GT
  compare (OptionKey _) StdinKey      = GT
  compare EOAKey        (OptionKey _) = LT
  compare StdinKey      (OptionKey _) = LT

  -- By EOAKey
  compare EOAKey   StdinKey = GT
  compare StdinKey EOAKey   = LT

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
