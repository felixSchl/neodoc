module Neodoc.Data.Description (
  Description(..)
, module Reexport
) where

import Prelude
import Data.Maybe
import Data.Foldable (intercalate)
import Data.Pretty (class Pretty, pretty)
import Neodoc.Value
import Neodoc.OptionAlias (Aliases)
import Neodoc.Data.UsageLayout (OptionArgument)
import Neodoc.Data.UsageLayout (OptionArgument(..)) as Reexport

data Description
  = OptionDescription
      Aliases
      Boolean -- repeatable?
      (Maybe OptionArgument)
      (Maybe Value)  -- default
      (Maybe String) -- env var backing
  | CommandDescription

instance eqDescription :: Eq Description where
  eq CommandDescription CommandDescription = true
  eq (OptionDescription as  r  mA  mDef  mEnv)
     (OptionDescription as' r' mA' mDef' mEnv')
     = as == as' && r == r' && mA == mA' && mDef == mDef' && mEnv == mEnv'
  eq _ _ = false

instance showDescription :: Show Description where
  show CommandDescription = "CommandDescription"
  show (OptionDescription as r mA mDef mEnv) = "OptionDescription " <> show as
    <> " " <> show r <> " " <> show mA <> " " <> show mDef <> " " <> show mEnv

instance prettyDescription :: Pretty Description where
  pretty CommandDescription = "<CommandDescription>" -- note: placeholder
  pretty (OptionDescription as r mA mDef mEnv) =
    (intercalate ", " $ pretty <$> as)
    <> (maybe "" (\v -> " " <> pretty v) mA)
    <> (if r then "..." else "")
    <> (maybe "" (\v -> " [default: " <> pretty v <> "]") mDef)
    <> (maybe "" (\v -> " [env: " <> v <> "]") mEnv)
