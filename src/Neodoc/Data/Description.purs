module Neodoc.Data.Description (
  Description(..)
) where

import Prelude
import Data.Maybe
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (intercalate)
import Data.Pretty (class Pretty, pretty)
import Data.Foreign (F)
import Data.Foreign as F
import Data.Foreign.Class as F
import Data.Foreign.Index as F
import Data.Foreign.Index ((!))
import Data.Foreign.Class
import Data.Foreign.Extra as F
import Data.String as String
import Neodoc.Value
import Neodoc.OptionAlias (Aliases)
import Neodoc.Data.OptionArgument (OptionArgument)

data Description
  = OptionDescription
      Aliases
      Boolean -- repeatable?
      (Maybe OptionArgument)
      (Maybe Value)  -- default
      (Maybe String) -- env var backing
  | CommandDescription

instance isForeignDescription :: IsForeign Description where
  read v = do
    typ :: String <- String.toUpper <$> F.readProp "type" v

    case typ of
      "COMMAND" -> pure CommandDescription
      "OPTION"  -> OptionDescription
        <$> readAliases v
        <*> F.readProp "repeatable" v
        <*> F.readPropMaybe "argument" v
        <*> F.readPropMaybe "default" v
        <*> F.readPropMaybe "env" v
      _ -> Left $ F.errorAt "type" (F.JSONError $ "unknown type: " <> typ)

    where
    readAliases v =
      lmap (F.errorAt "aliases") do
        F.readNonemptyList =<< v ! "aliases"

instance asForeignDescription :: AsForeign Description where
  write CommandDescription = F.toForeign { type: "COMMAND"  }
  write (OptionDescription aliases r mArg mDef mEnv) =
    F.toForeign {
      type: "OPTION"
    , aliases: Array.fromFoldable $ F.write <$> aliases
    , repeatable: F.write r
    , argument: maybe F.undefined F.write mArg
    , default: maybe F.undefined F.write mDef
    , env: maybe F.undefined F.write mEnv
    }

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
