module Neodoc.Data.Description (
  Description(..)
) where

import Prelude
import Data.Maybe
import Data.Generic.Rep
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (intercalate)
import Data.List.NonEmpty as NEL
import Data.Pretty (class Pretty, pretty)
import Data.Foreign (F)
import Data.Foreign as F
import Data.Foreign.Class as F
import Data.Foreign.Index as F
import Data.Foreign.Index ((!))
import Data.Foreign.Class
import Data.Foreign.Extra as F
import Data.String as String
import Control.Monad.Error.Class (catchError)
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

derive instance eqDescription :: Eq Description
derive instance ordDescription :: Ord Description
derive instance genericDescription :: Generic Description

instance showDescription :: Show Description where
  show = gShow

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
      _ -> F.fail $ F.errorAt "type" (F.JSONError $ "unknown type: " <> typ)

    where
    readAliases v =
      catchError (F.readNonemptyList =<< v ! "aliases") \es ->
        F.fail $ F.errorAt "aliases" $ NEL.head es

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

instance prettyDescription :: Pretty Description where
  pretty CommandDescription = "<CommandDescription>" -- note: placeholder
  pretty (OptionDescription as r mA mDef mEnv) =
    (intercalate ", " $ pretty <$> as)
    <> (maybe "" (\v -> " " <> pretty v) mA)
    <> (if r then "..." else "")
    <> (maybe "" (\v -> " [default: " <> pretty v <> "]") mDef)
    <> (maybe "" (\v -> " [env: " <> v <> "]") mEnv)
