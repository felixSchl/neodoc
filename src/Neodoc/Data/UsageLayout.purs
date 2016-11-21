module Neodoc.Data.UsageLayout where

import Prelude
import Data.Generic
import Data.String as String
import Data.Bifunctor (lmap)
import Data.Pretty (class Pretty, pretty)
import Data.Array as Array
import Data.Maybe (Maybe(..), maybe)
import Data.Foldable (intercalate)
import Data.List (List)
import Data.List.NonEmpty as NEL
import Data.String (singleton) as String
import Data.Either (Either(..))
import Data.NonEmpty (NonEmpty, (:|))
import Data.Foreign as F
import Data.Foreign.Class as F
import Data.Foreign.Index as F
import Data.Foreign.Index ((!))
import Data.Foreign.Class
import Data.Foreign.Extra as F
import Control.Monad.Error.Class (throwError, catchError)
import Neodoc.Data.Layout
import Neodoc.Data.OptionArgument

-- This type can be specialized for elements of a usage section
type UsageLayout = Layout UsageLayoutArg
type IsRepeatable = Boolean
type IsNegative = Boolean
data UsageLayoutArg
  = Command     String Boolean
  | Positional  String Boolean
  | Option      String IsNegative (Maybe OptionArgument) IsRepeatable
  | OptionStack (NonEmpty Array Char) IsNegative (Maybe OptionArgument) IsRepeatable
  | EOA
  | Stdin
  | Reference String

derive instance eqUsageLayoutArg :: Eq UsageLayoutArg
derive instance ordUsageLayoutArg :: Ord UsageLayoutArg
derive instance genericUsageLayoutArg :: Generic UsageLayoutArg

instance showUsageLayoutArg :: Show UsageLayoutArg where
  show = gShow


instance asForeignUsageLayoutArg :: AsForeign UsageLayoutArg where
  write (Command n r) = F.toForeign {
      type: "Command"
    , name: F.write n
    , repeatable: F.write r
    }
  write (Positional n r) = F.toForeign {
      type: "Positional"
    , name: F.write n
    , repeatable: F.write r
    }
  write (Option n neg mArg r) = F.toForeign {
      type: "Option"
    , name: F.write n
    , negative: F.write neg
    , argument: maybe F.undefined F.write mArg
    , repeatable: F.write r
    }
  write (OptionStack cs neg mArg r) = F.toForeign {
      type: "OptionStack"
    , chars: F.write $ Array.fromFoldable cs
    , negative: F.write neg
    , argument: maybe F.undefined F.write mArg
    , repeatable: F.write r
    }
  write (Reference n) = F.toForeign {
      type: "Reference"
    , name: F.write n
    }
  write Stdin = F.toForeign { type: "Stdin" }
  write EOA = F.toForeign { type: "EOA" }

instance isForeignUsageLayoutArg :: IsForeign UsageLayoutArg where
  read v = do
    typ :: String <- String.toUpper <$> F.readProp "type" v

    case typ of
      "EOA" -> pure EOA
      "STDIN" -> pure Stdin
      "COMMAND" ->
        Command
          <$> F.readProp "name"       v
          <*> F.readProp "repeatable" v
      "POSITIONAL" ->
        Positional
          <$> F.readProp "name"       v
          <*> F.readProp "repeatable" v
      "REFERENCE" ->
        Reference
          <$> F.readProp "name" v
      "OPTION" ->
        Option
          <$> F.readProp "name" v
          <*> F.readProp "negative" v
          <*> F.readPropMaybe "argument" v
          <*> F.readProp "repeatable" v
      "OPTIONSTACK" ->
        OptionStack
          <$> readOptionStack v
          <*> F.readProp "negative" v
          <*> F.readPropMaybe "argument" v
          <*> F.readProp "repeatable" v
      _ -> F.fail $ F.errorAt "type" $ F.JSONError $ "unknown type: " <> typ

    where
    readOptionStack v =
      catchError (F.readNonemptyArray =<< v ! "chars") \es ->
        F.fail $ F.errorAt "chars" $ NEL.head es

instance prettyUsageLayoutArg :: Pretty UsageLayoutArg where
  pretty = go
    where
    go (Command    n r) = n <> rep r
    go (Positional n r) = n <> rep r
    go EOA = "--"
    go Stdin = "-"
    go (Reference n) = "[" <> n <> "]"
    go (Option n neg ma r)
      = sign
      <> n
      <> maybe "" (("=" <> _) <<< pretty) ma
      <> rep r
      where sign = if neg then "--no-" else "--"
    go (OptionStack cs neg ma r)
      = sign
      <> intercalate "" (String.singleton <$> cs)
      <> maybe "" (("=" <> _) <<< pretty) ma
      <> rep r
      where sign = if neg then "+" else "-"
    rep r = if r then "..." else ""
