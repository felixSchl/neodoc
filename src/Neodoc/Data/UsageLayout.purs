module Neodoc.Data.UsageLayout where

import Prelude
import Data.Pretty (class Pretty, pretty)
import Data.Maybe (Maybe, maybe)
import Data.Foldable (intercalate)
import Data.List (List)
import Data.String (singleton) as String
import Data.Either (Either(..))
import Data.NonEmpty (NonEmpty)
import Data.Foreign as F
import Data.Foreign.Class as F
import Data.Foreign.Index as F
import Data.Foreign.Class
import Neodoc.Data.Layout
import Neodoc.Data.OptionArgument

-- This type can be specialized for elements of a usage section
type UsageLayout = Layout UsageLayoutArg
data UsageLayoutArg
  = Command     String Boolean
  | Positional  String Boolean
  | Option      String (Maybe OptionArgument) Boolean
  | OptionStack (NonEmpty Array Char) (Maybe OptionArgument) Boolean
  | EOA
  | Stdin
  | Reference String

instance isForeignUsageLayoutArg :: IsForeign UsageLayoutArg where
  read v = do
    typ :: String <- F.readProp "type" v
    case typ of
      "Command" ->
        Command <$> F.readProp "name"       v
                <*> F.readProp "repeatable" v
      "Positional" ->
        Positional  <$> F.readProp "name"       v
                    <*> F.readProp "repeatable" v
      "EOA"   -> pure EOA
      "Stdin" -> pure Stdin
      "Reference" ->
        Reference <$> F.readProp "name" v
      "Option" ->
        Option <$> F.readProp "name"
               <*> readOptionArgument v
               <*> F.readProp 

      -- "OptionStack" ->
      _ -> Left $ F.errorAt "type" (F.JSONError $ "unknown type: " <> typ)

instance eqUsageLayoutArg :: Eq UsageLayoutArg where
  eq (Command n r) (Command n' r') = n == n' && r == r'
  eq (Positional n r) (Positional n' r') = n == n' && r == r'
  eq (Option n a r) (Option n' a' r') = n == n' && a == a' && r == r'
  eq (OptionStack cs a r) (OptionStack cs' a' r') = cs == cs' && a == a' && r == r'
  eq EOA EOA = true
  eq Stdin Stdin = true
  eq (Reference s) (Reference s') = s == s'
  eq _ _ = false

instance showUsageLayoutArg :: Show UsageLayoutArg where
  show (Command n r) = "Command " <> show n <> " " <> show r
  show (Positional n r) = "Positional " <> show n <> " " <> show r
  show (Option n a r) = "Option " <> show n <> " " <> show a <> " " <> show r
  show (OptionStack cs a r) = "OptionStack " <> show cs <> " " <> show a <> " " <> show r
  show EOA = "EOA"
  show Stdin = "Stdin"
  show (Reference s) = "Reference " <> show s

instance prettyUsageLayoutArg :: Pretty UsageLayoutArg where
  pretty = go
    where
    go (Command    n r) = n <> rep r
    go (Positional n r) = n <> rep r
    go EOA = "--"
    go Stdin = "-"
    go (Reference n) = "[" <> n <> "]"
    go (Option n ma r)
      = "--"
      <> n
      <> maybe "" (("=" <> _) <<< pretty) ma
      <> rep r
    go (OptionStack cs ma r)
      = "-"
      <> intercalate "" (String.singleton <$> cs)
      <> maybe "" (("=" <> _) <<< pretty) ma
      <> rep r
    rep r = if r then "..." else ""
