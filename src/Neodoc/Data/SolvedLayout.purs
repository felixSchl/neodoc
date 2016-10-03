module Neodoc.Data.SolvedLayout where

import Prelude
import Data.Either (Either(..))
import Data.Pretty (class Pretty, pretty)
import Data.Maybe (Maybe(..), maybe)
import Data.Bifunctor (lmap)
import Data.Tuple.Nested ((/\))
import Data.Foldable (intercalate, all)
import Data.String as String
import Data.List (List)
import Data.String (singleton) as String
import Data.NonEmpty (NonEmpty)
import Data.Foreign as F
import Data.Foreign.Class as F
import Data.Foreign.Index as F
import Data.Foreign.Index ((!))
import Data.Foreign.Class
import Data.Foreign.Extra as F
import Neodoc.Data.Layout
import Neodoc.OptionAlias
import Neodoc.ArgKey (ArgKey(..))
import Neodoc.ArgKey.Class (class ToArgKey)
import Neodoc.Data.OptionArgument
import Data.Function (on)

-- This type can be specialized for elements of a usage section
type SolvedLayout = Layout SolvedLayoutArg
data SolvedLayoutArg
  = Command     String Boolean
  | Positional  String Boolean
  | Option      OptionAlias (Maybe OptionArgument) Boolean
  | EOA
  | Stdin

instance toArgKeySolvedLayoutArg :: ToArgKey SolvedLayoutArg where
  toArgKey (Command     n   _) = CommandKey n
  toArgKey (Positional  n   _) = PositionalKey n
  toArgKey (Option      a _ _) = OptionKey a
  toArgKey (EOA              ) = EOAKey
  toArgKey (Stdin            ) = StdinKey

instance eqSolvedLayoutArg :: Eq SolvedLayoutArg where
  eq (Command n r) (Command n' r') = n == n' && r == r'
  eq (Positional n r) (Positional n' r') = n == n' && r == r'
  eq (Option n mA r) (Option n' mA' r') = n == n' && mA == mA' && r == r'
  eq EOA EOA = true
  eq Stdin Stdin = true
  eq _ _ = false

instance showSolvedLayoutArg :: Show SolvedLayoutArg where
  show (Command n r) = "Command " <> show n <> " " <> show r
  show (Positional n r) = "Positional " <> show n <> " " <> show r
  show EOA = "EOA"
  show Stdin = "Stdin"
  show (Option n mA r) = "Option " <> show n <> " " <> show mA <> " " <> show r

instance ordSolvedLayoutArg :: Ord SolvedLayoutArg where
  compare (Command n r) (Command n' r') = compare (n /\ r) (n' /\ r')
  compare (Positional n r) (Positional n' r') = compare (n /\ r) (n' /\ r')
  compare (Option n mA r) (Option n' mA' r') = compare (n /\ mA /\ r) (n' /\ mA' /\ r')
  compare EOA EOA = EQ
  compare Stdin Stdin = EQ

  -- the following table must be carefully updated:

  -- by Command
  compare (Command _ _) (Positional _ _) = GT
  compare (Command _ _) (Option _ _ _)   = GT
  compare (Command _ _) EOA              = GT
  compare (Command _ _) Stdin            = GT
  compare (Positional _ _) (Command _ _) = LT
  compare (Option _ _ _)   (Command _ _) = LT
  compare EOA              (Command _ _) = LT
  compare Stdin            (Command _ _) = LT

  compare (Positional _ _) (Option _ _ _) = GT
  compare (Positional _ _) EOA            = GT
  compare (Positional _ _) Stdin          = GT
  compare (Option _ _ _) (Positional _ _) = LT
  compare EOA            (Positional _ _) = LT
  compare Stdin          (Positional _ _) = LT

  compare (Option _ _ _) EOA   = GT
  compare (Option _ _ _) Stdin = GT
  compare EOA   (Option _ _ _) = LT
  compare Stdin (Option _ _ _) = LT

  compare EOA Stdin = GT
  compare Stdin EOA = LT

instance prettySolvedLayoutArg :: Pretty SolvedLayoutArg where
  pretty = go
    where
    go (Command n r) = n <> rep r
    go (Positional n r) = n <> rep r
    go EOA = "--"
    go Stdin = "-"
    go (Option n mA r) = pretty n <> maybe "" pretty mA <> rep r
    rep r = if r then "..." else ""

instance asForeignSolvedLayoutArg :: AsForeign SolvedLayoutArg where
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
  write (Option n mArg r) = F.toForeign {
      type: "Option"
    , name: F.write n
    , argument: maybe F.undefined F.write mArg
    , repeatable: F.write r
    }
  write Stdin = F.toForeign { type: "Stdin" }
  write EOA = F.toForeign { type: "EOA" }

instance isForeignSolvedLayoutArg :: IsForeign SolvedLayoutArg where
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
      "OPTION" ->
        Option
          <$> F.readProp "name" v
          <*> F.readPropMaybe "argument" v
          <*> F.readProp "repeatable" v
      _ -> Left $ F.errorAt "type" (F.JSONError $ "unknown type: " <> typ)

isRepeatable :: SolvedLayout -> Boolean
isRepeatable (Group           _ r _) = r
isRepeatable (Elem (Command    _ r)) = r
isRepeatable (Elem (Positional _ r)) = r
isRepeatable (Elem (Option   _ _ r)) = r
isRepeatable _ = false

setRepeatable :: Boolean -> SolvedLayout -> SolvedLayout
setRepeatable r (Group           o _ xs) = Group o r xs
setRepeatable r (Elem (Command     n _)) = Elem (Command n r)
setRepeatable r (Elem (Positional  n _)) = Elem (Positional n r)
setRepeatable r (Elem (Option   a mA _)) = Elem (Option a mA r)
setRepeatable _ x = x

setRepeatableOr :: Boolean -> SolvedLayout -> SolvedLayout
setRepeatableOr r (Group           o r' xs) = Group o (r || r') xs
setRepeatableOr r (Elem (Command     n r')) = Elem (Command n (r || r'))
setRepeatableOr r (Elem (Positional  n r')) = Elem (Positional n (r || r'))
setRepeatableOr r (Elem (Option   a mA r')) = Elem (Option a mA (r || r'))
setRepeatableOr _ x = x

isOptional :: SolvedLayout -> Boolean
isOptional (Group o _ _) = o
isOptional _ = false

isPositional :: SolvedLayoutArg -> Boolean
isPositional (Positional _ _) = true
isPositional _ = false

isCommand :: SolvedLayoutArg -> Boolean
isCommand (Command _ _) = true
isCommand _ = false

isOption :: SolvedLayoutArg -> Boolean
isOption (Option _ _ _) = true
isOption _ = false

isFlag :: SolvedLayoutArg -> Boolean
isFlag (Option _ Nothing _) = true
isFlag _ = false

isGroup :: SolvedLayout -> Boolean
isGroup (Group _ _ _) = true
isGroup _ = false

-- Is this layout considered "free"?
isFreeLayout :: SolvedLayout -> Boolean
isFreeLayout (Elem (Option _ _ _)) = true
isFreeLayout (Elem _) = false
isFreeLayout (Group _ _ xs) = all (all isFreeLayout) xs
