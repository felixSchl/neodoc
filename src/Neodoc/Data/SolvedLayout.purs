module Neodoc.Data.SolvedLayout where

import Prelude
import Data.Pretty (class Pretty, pretty)
import Data.Maybe (Maybe, maybe)
import Data.Foldable (intercalate)
import Data.List (List)
import Data.String (singleton) as String
import Data.NonEmpty (NonEmpty)
import Neodoc.Data.Layout
import Neodoc.OptionAlias

data OptionArgument
  = OptionArgument
      String  -- name
      Boolean -- optional

instance eqOptionArgument :: Eq OptionArgument where
  eq (OptionArgument n o) (OptionArgument n' o') = n == n' && o == o'

instance showOptionArgument :: Show OptionArgument where
  show (OptionArgument n o) = "OptionArgument " <> show n <> " " <> show o

instance prettyOptionArgument :: Pretty OptionArgument where
  pretty (OptionArgument n o) = (if o then "[" else "")
                              <> n
                              <> (if o then "]" else "")

-- This type can be specialized for elements of a usage section
type SolvedLayout = Layout SolvedLayoutArg
data SolvedLayoutArg
  = Command     String Boolean
  | Positional  String Boolean
  | Option      OptionAlias (Maybe OptionArgument) Boolean
  | EOA
  | Stdin

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

instance prettySolvedLayoutArg :: Pretty SolvedLayoutArg where
  pretty = go
    where
    go (Command n r) = n <> rep r
    go (Positional n r) = n <> rep r
    go EOA = "--"
    go Stdin = "-"
    go (Option n mA r) = pretty n <> maybe "" pretty mA <> rep r
    rep r = if r then "..." else ""

isRepeatable :: SolvedLayoutArg -> Boolean
isRepeatable (Command _ r) = r
isRepeatable (Positional _ r) = r
isRepeatable (Option _ _ r) = r
isRepeatable _ = false
