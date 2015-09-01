module Docopt.Parsers.Meta.Option where

import Prelude
import Data.Monoid (mempty)
import Control.Apply ((*>), (<*))
import Data.Traversable (for, traverse)
import Debug.Trace (traceShow)
import Control.Monad
import Control.Alt ((<|>))
import Control.Monad.Eff.Console (log)
import Text.Parsing.Parser (Parser(), ParserT(..), PState(..))
import Text.Parsing.Parser.Pos (Position(..))
import Data.List (List(), (:))
import Text.Parsing.Parser.String (char, string, satisfy, eof, skipSpaces, whiteSpace)
import Text.Parsing.Parser.Combinators (try, sepBy)
import Data.Char (toString, toUpper)
import Data.String (charAt, fromChar, fromCharArray)
import Data.Maybe
import Data.Either
import qualified Data.List as List
import qualified Data.Array as Array
import Data.List (List(..), concat, toList, many)
import Docopt.Parsers.Base
import Docopt.Parsers.Meta.Argname

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

data LongOption  = LongOption String OptionArgument
data ShortOption = ShortOption Char (Array Char) OptionArgument
data OptionArgument
  = Explicit String
  | Implicit String
  | None

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance showLongOption :: Show LongOption where
  show (LongOption name arg) = "LongOption " ++ name ++ " " ++ show arg

instance showShortOption :: Show ShortOption where
  show (ShortOption name stack arg) = "ShortOption "
    ++ (fromChar name) ++ " "
    ++ (fromCharArray stack) ++ " "
    ++ show arg

instance showOptionArgument :: Show OptionArgument where
  show (Explicit s) = "Explicit " ++ s
  show (Implicit s) = "Implicit " ++ s
  show None         = "None"

--------------------------------------------------------------------------------
-- Parsers
--------------------------------------------------------------------------------

-- | Parse a long option.
-- |
-- | A short option may or may not have an argument.
-- | The argument may be specified explicitly by way of an equal sign `=`,
-- | or ambiguosly, by way of a space. Where a space is used, it is unclear at
-- | first as to whether the token is an argument to the option or a positional
-- | option. It is only through the parsing of the `Options` block, that the
-- | answer can be dervied.
-- |
-- | By default, the adjacent token, if parsed correctly, is assigned as the
-- | argument to the preceding option and must be proved otherwise.
-- |
-- | ```bash
-- | naval-fate --verbose
-- | naval-fate --verbose=<argument>|ARGUMENT
-- | naval-fate --verbose <argument>|ARGUMENT
-- | ```
-- |
longOption :: Parser String LongOption
longOption = do
  string "--"
  LongOption
    <$> (fromCharArray <$> (Array.some $ matches "[a-z]"))
    <*> optionArgument

-- | Parse a short option.
-- |
-- | A short option may or may not have an argument.
-- | The argument may be specified explicitly by way of an equal sign `=`,
-- | or ambiguosly, by way of a space. Where a space is used, it is unclear at
-- | first as to whether the token is an argument to the option or a positional
-- | option. It is only through the parsing of the `Options` block, that the
-- | answer can be dervied.
-- |
-- | By default, the adjacent token, if parsed correctly, is assigned as the
-- | argument to the preceding option and must be proved otherwise.
-- |
-- | ```bash
-- | naval-fate -v
-- | naval-fate -vv
-- | naval-fate -xvzf
-- | naval-fate -v <argument>|ARGUMENT
-- | naval-fate -v=<argument>|ARGUMENT
-- | naval-fate -vv <argument>|ARGUMENT
-- | naval-fate -vv=<argument>|ARGUMENT
-- | ```
-- |
shortOption :: Boolean -> Parser String ShortOption
shortOption allowStacked = do
  char '-'
  ShortOption
    <$> (matches "[a-z]")
    <*> (if allowStacked then
          Array.many $ matches "[a-z]"
        else
          return mempty)
    <*> optionArgument

-- | Parse the argument binding of an options.
-- | An option can either have an `Explicit`, an `Implicit` or no
-- | argument binding at all.
-- |
-- | `Explicit` are those that are either assigned via an `=`, or where the
-- | argument is directly adjacent (no space). There is no question as to
-- | whether the following identifier is an argument or a `Positional`.
-- |
-- | `Implicit` are those that are seperated only by spaces. It is impossible
-- | to know, without "solving" using further constraints, if we are dealing
-- | with a `Positional` or a bound argument. Note that the docopt spec dictates
-- | that without further prove, an `Implicit` argument is treated as a
-- | `Positional`.
-- |
optionArgument :: Parser String OptionArgument
optionArgument = do
  (try do
    char '='
    Explicit <$> do _ARGNAME <|> _argname_)
  <|>
  (try do
    Explicit <$> do _ARGNAME <|> _argname_)
  <|>
  (try do
    many space
    Implicit <$> do _ARGNAME <|> _argname_)
  <|> do
    return None
