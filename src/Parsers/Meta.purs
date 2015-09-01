module Docopt.Parsers.Meta where

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

type Description = Maybe String
data OptionSpec
  = FullOptionSpec  ShortOption LongOption Description
  | ShortOptionSpec ShortOption Description
  | LongOptionSpec  LongOption Description

data OptionArgument
  = Explicit String
  | Implicit String
  | None

data LongOption = LongOption String OptionArgument
data ShortOption = ShortOption Char (Array Char) OptionArgument

type Option = Either LongOption ShortOption 

-- | Represent a meta token, derived from a usage line
data Meta
  = MetaCommand String
  | MetaPositional String
  | MetaLongOption LongOption
  | MetaShortOption ShortOption

-- | Represent a single usage
type Usage = List Meta
type UsageBlock = List Usage

instance showArgument :: Show OptionArgument where
  show (Explicit s) = "Explicit " ++ s
  show (Implicit s) = "Implicit " ++ s
  show None         = "None"

instance showLongOption :: Show LongOption where
  show (LongOption name arg) = "LongOption " ++ name ++ " " ++ show arg

instance showShortOption :: Show ShortOption where
  show (ShortOption name stack arg) = "ShortOption "
    ++ (fromChar name) ++ " "
    ++ (fromCharArray stack) ++ " "
    ++ show arg

instance showMeta :: Show Meta where
  show (MetaCommand opt)    = "MetaCommand " ++ show opt
  show (MetaPositional opt) = "MetaPositional " ++ show opt
  show (MetaLongOption opt)  = "MetaLongOption " ++ show opt
  show (MetaShortOption opt) = "MetaShortOption " ++ show opt

-- | Parse an ARGNAME
_ARGNAME :: Parser String String
_ARGNAME = do
  fromCharArray <$> (Array.cons
    <$> (matches "[A-Z]")
    <*> (Array.many $ matches "[A-Z]"))

-- | Parse an <argname>
_argname_ :: Parser String String
_argname_ = do
  char '<'
  *> (fromCharArray <$> (Array.some $ matches "[a-z]")) <*
  char '>'

-- | Parse a positional argument
-- |
-- | A positional argument is a simple identifier of the shape:
-- | ```bash
-- | naval-fate <foo>
-- | naval-fata FOO
-- | ```
-- |
positional :: Parser String Meta
positional = MetaPositional <$> (_ARGNAME <|> _argname_)

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

-- | Parse any type of option
option :: Parser String Option
option = (Left <$> longOption) <|> (Right <$> shortOption true)

-- | Parse any valid usage token
usageToken :: Parser String Meta
usageToken = option <|> positional

-- | Parse a `Usage line` into tokens.
usage :: String -> Parser String Usage
usage program = do

  -- Program name
  string program <* many space
  Position { column: col } <- getPosition

  -- Parse the first and any consecutive usage rows.
  concat <$> ((:)
    <$> (usageToken `sepBy` many space)
    <*> (many $ try do
          many space *> eol
          indent (col - 1)
          many space
          usageToken `sepBy` many space))

-- | Parse a complete usage block
-- |
-- | ```
-- | Usage:
-- | / naval-fate -vvv --qux
-- | //////////// --foo --bar
-- | / naval-fate --debug
-- | ```
-- |
-- | TODO: Add support for the following notation:
-- |
-- | ```
-- | Usage: naval-fate -vvv --qux
-- | ///////////////// --foo --bar
-- | ////// naval-fate --debug
-- | ```
-- |
usageBlock :: String -> Parser String UsageBlock
usageBlock program = do

  -- Title
  string "Usage:" <* chompRight
  many space
  Position { column: col } <- getPosition

  -- Usage lines (at least one)
  (:)
    <$> (usage program)
    <*> (many $ try do
          eol
          indent (col - 1)
          usage program <* many space)

optionLine :: Parser String Unit
optionLine = do
  return unit

  -- ((try do
  --     FullOptionSpec
  --       <$> (shortOption false)
  --       <*> (do char ','
  --               many space
  --               longOption))
  --   <|> (try do ShortOptionSpec <$> shortOption false)
  --   <|> (try do LongOptionSpec <$> longOption)
  -- ) <*> Nothing

  --
  -- char ','
  -- many space
  -- lopt <- longOption
  --
  -- -- The description is expected to start
  -- --  with at least 2 spaces distance.
  -- space
  -- space
  -- many space
  --
  -- desc <- description
  --
  -- return unit

optionsBlock :: Parser String Unit
optionsBlock = do

  -- Title
  string "Options:" <* chompRight
  many space
  Position { column: col } <- getPosition

  -- Option lines
  x <- optionLine

  return unit

meta :: String -> Parser String Unit
meta program = do
  skipSpaces

  -- Parse the `Usage` section
  usage <- usageBlock program
  debug "Usage:"
  debug usage

  many $ try $ many space *> eol

  -- Parse the `Options` section
  opts <- optionsBlock
  debug "Options:"
  debug opts

  return unit
