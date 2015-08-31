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

-- | Represent a meta token, derived from a usage line
data Meta
  = Command String
  | Positional String
  | LongOpt String (Maybe String)
  | ShortOpt Char (Array Char) (Maybe String)

-- | Represent a single usage
type Usage = List Meta
type UsageBlock = List Usage

instance showMeta :: Show Meta where
  show (Command name) =
    "Command " ++ name
  show (Positional name) =
    "Positional " ++ name
  show (LongOpt name arg) =
    "LongOpt " ++ name ++ " " ++ show arg
  show (ShortOpt x xs arg) =
    "ShortOpt " ++ (show $ fromChar x ++ fromCharArray xs) ++ " " ++ show arg

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
positional = do
  Positional <$> (_ARGNAME <|> _argname_)

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
-- | naval-fate --verbose
-- | naval-fate --verbose=<argument>|ARGUMENT
-- | naval-fate --verbose <argument>|ARGUMENT
-- | ```
-- |
longOption :: Parser String Meta
longOption = do
  string "--"
  LongOpt
    <$> (fromCharArray <$> (Array.some $ matches "[a-z]"))
    <*> ((do many $ char ' '
             Just <$> (_ARGNAME <|> _argname_)) <|> return Nothing)

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
shortOption :: Parser String Meta
shortOption = do
  char '-'
  ShortOpt
    <$> (matches "[a-z]")
    <*> (Array.many $ matches "[a-z]")
    <*> ((do many $ char ' '
             Just <$> (_ARGNAME <|> _argname_)) <|> return Nothing)

-- | Parse any type of option
option :: Parser String Meta
option = longOption <|> shortOption

-- | Parse any valid usage token
usageToken :: Parser String Meta
usageToken = option <|> positional

-- | Parse a `Usage line` into tokens.
usage :: String -> Parser String Usage
usage program = do

  -- Program name
  string program <* space
  Position { column: col } <- getPosition

  -- First line of usage tokens
  x <- usageToken `sepBy` many space

  -- And the rest of 'em
  xs <- (many $ try do
          many space *> eol
          indent (col - 1)
          many space
          usageToken `sepBy` many space)

  return $ concat (x:xs)

-- | Parse a complete usage block
-- |
-- | Usage:
-- |    naval-fate -vvv
-- |               -v
-- |    naval-fate --debug
usageBlock :: String -> Parser String UsageBlock
usageBlock program = do
  -- Title
  string "Usage:" <* chompRight

  -- First usage line, indicates indentation
  many space
  Position { column: col } <- getPosition
  x <- usage program

  -- Subsequent usage lines
  xs <- many $ try $ do
    eol
    indent (col - 1)
    usage program <* many space <* eol

  return (x:xs)

meta :: String -> Parser String Unit
meta program = do
  skipSpaces
  x <- usageBlock program
  debug x
  return unit
