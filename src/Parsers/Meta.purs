module Docopt.Parsers.Meta where

import Prelude
import Control.Apply ((*>), (<*))
import Data.Traversable (for, traverse)
import Debug.Trace (traceShow)
import Control.Monad
import Control.Alt ((<|>))
import Control.Monad.Eff.Console (log)
import Text.Parsing.Parser (Parser(), ParserT(..), PState(..))
import Text.Parsing.Parser.Pos (Position(..))
import Data.List (List(), (:))
import Text.Parsing.Parser.String (char, string, satisfy, eof, skipSpaces)
import Text.Parsing.Parser.Combinators (try, sepBy)
import Data.Char (toString, toUpper)
import Data.String (charAt, fromChar, fromCharArray)
import Data.Maybe
import Data.Either
import qualified Data.String.Regex as Regex
import qualified Data.List as List
import qualified Data.Array as Array
import Data.List (many, concat, toList)

-- Match a single space character
space :: Parser String Unit
space = char ' ' *> return unit

-- | Match a char against a regex
matches :: String -> Parser String Char
matches s = satisfy \c ->
            Regex.test (Regex.regex s Regex.noFlags) (toString c)

-- | Parse the end-of-line
eol :: Parser String Unit
eol = ((char '\r' *> char '\n') <|> char '\n') *> return unit

-- | Return the current parser position
getPosition :: forall a. Parser a Position
getPosition = ParserT $ \(PState { input: s, position: pos }) ->
  return { input: s, result: Right pos, consumed: true, position: pos }

-- | Ignore all whitespace until the EOL
chompRight :: Parser String Unit
chompRight = (many space) *> (eol <|> eof) *> return unit

-- | Chomp characters from the left
indent :: Int -> Parser String Unit
indent n = (for (List.range 1 n) (const space)) *> return unit

-- | Represent a meta token, derived from a usage line
data Meta
  = Command String
  | Positional String
  | LongOpt String (Maybe String)
  | ShortOpt Char (Array Char) (Maybe String)

-- | Represent a single usage
type Usage = List.List Meta
type UsageBlock =  List.List Usage

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
  x <- matches "[A-Z]"
  xs <- Array.many $ matches "[A-Z]"
  return $ fromCharArray $ Array.cons x xs

-- | Parse an <argname>
_argname_ :: Parser String String
_argname_ = do
  char '<'
  xs <- Array.some $ matches "[a-z]"
  char '>'
  return $ fromCharArray xs

-- | Parse a positional argument
positional :: Parser String Meta
positional = do
  x <- _ARGNAME <|> _argname_
  return $ Positional x

-- | Parse a long option
-- |
-- | --foo
-- | --foo <argument>|ARGUMENT
-- |
longOption :: Parser String Meta
longOption = do
  string "--"
  xs <- Array.some $ matches "[a-z]"
  arg <- (do
    List.many $ char ' '
    arg <- (_ARGNAME <|> _argname_)
    return $ Just arg) <|> (return Nothing)
  return $ LongOpt (fromCharArray xs) arg

-- | Parse a short option
-- |
-- | -v
-- | -vv
-- | -xvzf
-- | -v <argument>|ARGUMENT
-- | -vv <argument>|ARGUMENT
-- | -xvzf <argument>|ARGUMENT
-- |
shortOption :: Parser String Meta
shortOption = do
  string "-"
  x <- matches "[a-z]"
  stacked <- Array.many $ matches "[a-z]"
  arg <- (do
    List.many $ char ' '
    arg <- (_ARGNAME <|> _argname_)
    return $ Just arg) <|> (return Nothing)
  return $ ShortOpt x stacked arg

-- | Parse any type of option
option :: Parser String Meta
option = longOption <|> shortOption

-- | Parse any valid usage token
usageToken :: Parser String Meta
usageToken = option <|> positional

-- | Parse a `Usage line` into tokens.
usage :: String -> Parser String (List Meta)
usage program = do

  -- | Program name
  string program <* space
  Position { column: col } <- getPosition

  -- | First line of usage tokens
  xs <- usageToken `sepBy` (many space)

  -- | Subsequent lines of usage tokens
  xss <- try $ do
    eol
    xss <-  many $ do
      indent (col - 1) *> (many space)
      usageToken `sepBy` (many space) <* chompRight
    return $ concat xss

  return $ concat $ toList [xs, xss]

-- | Parse a complete usage block
-- |
-- | Usage:
-- |    naval-fate -vvv
-- |               -v
usageBlock :: String -> Parser String String
usageBlock program = do
  -- Title
  string "Usage:"
  chompRight

  -- First usage line, indicates indentation
  many $ space
  Position { column: col } <- getPosition
  x <- usage program
  chompRight

  -- Subsequent usage lines
  xs <- many $ do
    indent (col - 1)
    usage program <* chompRight

  traceShow ((x:xs)) \_ -> return unit

  return "xoo"

meta :: String -> Parser String String
meta program = do
  skipSpaces
  usageBlock program

  -- xs <- List.many $
  -- return unit
