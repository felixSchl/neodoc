module Docopt.Parsers.Meta where

import Prelude
import Control.Monad
import Control.Alt ((<|>))
import Control.Monad.Eff.Console (log)
import Text.Parsing.Parser (Parser())
import Data.List (List(), (:))
import Text.Parsing.Parser.String (char, string, satisfy, eof)
import Text.Parsing.Parser.Combinators (sepBy)
import Data.Char (toString, toUpper)
import Data.String (charAt, fromChar, fromCharArray)
import Data.Maybe
import qualified Data.String.Regex as Regex
import qualified Data.List as List
import qualified Data.Array as Array

-- | Match a char against a regex
matches :: String -> Parser String Char
matches s = satisfy \c ->
            Regex.test (Regex.regex s Regex.noFlags) (toString c)

data Meta
  = Command String
  | Positional String
  | Option
  | LongOpt String (Maybe String)
  | ShortOpt Char (Array Char) (Maybe String)

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
  string program
  char ' '
  usageToken `sepBy` (Array.many $ char ' ')
