module Docopt.Spec.Parser.Usage where

import Prelude
import Debug.Trace
import Control.Lazy (defer)
import Control.MonadPlus (guard)
import Control.Monad.Trans (lift)
import Control.Monad.State (get)
import Control.Alt ((<|>))
import Control.Apply ((*>), (<*))
import Data.List (List(..), many, some, (:), toList, concat)
import qualified Text.Parsing.Parser as P
import qualified Text.Parsing.Parser.Combinators as P
import qualified Text.Parsing.Parser.Pos as P
import qualified Text.Parsing.Parser.String as P
import qualified Data.List as L
import Data.String (length)
import Data.Either
import Data.Tuple
import Data.Maybe hiding (maybe)
import qualified Data.Array as A
import qualified Data.String as Str

import Docopt.Spec.Parser.Base
import Docopt.Spec.Parser.Common
import Docopt.Spec.Parser.Lexer
import Docopt.Spec.Parser.State

type OptionAlias    = String
type OptionArgument = String
type IsOptional     = Boolean
type IsRepeatable   = Boolean
type Branch         = List Argument

parse :: (List PositionedToken) -> Either P.ParseError (List Usage)
parse = flip runTokenParser usageParser

-- | Represent a single program usage.
-- | A single usage is made up of a list of mutually exclusive groups,
-- | separated by a vertical bar `|`. Each of those groups can contain
-- | one or more `Argument`.
-- |
-- | node node | node | node
-- | ^^^^ ^^^^   ^^^^   ^^^^
-- |   |   |      |      |
-- | [ 0 , 1 ]  [ 0 ]  [ 0 ]
-- |    \ /       |      |
-- | [   0    ,   1   ,  2 ]
data Usage = Usage String (List Branch)
data Argument
  = Command     String
  | Positional  String
                IsRepeatable
  | Option      String
                (Maybe OptionArgument)
                IsRepeatable
  | OptionStack Char
                (Array Char)
                (Maybe OptionArgument)
                IsRepeatable
  | Group       IsOptional
                (List Branch)
                IsRepeatable

instance showUsage :: Show Usage where
  show (Usage n xs) = "Usage " ++ show n ++ " " ++ show xs

instance showArgument :: Show Argument where
  show (Command n)           = "Command " ++ n
  show (Positional n b)      = "Positional " ++ n ++ " " ++ show b
  show (Option n a b)        = "Option " ++ show n ++ " " ++ show a ++ " " ++ show b
  show (OptionStack n s a b) = "OptionStack " ++ show n ++ " " ++ show s ++ " " ++ show a ++ " " ++ show b
  show (Group n b o)         = "Group " ++ show n ++ " " ++ show b ++ " " ++ show o

instance eqArgument :: Eq Argument where
  eq (Command s)            (Command s')               = (s == s')
  eq (Positional s r)       (Positional s' r')         = (s == s') && (r == r')
  eq (Option s a r)         (Option s' a' r')          = (s == s') && (a == a') && (r == r')
  eq (Group b xs r)         (Group b' xs' r')          = (b == b') && (xs == xs') && (r == r')
  eq (OptionStack c cs a r) (OptionStack c' cs' a' r') = (c == c') && (cs == cs') && (a == a') && (r == r')
  eq _                      _                          = false


-- | TokenParser to parse the usage section
-- |
-- | This parser is tricky because it has to solve the following problems:
-- |    * What is the program name?
-- |    * Disambiguate between a usage and random text.
-- |    * Disambiguate between a usage and a line-wrapped usage.
-- |
-- | Hence, the following approach is employed:
-- |    * The parser expects a valid program name as first token
-- |    * The parser marks the indent of the first program name
-- |    * Tokens past the identation mark are considered to be part of the
-- |      previous usage pattern.
-- |    * Tokens before the identation mark are NOT considered interesting and
-- |      are IGNORED.
-- |    * A token at the identation mark starts a new usage pattern parse.
-- |
usageParser :: TokenParser (List Usage)
usageParser = do

  -- Calculate and mark the original program indentation.
  name <- program
  col' <- getCol
  let startCol = col' - (length name) - 1
  markIndent' startCol $ do
    Cons
    <$> (usageLine name)
    <*> many do program *> usageLine name

  where

    usageLine :: String -> TokenParser Usage
    usageLine name = Usage name <$> do
      x <- (some $ moreIndented *> elem) `P.sepBy1` vbar
      eof <|> (P.lookAhead $ lessIndented)
      pure x

    elem :: TokenParser Argument
    elem = defer \_ -> do
      P.choice
        [ option
        , positional
        , command
        , group
        ] P.<?> "Option, Positional, Command or Group"

    longOption :: TokenParser Argument
    longOption = do
      { name: name, arg: arg } <- lopt
      Option name arg
        <$> repetition

    shortOption :: TokenParser Argument
    shortOption = do
      { flag: flag, stack: stack, arg: arg } <- sopt
      OptionStack flag stack arg
        <$> repetition

    option :: TokenParser Argument
    option = longOption <|> shortOption

    positional :: TokenParser Argument
    positional = Positional
      <$> (angleName <|> shoutName)
      <*> repetition

    command :: TokenParser Argument
    command = do
      cmd <- Command <$> name
      P.notFollowedBy tripleDot -- Commands may not repeat!
      pure cmd

    group :: TokenParser Argument
    group = defer \_ -> P.choice
      [ reqGroup
      , optGroup ]

    optGroup :: TokenParser Argument
    optGroup = defer \_ -> Group true
      <$> (P.between
            (indented *> lsquare)
            (rsquare)
            ((some elem) `P.sepBy1` vbar))
      <*> repetition

    reqGroup :: TokenParser Argument
    reqGroup = defer \_ -> Group false
      <$> (P.between
            (indented *> lparen)
            (rparen)
            ((some elem) `P.sepBy1` vbar))
      <*> repetition

    repetition :: TokenParser Boolean
    repetition = P.choice
      [ P.try $ indented *> tripleDot *> pure true
      , pure false ]

    program :: TokenParser String
    program = name <|> word
