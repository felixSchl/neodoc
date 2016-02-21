module Docopt.Spec.Parser.Usage where

import Prelude
import Debug.Trace
import Control.Lazy (defer)
import Control.MonadPlus (guard)
import Control.Monad.Trans (lift)
import Control.Monad.State (get)
import Control.Alt ((<|>))
import Control.Apply ((*>), (<*))
import Data.Foldable (intercalate)
import Data.List (List(..), many, some, (:), toList, concat, singleton
                  , modifyAt, length)
import qualified Text.Parsing.Parser as P
import qualified Text.Parsing.Parser.Combinators as P
import qualified Text.Parsing.Parser.Pos as P
import qualified Text.Parsing.Parser.String as P
import qualified Data.List as L
import Data.String (fromChar)
import qualified Data.String as Str
import Data.Either
import Data.Tuple
import Data.Maybe (Maybe(..), maybe)
import qualified Data.Array as A
import qualified Data.String as Str
import Control.Bind ((=<<))

import Docopt.Spec.Parser.Base
import Docopt.Spec.Parser.Common
import Docopt.Spec.Parser.Lexer
import Docopt.Spec.Parser.State

type OptionAlias    = String
type OptionArgument = String
type IsOptional     = Boolean
type IsRepeatable   = Boolean
type Branch         = List Argument

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
  | EOA

instance showUsage :: Show Usage where
  show (Usage n xs) = "Usage " ++ show n ++ " " ++ show xs

instance showArgument :: Show Argument where
  show (EOA)                 = "--"
  show (Command n)           = "Command " ++ n
  show (Positional n b)      = "Positional " ++ n ++ " " ++ show b
  show (Option n a b)        = "Option " ++ show n ++ " " ++ show a ++ " " ++ show b
  show (OptionStack n s a b) = "OptionStack " ++ show n ++ " " ++ show s ++ " " ++ show a ++ " " ++ show b
  show (Group n b o)         = "Group " ++ show n ++ " " ++ show b ++ " " ++ show o

instance eqUsage :: Eq Usage where
  eq (Usage n xs) (Usage n' xs') = (n == n') && (xs == xs')

instance eqArgument :: Eq Argument where
  eq (EOA)                  (EOA)                      = true
  eq (Command s)            (Command s')               = (s == s')
  eq (Positional s r)       (Positional s' r')         = (s == s') && (r == r')
  eq (Option s a r)         (Option s' a' r')          = (s == s') && (a == a') && (r == r')
  eq (Group b xs r)         (Group b' xs' r')          = (b == b') && (xs == xs') && (r == r')
  eq (OptionStack c cs a r) (OptionStack c' cs' a' r') = (c == c') && (cs == cs') && (a == a') && (r == r')
  eq _                      _                          = false

prettyPrintUsage :: Usage -> String
prettyPrintUsage (Usage name bs) =
  name ++ " " ++ intercalate " | " (prettyPrintBranch <$> bs)
    where
      prettyPrintBranch :: Branch -> String
      prettyPrintBranch xs = intercalate " " (prettyPrintArg <$> xs)

      prettyPrintArg :: Argument -> String
      prettyPrintArg (Command n) = n
      prettyPrintArg (Positional n r)
        = n ++ if r then "..." else ""
      prettyPrintArg (Option n a r)
        = "--" ++ n
          ++ (maybe "" ("="++) a)
          ++ if r then "..." else ""
      prettyPrintArg (OptionStack f fs a r)
        = "-" ++ (fromChar f)
          ++ (intercalate "" $ fromChar <$> toList fs)
          ++ (maybe "" ("="++) a)
          ++ if r then "..." else ""
      prettyPrintArg (Group b xs r)
        = if b then "(" else "["
          ++ intercalate " | " (prettyPrintBranch <$> bs)
          ++ if b then ")" else "]"
          ++ if r then "..." else ""
      prettyPrintArg (EOA) = "--"

run :: String -> Either P.ParseError (List Usage)
run x = parse =<< lex x

parse :: (List PositionedToken) -> Either P.ParseError (List Usage)
parse = flip runTokenParser usageParser

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
  let startCol = col' - (Str.length name) - 1
  markIndent' startCol $ do
    Cons
    <$> (usageLine name)
    <*> many do program *> usageLine name

  where

    usageLine :: String -> TokenParser Usage
    usageLine name = Usage name <$> do
      xs  <- (many $ moreIndented *> elem) `P.sepBy1` vbar
      eoa <- P.choice [
        P.try $ do
          moreIndented *> doubleDash
          return $ Just EOA
      , do
          eof <|> (P.lookAhead $ lessIndented)
          return Nothing
      ]

      -- Push the EOA onto the last branch (the most right branch)
      return $ maybe xs id do
        e <- eoa
        modifyAt
          (length xs - 1)
          (\as -> as ++ (singleton e))
          xs

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
    program = (name <|> word) P.<?> "Program Name"
