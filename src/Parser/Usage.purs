module Docopt.Parser.Usage where

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
import Docopt.Parser.Base
import Docopt.Parser.Common
import Docopt.Parser.Lexer
import Docopt.Parser.State
import qualified Data.Array as A
import qualified Data.String as Str

type OptionAlias    = String
type OptionArgument = String
type IsOptional     = Boolean
type IsRepeatable   = Boolean

parse :: (List PositionedToken) -> Either P.ParseError (List Usage)
parse = flip runTokenParser usageParser

-- | Represent a single program usage.
-- | A single usage is made up of a list of mutually exclusive groups,
-- | separated by a vertical bar `|`. Each of those groups can contain
-- | one or more `UsageNode`.
-- |
-- | node node | node | node
-- | ^^^^ ^^^^   ^^^^   ^^^^
-- |   |   |      |      |
-- | [ 0 , 1 ]  [ 0 ]  [ 0 ]
-- |    \ /       |      |
-- | [   0    ,   1   ,  2 ]
data Usage = Usage String (List (List UsageNode))
data UsageNode
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
                (List (List UsageNode))
                IsRepeatable

instance showUsage :: Show Usage where
  show (Usage n xs) = "Usage " ++ show n ++ " " ++ show xs

instance showUsageNode :: Show UsageNode where
  show (Command n) =
    "Command " ++ n
  show (Positional n b) =
    "Positional " ++ n ++ " " ++ show b
  show (Option n a b) =
    "Option " ++ show n ++ " " ++ show a ++ " " ++ show b
  show (OptionStack n s a b) =
    "OptionStack " ++ show n ++ " " ++ show s ++ " " ++ show a ++ " " ++ show b
  show (Group n b o) =
    "Group " ++ show n ++ " " ++ show b ++ " " ++ show o

instance eqUsageNode :: Eq UsageNode where
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
  name <- parseProgram
  col' <- getCol
  let startCol = col' - (length name) - 1
  markIndent' startCol $ do
    Cons
    <$> (parseSingleUsage name)
    <*> many do
          parseProgram
          (parseSingleUsage name)

  where

    parseSingleUsage :: String -> TokenParser Usage
    parseSingleUsage name = Usage name <$> do
      x <- (some $ moreIndented *> parseElem) `P.sepBy1` vbar
      eof <|> (P.lookAhead $ lessIndented)
      pure x

    parseElem :: TokenParser UsageNode
    parseElem = defer \_ -> do
      P.choice
        [ parseOption
        , parsePositional
        , parseCommand
        , parseGroup
        ] P.<?> "Option, Positional, Command or Group"

    parseLongOption :: TokenParser UsageNode
    parseLongOption = Option
      <$> lopt
      <*> (tryMaybe do
            equal
            (shoutName <|> angleName <|> name))
      <*> parseRepetition

    parseShortOption :: TokenParser UsageNode
    parseShortOption = do
      { flag: flag, stack: stack, arg: arg } <- sopt
      OptionStack flag stack
          <$> (case arg of
                Just _  -> pure arg
                Nothing -> tryMaybe do
                  equal
                  P.choice [
                    P.try angleName
                  , P.try shoutName
                  , P.try name
                  ])
          <*> parseRepetition

    parseOption :: TokenParser UsageNode
    parseOption = (parseLongOption <|> parseShortOption)

    parsePositional :: TokenParser UsageNode
    parsePositional = Positional
      <$> (angleName <|> shoutName)
      <*> parseRepetition

    parseCommand :: TokenParser UsageNode
    parseCommand = do
      cmd <- Command <$> name
      P.notFollowedBy tripleDot -- Commands may not repeat!
      pure cmd

    parseGroup :: TokenParser UsageNode
    parseGroup = defer \_ -> P.choice
      [ parseReqGroup
      , parseOptGroup ]

    parseOptGroup :: TokenParser UsageNode
    parseOptGroup = defer \_ -> Group true
      <$> (P.between
            (indented *> lsquare)
            (rsquare)
            ((some parseElem) `P.sepBy1` vbar))
      <*> parseRepetition

    parseReqGroup :: TokenParser UsageNode
    parseReqGroup = defer \_ -> Group false
      <$> (P.between
            (indented *> lparen)
            (rparen)
            ((some parseElem) `P.sepBy1` vbar))
      <*> parseRepetition

    parseRepetition :: TokenParser Boolean
    parseRepetition = P.choice
      [ P.try $ indented *> tripleDot *> pure true
      , pure false ]

    parseProgram :: TokenParser String
    parseProgram = name <|> word
