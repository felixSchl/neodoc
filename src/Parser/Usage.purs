module Docopt.Parser.Usage where

import Prelude
import Control.Lazy (defer)
import Control.Alt ((<|>))
import Control.Apply ((*>), (<*))
import Data.List (List(..), some)
import qualified Text.Parsing.Parser as P
import qualified Text.Parsing.Parser.Combinators as P
import qualified Text.Parsing.Parser.Pos as P
import qualified Text.Parsing.Parser.String as P
import Data.Either
import Data.Maybe
import Docopt.Parser.Base
import Docopt.Parser.Lexer

type OptionName     = String
type OptionAlias    = String
type OptionArgument = String
type IsOptional     = Boolean
type IsRepeatable   = Boolean

data UsageNode
  = Command     String
  | Positional  String
                IsRepeatable
  | Option      OptionName
                (Maybe OptionAlias)
                (Maybe OptionArgument)
                IsRepeatable
  | Group       IsOptional
                (List UsageNode)
                IsRepeatable

instance showUsageNode :: Show UsageNode where
  show (Command n) =
    "Command " ++ n
  show (Positional n b) =
    "Positional " ++ n ++ " " ++ show b
  show (Option n a arg b) =
    "Option " ++ n ++ " " ++ show a ++ " " ++ show arg ++ " " ++ show b
  show (Group n b o) =
    "Group " ++ show n ++ " " ++ show b ++ " " ++ show o

parseUsage :: TokenParser Unit
parseUsage = do
  program <- parseProgram
  elem <- parseElem
  debug elem

  where
    parseProgram :: TokenParser String
    parseProgram = name

    parseElem :: TokenParser UsageNode
    parseElem = defer \_ ->
          parsePositional
      <|> parseCommand
      <|> parseGroup

    parsePositional :: TokenParser UsageNode
    parsePositional = Positional
      <$> (angleName <|> shoutName)
      <*> parseRepetition

    parseCommand :: TokenParser UsageNode
    parseCommand = Command <$> name

    parseRepetition :: TokenParser Boolean
    parseRepetition = (P.try tripleDot *> pure true) <|> pure false

    parseGroup :: TokenParser UsageNode
    parseGroup = defer \_ -> parseReqGroup <|> parseOptGroup

    parseOptGroup :: TokenParser UsageNode
    parseOptGroup = defer \_ -> Group
      <$> pure true
      <*> P.between lsquare rsquare (some parseElem)
      <*> parseRepetition

    parseReqGroup :: TokenParser UsageNode
    parseReqGroup = defer \_ -> Group
      <$> pure false
      <*> P.between lparen rparen (some parseElem)
      <*> parseRepetition
