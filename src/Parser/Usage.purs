module Docopt.Parser.Usage where

import Prelude
import Control.Lazy (defer)
import Control.Alt ((<|>))
import Control.Apply ((*>), (<*))
import Data.List (List(..), many, some, (:), toList)
import qualified Text.Parsing.Parser as P
import qualified Text.Parsing.Parser.Combinators as P
import qualified Text.Parsing.Parser.Pos as P
import qualified Text.Parsing.Parser.String as P
import Data.Either
import Data.Maybe
import Docopt.Parser.Base
import Docopt.Parser.Common
import Docopt.Parser.Lexer

type OptionAlias    = String
type OptionArgument = String
type IsOptional     = Boolean
type IsRepeatable   = Boolean

data UsageNode
  = Command     String
  | Positional  String
                IsRepeatable
  | Option      (Maybe String)
                (Maybe Char)
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
    "Option " ++ show n ++ " " ++ show a ++ " " ++ show arg ++ " " ++ show b
  show (Group n b o) =
    "Group " ++ show n ++ " " ++ show b ++ " " ++ show o

-- | Parse the usage section
parseUsage :: TokenParser Unit
parseUsage = do
  P.Position { column: col } <- getPosition
  program <- parseProgram
  elems <- mark do
    P.manyTill
      (indented *> parseElem)
      (parseProgram)
  debug elems
  debug program

  where
    parseProgram :: TokenParser String
    parseProgram = name

    parseElem :: TokenParser UsageNode
    parseElem = defer \_ ->
          (indented *> parsePositional)
      <|> (indented *> parseCommand)
      <|> (indented *> parseGroup)

    parsePositional :: TokenParser UsageNode
    parsePositional = Positional
      <$> (angleName <|> shoutName)
      <*> parseRepetition

    parseCommand :: TokenParser UsageNode
    parseCommand = Command <$> name

    parseGroup :: TokenParser UsageNode
    parseGroup = defer \_ ->
          parseReqGroup
      <|> parseOptGroup

    parseOptGroup :: TokenParser UsageNode
    parseOptGroup = defer \_ -> Group
      <$> (indented *> pure true)
      <*> (indented *> P.between lsquare rsquare (some parseElem))
      <*> (indented *> parseRepetition)

    parseReqGroup :: TokenParser UsageNode
    parseReqGroup = defer \_ -> Group
      <$> (indented *> pure false)
      <*> (indented *> P.between lparen rparen (some parseElem))
      <*> (indented *> parseRepetition)

    parseRepetition :: TokenParser Boolean
    parseRepetition = (P.try tripleDot *> pure true) <|> pure false
