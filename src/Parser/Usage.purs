module Docopt.Parser.Usage where

import Prelude
import Control.Lazy (defer)
import Control.Monad.Trans (lift)
import Control.Monad.State (get)
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
import Docopt.Parser.State

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

data Usage = Usage String (List UsageNode)

instance showUsage :: Show Usage where
  show (Usage n xs) = "Usage " ++ show n ++ " " ++ show xs

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

  usages <- many parseLine
  input <- getInput
  debug input
  debug usages

  where

    parseLine :: TokenParser Usage
    parseLine = Usage
      <$> parseProgram
      <*> mark do
        input <- getInput
        debug input
        P.manyTill
          parseElem
          ((P.lookAhead <<< void $ do
            lessIndented *> parseProgram) <|> eof)

    parseProgram :: TokenParser String
    parseProgram = name

    parseElem = defer \_ -> do
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
    parseOptGroup = defer \_ -> Group true
      <$> (P.between
            (indented *> lsquare)
            (indented *> rsquare)
            (some parseElem))
      <*> parseRepetition

    parseReqGroup :: TokenParser UsageNode
    parseReqGroup = defer \_ -> Group false
      <$> (P.between
            (indented *> lparen)
            (indented *> rparen)
            (some parseElem))
      <*> parseRepetition

    parseRepetition :: TokenParser Boolean
    parseRepetition = P.choice
      [ P.try $ indented *> tripleDot *> pure true
      , pure false
      ]
