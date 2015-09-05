module Docopt.Parser.Docopt where

import Prelude
import Control.MonadPlus (guard)
import Control.Alt ((<|>))
import Control.Apply ((*>), (<*))
import qualified Text.Parsing.Parser as P
import qualified Text.Parsing.Parser.Combinators as P
import qualified Text.Parsing.Parser.Pos as P
import qualified Text.Parsing.Parser.String as P
import Data.List (List(), many, toList, fromList, (:))
import Data.String (fromCharArray)
import Docopt.Parser.Base

data Section
  = Preamble String
  | Usage String
  | Options String

data Docopt = Docopt (List Section)

instance showDocopt :: Show Docopt where
  show (Docopt xs) = "Docopt " ++ show xs

instance showSection :: Show Section where
  show (Preamble x) = "Preamble: " ++ show x
  show (Usage    x) = "Usage: "    ++ show x
  show (Options  x) = "Options: "  ++ show x

prelex :: P.Parser String Docopt
prelex = do
  preamble <- Preamble <$> parseSectionSource
  sections <- many $ P.try do
    parseSectionCons <*> parseSectionSource
  return $ Docopt $ preamble:sections

parseSectionSource :: P.Parser String String
parseSectionSource = fromCharArray <<< fromList <$> do
  P.manyTill
    P.anyChar
    (P.lookAhead (void parseSectionCons) <|> P.eof)

parseSectionCons :: P.Parser String (String -> Section)
parseSectionCons = do
  P.Position { column: col }  <- getPosition
  (guard $ col == 1) P.<?> "Section to be anchored"
  many space
  P.choice
    [ P.string "Usage:"   *> pure Usage
    , P.string "Options:" *> pure Options
    ]
