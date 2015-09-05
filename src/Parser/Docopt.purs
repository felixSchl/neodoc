module Docopt.Parser.Docopt where

import Prelude
import Control.MonadPlus (guard)
import Control.Alt ((<|>))
import Control.Apply ((*>), (<*))
import qualified Text.Parsing.Parser as P
import qualified Text.Parsing.Parser.Combinators as P
import qualified Text.Parsing.Parser.Pos as P
import qualified Text.Parsing.Parser.String as P
import Data.List (List(), many, toList, fromList, (:), length, filter)
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

-- | Pre-parse the docopt string and break it into
--   sections.
prelex :: P.Parser String Docopt
prelex = do
  preamble <- Preamble <$> parseSectionSource
  sections <- many $ P.try do
    parseSectionCons <*> parseSectionSource
  (guard $ hasSingleUsage sections)
    P.<?> "Expected single Usage section, got " ++ (show $ numUsages sections)
  return $ Docopt $ preamble:sections

  where
    hasSingleUsage xs = (numUsages xs) == 1
    numUsages xs      = length $ filter isUsage xs
    isUsage (Usage _) = true
    isUsage _         = false

-- | Parse the body of a section.
--   The body of a section is defined as the string until
--   another section starts or the EOF is met.
parseSectionSource :: P.Parser String String
parseSectionSource = fromCharArray <<< fromList <$> do
  P.manyTill
    P.anyChar
    (P.lookAhead (void parseSectionCons) <|> P.eof)

-- | Parse a section header and partially apply the
--   corresponding Section constructor
parseSectionCons :: P.Parser String (String -> Section)
parseSectionCons = do
  P.Position { column: col }  <- getPosition
  (guard $ col == 1) P.<?> "Anchored Section"
  many space
  P.choice
    [ P.string "Usage:"   *> pure Usage
    , P.string "Options:" *> pure Options
    ]
