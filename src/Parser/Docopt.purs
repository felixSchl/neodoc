module Docopt.Parser.Docopt where

import Prelude
import Control.MonadPlus (guard)
import Control.Alt ((<|>))
import Control.Apply ((*>), (<*))
import qualified Text.Parsing.Parser as P
import qualified Text.Parsing.Parser.Combinators as P
import qualified Text.Parsing.Parser.Pos as P
import qualified Text.Parsing.Parser.String as P
import Data.List (List(..), many, toList, fromList, (:), length, filter)
import Data.String (fromCharArray)
import Data.Either
import Docopt.Parser.Base

data Section
  = Preamble String
  | Usage String
  | Options String

unSection :: Section -> String
unSection (Preamble s) = s
unSection (Usage    s) = s
unSection (Options  s) = s

data Source = Source String (List String)

instance showDocopt :: Show Source where
  show (Source x xs) = "Source " ++ show x ++ show xs

instance showSection :: Show Section where
  show (Preamble x) = "Preamble: " ++ show x
  show (Usage    x) = "Usage: "    ++ show x
  show (Options  x) = "Options: "  ++ show x

-- | Pre-parse the docopt string and break it into sections.
prelex :: P.Parser String Source
prelex = do
  preamble <- Preamble <$> parseSectionSource
  sections <- many $ P.try do
    parseSectionCons <*> parseSectionSource

  usage <-
    case findUsages sections of
      Nil                 -> P.fail "No usage section found!"
      Cons _ (Cons _ Nil) -> P.fail "More than one usage section found!"
      Cons x Nil          -> pure x

  return $ Source
    (unSection usage)
    (map unSection $ findOptions sections)

  where
    findUsages           = filter isUsage
    isUsage (Usage _)    = true
    isUsage _            = false
    findOptions          = filter isOption
    isOption (Options _) = true
    isOption _           = false

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

docopt :: String -> Either P.ParseError Unit
docopt input = do
  source <- P.runParser input prelex
  debug source
  return unit
