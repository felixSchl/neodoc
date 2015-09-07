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
import qualified Data.Array as A
import Data.String (toCharArray, fromCharArray)
import qualified Data.String as Str
import Data.Either
import Docopt.Parser.Base
import qualified Docopt.Parser.Lexer as Lexer
import qualified Docopt.Parser.Usage as Usage

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

  P.manyTill P.anyChar (P.lookAhead anySectionHeader)
  sections <- many parseSection
  usage <- case findUsages sections of
    Nil               -> P.fail "No usage section found!"
    Cons _ (Cons _ _) -> P.fail "More than one usage section found!"
    Cons x Nil        -> pure x

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

    anySectionHeader :: P.Parser String String
    anySectionHeader = usageSectionHeader <|> optionsSectionHeader

    usageSectionHeader :: P.Parser String String
    usageSectionHeader = sectionHeader "usage"

    optionsSectionHeader :: P.Parser String String
    optionsSectionHeader = sectionHeader "options"

    sectionHeader :: String -> P.Parser String String
    sectionHeader s = (do
      col <- getCol
      guard (col == 1)
      string' s <* (P.try $ P.char ':')
      ) P.<?> "section header"

    emptyLine :: P.Parser String Unit
    emptyLine = (do
      col <- getCol
      guard (col == 1)
      many space *> eol
      ) P.<?> "empty line"

    extractSectionSource :: P.Parser String String
    extractSectionSource = do
      many space *> eol
      many $ P.try emptyLine

      -- The section block extends until the first empty line is
      -- encountered OR until a new section is found OR the EOF
      -- is encountered.

      source <- fromCharArray <<< fromList <$> P.manyTill
        P.anyChar
        (P.try $ P.lookAhead do
              emptyLine
          <|> void anySectionHeader
          <|> P.eof)

      P.eof <|> (void $ P.try $ do
        emptyLine
        P.manyTill P.anyChar $ P.try do
          P.eof <|> (void $ P.lookAhead anySectionHeader))

      pure source

    parseSection :: P.Parser String Section
    parseSection = P.choice
      [ Usage   <$> (usageSectionHeader   *> extractSectionSource)
      , Options <$> (optionsSectionHeader *> extractSectionSource)
      ]

docopt :: String -> Either P.ParseError Unit
docopt input = do

  debug "Scanning..."
  Source usageSrc _ <- P.runParser input prelex
  debug usageSrc

  debug "Lexing..."
  usageToks         <- P.runParser usageSrc Lexer.parseTokens
  debug usageToks

  debug "Parsing..."
  usage             <- Lexer.runTokenParser usageToks Usage.parseUsage
  debug usage
