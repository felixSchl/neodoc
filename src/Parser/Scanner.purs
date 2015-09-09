module Docopt.Parser.Scanner where

import Prelude
import Control.MonadPlus (guard)
import Control.Alt ((<|>))
import Control.Apply ((*>), (<*))
import qualified Text.Parsing.Parser as P
import qualified Text.Parsing.Parser.Combinators as P
import qualified Text.Parsing.Parser.Pos as P
import qualified Text.Parsing.Parser.String as P
import Data.List (List(..), many, toList, fromList, (:), length, filter
                 , takeWhile, sort, head)
import qualified Data.Array as A
import Data.String (toCharArray, fromCharArray)
import qualified Data.String as Str
import Data.Maybe
import Data.Either
import Docopt.Parser.Base
import qualified Docopt.Parser.Lexer as Lexer
import qualified Docopt.Parser.Usage as Usage
import Docopt.Textwrap (dedent)

data Section
  = Preamble String
  | Usage String
  | Options String

unSection :: Section -> String
unSection (Preamble s) = s
unSection (Usage    s) = s
unSection (Options  s) = s

data Docopt = Docopt String (List String)

instance showDocopt :: Show Docopt where
  show (Docopt x xs) = "Docopt " ++ show x ++ show xs

instance showSection :: Show Section where
  show (Preamble x) = "Preamble: " ++ show x
  show (Usage    x) = "Usage: "    ++ show x
  show (Options  x) = "Options: "  ++ show x

-- | Pre-parse the docopt string and break it into sections.
scanDocopt :: String -> Either P.ParseError Docopt
scanDocopt = (flip P.runParser docoptScanner) <<< dedent

docoptScanner :: P.Parser String Docopt
docoptScanner = do

  P.manyTill P.anyChar ((void $ P.lookAhead anySectionHeader) <|> P.eof)
  sections <- many parseSection
  usage <- case findUsages sections of
    Nil        -> P.fail "No usage section found!"
    Cons x Nil -> pure x
    _          -> P.fail "More than one usage section found!"

  return $ Docopt
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

      source <- fromCharArray <<< fromList <$> (P.manyTill
        P.anyChar
        (P.try $ P.lookAhead do
              emptyLine
          <|> void anySectionHeader
          <|> P.eof)) P.<?> "Section source"

      P.eof <|> (void $ P.try $ do
        emptyLine
        P.manyTill P.anyChar $ P.try do
          P.eof <|> (void $ P.lookAhead anySectionHeader))

      pure source

    parseSection :: P.Parser String Section
    parseSection = P.choice
      [ Usage   <$> (usageSectionHeader   *> extractSectionSource)
      , Options <$> (optionsSectionHeader *> extractSectionSource)
      ] P.<?> "Usage or Options section"
