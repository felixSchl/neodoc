module Docopt.Spec.Parser.Scanner where

import Prelude
import Debug.Trace
import Control.MonadPlus (guard)
import Control.Alt ((<|>))
import Control.Apply ((*>), (<*))
import Text.Parsing.Parser.Combinators ((<?>))
import qualified Text.Parsing.Parser as P
import qualified Text.Parsing.Parser.Combinators as P
import qualified Text.Parsing.Parser.Pos as P
import qualified Text.Parsing.Parser.String as P
import Data.List (List(..), many, toList, fromList, (:), length, filter
                 , takeWhile, sort, head)
import qualified Data.Array as A
import Data.String (toLower, toCharArray, fromCharArray)
import qualified Data.String as Str
import Data.Maybe
import Data.Either
import Docopt.Spec.Parser.Base
import qualified Docopt.Spec.Parser.Lexer as Lexer
import qualified Docopt.Spec.Parser.Usage as Usage
import Text.Wrap (dedent)

type Docopt = { usage :: Section, options :: List Section }
type Section = String

scan :: String -> Either P.ParseError Docopt
scan = (flip P.runParser docoptScanner) <<< dedent

docoptScanner :: P.Parser String Docopt
docoptScanner = do

  P.manyTill
    P.anyChar
    ((void $ P.lookAhead sectionLabel) <|> P.eof)

  label <- sectionLabel <?> "section label"
  guard $ (toLower label) == "usage"

  -- "Fix" the section by replacing the original section header with whitespace
  -- to maintain proper offsets.
  fixColOffset <- (P.char '\n' *> pure 0)
              <|> (pure $ Str.length label + 1) -- + 1 for the colon

  usage <- (fromCharArray <<< fromList <$> do
    P.manyTill
      P.anyChar
      ((void $ P.lookAhead optionSection) <|> P.eof)
  ) <?> "usage section"

  let fixedUsage = (fromCharArray $ A.replicate fixColOffset ' ') ++ usage

  options <- many optionSection

  return {
    usage:   dedent fixedUsage
  , options: options
  }

  where
    optionSection :: P.Parser String String
    optionSection = go <?> "option section"
      where
        go = do
          label <- sectionLabel <?> "section label"
          (guard $ endsWith "options" $ toLower label)
            <?> "section label ending in \"options\". E.g.: \"Advanved Options:\""
          fromCharArray <<< fromList <$> do
            P.manyTill
              P.anyChar
              ((void $ P.lookAhead optionSection) <|> P.eof)

    sectionLabel :: P.Parser String String
    sectionLabel = do
      many space
      name <- fromCharArray <$> do
        A.many $ P.noneOf [ '\n', '\r', ':' ]
      P.char ':'
      return name

    endsWith :: String -> String -> Boolean
    endsWith sub s = Str.drop (Str.length s - Str.length sub) s == sub
