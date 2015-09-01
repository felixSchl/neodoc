module Docopt.Parsers.Base where

import Prelude
import Data.Monoid (mempty)
import Control.Apply ((*>), (<*))
import Data.Traversable (for, traverse)
import Debug.Trace (traceShow)
import Control.Monad
import Control.Alt ((<|>))
import Control.Monad.Eff.Console (log)
import Text.Parsing.Parser (Parser(), ParserT(..), PState(..))
import Text.Parsing.Parser.Pos (Position(..))
import Data.List (List(), (:))
import Text.Parsing.Parser.String (char, string, satisfy, eof, skipSpaces, whiteSpace)
import Text.Parsing.Parser.Combinators (try, sepBy)
import Data.Char (toString, toUpper)
import Data.String (charAt, fromChar, fromCharArray)
import Data.Maybe
import Data.Either
import qualified Data.List as List
import qualified Data.Array as Array
import Data.List (List(..), concat, toList, many)
import Docopt.Parsers.Base
import qualified Data.String.Regex as Regex

-- Trace `x` and return noop parser
debug :: forall a b. (Show a) => a -> Parser b Unit
debug x = traceShow x \_ -> return unit

-- Match a single space character
space :: Parser String Unit
space = char ' ' *> return unit

-- | Match a char against a regex
matches :: String -> Parser String Char
matches s = satisfy \c ->
            Regex.test (Regex.regex s Regex.noFlags) (toString c)

-- | Parse the end-of-line
eol :: Parser String Unit
eol = ((char '\r' *> char '\n') <|> char '\n') *> return unit

-- | Return the current parser position
getPosition :: forall a. Parser a Position
getPosition = ParserT $ \(PState { input: s, position: pos }) ->
  return { input: s, result: Right pos, consumed: true, position: pos }

-- | Ignore all whitespace until the EOL
chompRight :: Parser String Unit
chompRight = (many space) *> (eol <|> eof) *> return unit

-- | Chomp characters from the left
indent :: Int -> Parser String Unit
indent n = (for (List.range 1 n) (const space)) *> return unit

-- | Trace the current parser position
debugPosition = debug <$> getPosition
