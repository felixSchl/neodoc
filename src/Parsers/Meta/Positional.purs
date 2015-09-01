module Docopt.Parsers.Meta.Positional where

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
import Docopt.Parsers.Meta.Argname

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------
data Positional  = Positional String

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance showPositional :: Show Positional where
  show (Positional s) = "Positional " ++ s

--------------------------------------------------------------------------------
-- Parsers
--------------------------------------------------------------------------------

-- | Parse a positional argument
-- |
-- | A positional argument is a simple identifier of the shape:
-- | ```bash
-- | naval-fate <foo>
-- | naval-fata FOO
-- | ```
-- |
positional :: Parser String Positional
positional = Positional <$> (_ARGNAME <|> _argname_)
