module Docopt.Parsers.Meta.Options where

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
import Docopt.Parsers.Meta.Option

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

type Description = Maybe String
data OptionSpec
  = FullOptionSpec  ShortOption LongOption Description
  | ShortOptionSpec ShortOption Description
  | LongOptionSpec  LongOption Description
