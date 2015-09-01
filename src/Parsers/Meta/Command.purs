module Docopt.Parsers.Meta.Command where

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

data Command = Command String

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance showCommand :: Show Command where
  show (Command s) = "Command " ++ s

--------------------------------------------------------------------------------
-- Parsers
--------------------------------------------------------------------------------

-- | Parse a command
-- |
-- | A command is a simple identifier of the shape:
-- |
-- | ```bash
-- | naval-fata foo
-- | ```
-- |
command :: Parser String Command
command = (<$>) Command $ do
  fromCharArray <$> (Array.cons
    <$> (matches "[a-z]")
    <*> (Array.many $ matches "[a-z]"))
