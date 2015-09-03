module Docopt.Parsers.Meta.Usage where

import Prelude
import Control.Lazy (defer)
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
import Text.Parsing.Parser.String (char, string, satisfy, eof, skipSpaces
                                  , whiteSpace, anyChar, noneOf)
import Text.Parsing.Parser.Combinators (try, sepBy, between, sepBy1)
import Data.Char (toString, toUpper)
import Data.String (charAt, fromChar, fromCharArray)
import Data.Maybe
import Data.Either
import qualified Data.List as List
import qualified Data.Array as Array
import Data.List (List(..), concat, toList, many, some)
import Docopt.Parsers.Base
import Docopt.Parsers.Meta.Option
import Docopt.Parsers.Meta.Positional
import Docopt.Parsers.Meta.Command

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

data UsageToken
  = UsageTokenCommand       Command
  | UsageTokenPositional    Positional
  | UsageTokenLongOption    LongOption
  | UsageTokenShortOption   ShortOption
  | UsageTokenGroupOptional (List (List UsageToken))

type Usage = List UsageToken
type UsageBlock = List Usage

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance showUsageToken :: Show UsageToken where
  show (UsageTokenCommand tok)       = "UsageTokenCommand "       ++ show tok
  show (UsageTokenPositional tok)    = "UsageTokenPositional "    ++ show tok
  show (UsageTokenLongOption tok)    = "UsageTokenLongOption "    ++ show tok
  show (UsageTokenShortOption tok)   = "UsageTokenShortOption "   ++ show tok
  show (UsageTokenGroupOptional tok) = "UsageTokenGroupOptional " ++ show tok

-- | Parse any valid usage token
-- |
-- | A usage token is any valid argument, option or positional.
-- |
usageToken :: Parser String UsageToken
usageToken = defer \_ -> do
      (UsageTokenLongOption    <$> longOption)
  <|> (UsageTokenShortOption   <$> (shortOption true))
  <|> (UsageTokenPositional    <$> positional)
  <|> (UsageTokenGroupOptional <$> (do
        between
          (char '[')
          (char ']')
          (flip sepBy
            (char '|')
            (many $ try do
              many space *> usageToken <* many space))))

-- | Parse a `Usage line`
usageLine :: String -> Parser String Usage
usageLine program = do
  string program <* many space
  Position { column: col } <- getPosition
  concat <$> ((:)
    <$> (usageToken `sepBy` many space)
    -- XXX: Is this too loose?
    <*> (many $ try do
          many space *> eol
          indent (col - 1)
          many space
          usageToken `sepBy` many space))

-- | Parse a complete usage block
-- |
-- | ```
-- | Usage:
-- | / naval-fate -vvv --qux
-- | //////////// --foo --bar
-- | / naval-fate --debug
-- | ```
-- |
-- | TODO: Add support for the following notation:
-- |
-- | ```
-- | Usage: naval-fate -vvv --qux
-- | ///////////////// --foo --bar
-- | ////// naval-fate --debug
-- | ```
-- |
usageBlock :: String -> Parser String UsageBlock
usageBlock program = do
  string "Usage:" <* chompRight
  many space
  Position { column: col } <- getPosition
  (:)
    <$> (usageLine program)
    <*> (many $ try do
          eol
          indent (col - 1)
          usageLine program <* many space)
