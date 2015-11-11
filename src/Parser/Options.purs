-- | This module parses the options section of the Docopt source string.
-- |
-- | ```
-- |   -h --help     Show this screen.
-- |   --version     Show version.
-- |   --speed=<kn>  Speed in knots [default: 10].
-- |   --moored      Moored (anchored) mine.
-- |   --drifting    Drifting mine.
-- | ```
-- |
-- | XXX:
-- | Because of the structure of the string, lexing and parsing must be
-- | interwoven to some degree. It would be great, however to re-use the
-- | existing TokenParsers written for the usage section somehow. Maybe
-- | a `pre-lex -> lex -> parse` method is justified? Or just ignore all
-- | unknown tokens.
-- |
-- | Known tokens would be [ SOpt, LOpt, Comma, '--', ':', '[default', ']'
-- |                       , StringLiteral, NumericLiteral ]
-- |
-- | During parsing, we would look for either:
-- |
-- | [ Sopt, Comma, Lopt ]
-- | [ Sopt, Lopt ]
-- | [ Lopt ]
-- | [ -- ]
-- |
-- | All following tokens are then part of the description, mostly garbage.
-- | However, we keep an eye open for '[default' and ']' and the StringLiteral
-- | and NumericLiterals contained in it.

module Docopt.Parser.Options where

import Prelude
import Control.Lazy (defer)
import Control.Alt ((<|>))
import Control.Apply ((*>), (<*))
import Control.MonadPlus (guard)
import Data.List (List(..), some, (:), toList, length, singleton, many)
import qualified Text.Parsing.Parser as P
import qualified Text.Parsing.Parser.Combinators as P
import qualified Text.Parsing.Parser.Pos as P
import qualified Text.Parsing.Parser.String as P
import Data.Either
import Data.Maybe hiding (maybe)
import Data.Generic
import Data.String (toLower)
import Docopt.Parser.Base
import Docopt.Parser.Lexer
import Docopt.Parser.Common
import qualified Data.Array as A

type Argument = String
data ShortOption = ShortOption Char (Maybe Argument)
data LongOption = LongOption String (Maybe Argument)
data OptionSpec = OptionSpec {
  short   :: Maybe ShortOption
, long    :: Maybe LongOption
, default :: Maybe Argument
}
type PartialOptionSpec = (Maybe Argument) -> OptionSpec

derive instance genericOptionSpec  :: Generic OptionSpec
derive instance genericShortOption :: Generic ShortOption
derive instance genericLongOption  :: Generic LongOption

instance showOptionSpec  :: Show OptionSpec  where show = gShow
instance showShortOption :: Show ShortOption where show = gShow
instance showLongOption  :: Show LongOption  where show = gShow

parse :: (List PositionedToken) -> Either P.ParseError Unit
parse = flip runTokenParser optionsParser

optionsParser :: TokenParser Unit
optionsParser = do

  P.Position { column: col } <- getTokenPosition
  x <- markIndent' col do
    partial <- partialOptionSpec
    -- XXX: Add scan for `[defaults: ...]`
    debug "scanning"
    toks <- P.manyTill anyToken $ P.choice [
      P.try $ P.lookAhead $ void partialOptionSpec
    , eof
    ]
    debug toks
    debug "here"




  debug x

  -- as <- markIndent' col do
  --   many $ P.try do
  --     -- XXX: Add scan for `[defaults: ...]`
  --     P.manyTill anyToken $ P.lookAhead do
  --       optionLine
  -- debug as

  where

    anyName :: TokenParser String
    anyName = angleName <|> shoutName <|> name

    defaults :: TokenParser String
    defaults = do
      lsquare
      anyName >>= \n -> guard $ toLower n == "default"
      x <- P.choice
        [ colon *> P.manyTill anyToken rsquare
        , P.manyTill anyToken rsquare
        ]
      debug "x:" *> debug x
      pure "XXX"

    partialOptionSpec :: TokenParser PartialOptionSpec
    partialOptionSpec = sameIndent *> do
      opt <- P.choice
        [ P.try shortAndLong
        , P.try onlyLong
        , P.try onlyShort
        ]
      pure opt

    onlyShort :: TokenParser PartialOptionSpec
    onlyShort = do
      short <- shortOption
      return \default ->
        OptionSpec {
          short:   Just short
        , long:    Nothing
        , default: default
        }

    onlyLong :: TokenParser PartialOptionSpec
    onlyLong = do
      long <- parseLongOption
      return \default ->
        OptionSpec {
          short:   Nothing
        , long:    Just long
        , default: default
        }

    shortAndLong :: TokenParser PartialOptionSpec
    shortAndLong = do
      getInput >>= debug
      short <- shortOption
      long <- P.choice
        [ P.try comma *> parseLongOption
        , parseLongOption
        ]
      return \default->
        OptionSpec {
          short:   Just short
        , long:    Just long
        , default: default
        }

    shortOption :: TokenParser ShortOption
    shortOption = do
      { flag: flag, stack: stack, arg: arg } <- sopt
      (guard $ (A.length stack == 0))
        P.<?> "No stacked options"
      return $ ShortOption flag arg

    parseLongOption :: TokenParser LongOption
    parseLongOption = do
      { name: name, arg: arg } <- lopt
      return $ LongOption name arg
