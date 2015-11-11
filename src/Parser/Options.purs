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
import Data.List (
  List(..), some, (:), toList, length
, singleton, many, head, catMaybes)
import qualified Text.Parsing.Parser as P
import qualified Text.Parsing.Parser.Combinators as P
import qualified Text.Parsing.Parser.Pos as P
import qualified Text.Parsing.Parser.String as P
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe, isJust)
import Data.Generic
import Data.String (toLower, fromChar)
import qualified Data.Array as A

import Docopt.Parser.Base
import Docopt.Parser.Common
import qualified Docopt.Parser.Lexer as L

type Argument = String
type Default = String
data ShortOption = ShortOption Char (Maybe Argument)
data LongOption = LongOption String (Maybe Argument)
data Option = Option
                (Maybe ShortOption)
                (Maybe LongOption)
                (Maybe Default)
type PartialOption = (Maybe Argument) -> Option

derive instance genericOptionSpec  :: Generic Option
derive instance genericShortOption :: Generic ShortOption
derive instance genericLongOption  :: Generic LongOption

instance showShortOption :: Show ShortOption where
  show (ShortOption c a) = "-" ++ (fromChar c)
                               ++ (case a of
                                        Just a -> "=" ++ a
                                        _      -> "")

instance showlongOption :: Show LongOption where
  show (LongOption s a) = "--" ++ s
                               ++ (case a of
                                        Just a -> "=" ++ a
                                        _      -> "")

instance showOptionSpec  :: Show Option where
  show (Option s l d) =
    let hasLong    = isJust l
        hasShort   = isJust s
        hasDefault = isJust d
        short      = maybe "" show s
        long       = maybe "" (\x -> (if hasShort then ", " else "") ++ show x) l
        default    = maybe "" (\x -> " [" ++ x ++ "]") d
     in show $ short ++ long ++ default

parse :: (List L.PositionedToken) -> Either P.ParseError Unit
parse = flip L.runTokenParser optionsParser

optionsParser :: L.TokenParser Unit
optionsParser = do

  P.Position { column: col } <- getTokenPosition
  x <- markIndent' col do
    A.many do
      option' <- option
      pure option'
  debug x

  where

    anyName :: L.TokenParser String
    anyName = L.angleName <|> L.shoutName <|> L.name

    defaults :: L.TokenParser String
    defaults = L.default

    option :: L.TokenParser Option
    option = sameIndent *> do
      f <- P.choice
        [ P.try shortAndLong
        , P.try onlyLong
        , P.try onlyShort
        ]

      -- Parse one token at a time towards the next option or the eof.
      -- If a `[default: ...]` token is met, list it.
      default <- head <<< catMaybes <$> do
        flip P.manyTill (L.eof <|> (P.try $ P.lookAhead $ void option)) do
          P.choice [
            P.try $ Just <$> defaults
          , L.anyToken *> pure Nothing
          ]
      pure $ f default

    onlyShort :: L.TokenParser PartialOption
    onlyShort = Option
        <$> (Just <$> shortOption)
        <*> (pure Nothing)

    onlyLong :: L.TokenParser PartialOption
    onlyLong = Option
        <$> (pure Nothing)
        <*> (Just <$> longOption)

    shortAndLong :: L.TokenParser PartialOption
    shortAndLong = markLine do
      Option
        <$> (Just <$> shortOption)
        <*> (Just <$> (sameLine *> P.choice
              [ P.try L.comma *> longOption
              , longOption
              ]))

    shortOption :: L.TokenParser ShortOption
    shortOption = do
      { flag: flag, stack: stack, arg: arg } <- L.sopt
      (guard $ (A.length stack == 0))
        P.<?> "No stacked options"
      return $ ShortOption flag arg

    longOption :: L.TokenParser LongOption
    longOption = do
      { name: name, arg: arg } <- L.lopt
      return $ LongOption name arg
