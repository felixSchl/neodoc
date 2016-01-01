module Docopt.Spec.Parser.Options where

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

import Docopt.Spec.Parser.Base
import Docopt.Spec.Parser.Common
import qualified Docopt.Spec.Parser.Lexer as L

type Argument    = String
type Default     = String
data ShortOption = ShortOption Char  (Maybe Argument)
data LongOption  = LongOption String (Maybe Argument)

-- XXX: Desc should have a `Maybe Argument`, pulled
--      from it's short and long option.
--      Need to decide what happens if both options
--      specifiy a different argument!
-- XXX: This type should be called `Desc`
data Desc = Desc (Maybe ShortOption)
                     (Maybe LongOption)
                     (Maybe Default)

type PartialDesc = (Maybe Argument) -> Desc

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

instance showOptionSpec  :: Show Desc where
  show (Desc s l d) =
    let hasLong    = isJust l
        hasShort   = isJust s
        hasDefault = isJust d
        short      = maybe "" show s
        long       = maybe "" (\x -> (if hasShort then ", " else "") ++ show x) l
        default    = maybe "" (\x -> " [" ++ x ++ "]") d
     in show $ short ++ long ++ default

parse :: (List L.PositionedToken) -> Either P.ParseError (List Desc)
parse = flip L.runTokenParser optionsParser

optionsParser :: L.TokenParser (List Desc)
optionsParser = do

  many option

  where

    anyName :: L.TokenParser String
    anyName = L.angleName <|> L.shoutName <|> L.name

    defaults :: L.TokenParser String
    defaults = L.default

    option :: L.TokenParser Desc
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

    onlyShort :: L.TokenParser PartialDesc
    onlyShort = Desc
        <$> (Just <$> shortOption)
        <*> (pure Nothing)

    onlyLong :: L.TokenParser PartialDesc
    onlyLong = Desc
        <$> (pure Nothing)
        <*> (Just <$> longOption)

    shortAndLong :: L.TokenParser PartialDesc
    shortAndLong = markLine do
      Desc
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
