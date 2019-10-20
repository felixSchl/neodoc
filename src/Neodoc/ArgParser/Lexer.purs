module Neodoc.ArgParser.Lexer (
  lex
, Options()
) where

import Prelude
import Debug.Trace
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Bifunctor (lmap)
import Data.Optimize.Uncurried
import Control.Apply ((*>), (<*))
import Control.Alt ((<|>))
import Data.List (List(..), (:), singleton, many, fromFoldable, some, toUnfoldable)
import Data.String.CodeUnits (singleton) as String
import Data.Traversable (foldMap)
import Data.Foldable (elem)
import Neodoc.Parsing.Parser as P
import Neodoc.Parsing.Parser (Parser)
import Neodoc.Parsing.Parser.String (StringParser)
import Neodoc.Parsing.Parser.Combinators as P
import Neodoc.Parsing.Parser.Pos as P
import Neodoc.Parsing.Parser.String as P
import Data.Array as A
import Control.Plus (empty)

import Neodoc.ArgParser.Token (PositionedToken(..), Token(..))
import Neodoc.Value (Value(..))

type Options r = {
  stopAt :: Array String
  | r
}

-- | Parser that parses strings
type StringParser' a = StringParser String Unit Unit a

-- | Parse a single token from the ARGV stream.
-- | Because each item on the ARGV stream is a a string itself, apply a parser
-- | to each item and derive a token.
parseToken :: StringParser' Token
parseToken = do
  P.choice $ P.try <$> [
    lopt  <* P.eof
  , sopt  <* P.eof
  , eoa   <* P.eof
  , stdin <* P.eof
  , lit   <* P.eof
  ]

  where
    illegalOptChar :: Array Char
    illegalOptChar = [ '=', ' ', '\t', '\n', '\r' ]

    illegalSOptChar :: Array Char
    illegalSOptChar = '-' A.: illegalOptChar

    stdin :: StringParser' Token
    stdin = do
      _ <- P.char '-'
      pure Stdin

    eoa :: StringParser' Token
    eoa = do
      _ <- P.string "--"
      pure $ EOA empty

    -- | Parse a short option
    sopt :: StringParser' Token
    sopt = do
      _ <- P.char '-'
      x   <- P.noneOf illegalSOptChar
      xs  <- A.many $ P.noneOf illegalOptChar -- re-allow '-'s
      arg <- P.optionMaybe arg
      pure $ SOpt x xs arg

    -- | Parse a long option
    lopt :: StringParser' Token
    lopt = do
      _ <- P.string "--"
      xs <- foldMap String.singleton <$> do
        some $ P.noneOf illegalOptChar
      arg <- P.optionMaybe arg
      pure $ LOpt xs arg

    -- | Parse a literal
    lit :: StringParser' Token
    lit = Lit <<< foldMap String.singleton <$> do
      many P.anyChar

    arg = do
      _ <- P.char '='
      foldMap String.singleton <$> many P.anyChar

-- | Reduce the array of arguments (argv) to a list of tokens, by parsing each
-- | item individually.
lex
  :: ∀ r
   . List String
  -> Options r
  -> Either String (List PositionedToken)
lex xs options = lmap (P.extractError identity) $ go xs 1
  where
    go Nil _ = pure Nil
    go (x:xs) n = do
      let toEOA l = Right
            $ singleton
            $ PositionedToken (EOA (StringValue <$> xs)) x n
      tok <- P.runParser (Args5 unit P.initialPos unit x parseToken)
      case tok of
        (EOA _) -> toEOA "--"
        otherwise -> do
          toks <- go xs (n + 1)
          Right $ singleton (PositionedToken tok x n) <> toks
