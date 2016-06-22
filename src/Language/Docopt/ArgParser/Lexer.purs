-- | ARGV lexer
-- |
-- | > Given an argv array, i.e. `process.argv`, derive a stream of tokens
-- | > suitable for parsing against a docopt specification.
-- |
-- | ===

module Language.Docopt.ArgParser.Lexer (
    lex
  , Options()
  ) where

import Prelude
import Debug.Trace
import Data.Either (Either())
import Data.Maybe (Maybe(..))
import Control.Apply ((*>), (<*))
import Control.Alt ((<|>))
import Data.String (fromCharArray)
import Data.List (List(..), singleton, many)
import Data.Foldable (elem)
import Text.Parsing.Parser (ParseError, Parser, runParser) as P
import Text.Parsing.Parser.Combinators (try, choice, optional, optionMaybe) as P
import Text.Parsing.Parser.Pos (Position(Position)) as P
import Text.Parsing.Parser.String (eof, anyChar, char, oneOf, noneOf, string) as P
import Data.Array as A
import Control.Plus (empty)
import Language.Docopt.ArgParser.Token (PositionedToken(..), Token(..))
import Language.Docopt.SpecParser.Base (space, alphaNum)
import Language.Docopt.Value (Value(..)) as D

type Options r = {
  stopAt :: Array String
  | r
}

-- | Parse a single token from the ARGV stream.
-- | Because each item on the ARGV stream is a a string itself, apply a parser
-- | to each item and derive a token.
parseToken :: P.Parser String Token
parseToken = do
  P.choice $ P.try <$> [
    stdin <* P.eof
  , sopt  <* P.eof
  , lopt  <* P.eof
  , eoa   <* P.eof
  , lit   <* P.eof
  ]

  where
    stdin :: P.Parser String Token
    stdin = do
      P.char '-'
      P.eof
      pure $ Stdin

    eoa :: P.Parser String Token
    eoa = do
      P.string "--"
      P.eof
      pure $ EOA empty

    -- | Parse a short option
    sopt :: P.Parser String Token
    sopt = do
      P.char '-'
      x   <- alphaNum
      xs  <- A.many $ P.noneOf [ '=' ]
      arg <- P.optionMaybe arg
      P.eof
      pure $ SOpt x xs arg

    -- | Parse a long option
    lopt :: P.Parser String Token
    lopt = do
      P.string "--"
      xs <- fromCharArray <$> do
        A.some $ P.noneOf [ '=', ' ' ]
      arg <- P.optionMaybe arg
      P.eof
      pure $ LOpt xs arg

    -- | Parse a literal
    lit :: P.Parser String Token
    lit = Lit <<< fromCharArray <$> do
      A.many P.anyChar

    arg = do
      P.char '='
      fromCharArray <$> A.many P.anyChar

-- | Reduce the array of arguments (argv) to a list of tokens, by parsing each
-- | item individually.
lex
  :: forall r
   . List String
  -> Options r
  -> Either P.ParseError (List PositionedToken)
lex xs options = go xs 1
  where
    go Nil _ = pure Nil
    go (Cons x xs) n = do
      let toEOA l = pure $ singleton $ PositionedToken {
            token:     EOA (D.StringValue <$> xs)
          , sourcePos: P.Position { line: 1, column: n }
          , source:    x
          }

      tok <- P.runParser x parseToken
      case tok of
        (EOA  _) -> toEOA "--"
        otherwise -> do
          toks <- go xs (n + 1)
          pure
            $ singleton (PositionedToken {
                          token:     tok
                        , sourcePos: P.Position { line: 1, column: n }
                        , source:    x
                        }) <> toks
