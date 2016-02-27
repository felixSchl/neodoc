-- | ARGV lexer
-- |
-- | > Given an argv array, i.e. `process.argv`, derive a stream of tokens
-- | > suitable for parsing against a docopt specification.
-- |
-- | ===

module Docopt.ParserGen.Lexer (lex) where

import Prelude
import Debug.Trace
import Data.List (List(..))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Control.Apply ((*>), (<*))
import Data.String (fromCharArray)
import Data.List (List(..), foldM, many, singleton)
import qualified Text.Parsing.Parser as P
import qualified Text.Parsing.Parser.Combinators as P
import qualified Text.Parsing.Parser.Pos as P
import qualified Text.Parsing.Parser.String as P
import qualified Data.Array as A
import Control.Plus (empty)
import Control.Bind ((=<<))
import Docopt.ParserGen.Types
import Docopt.Parser.Base
import qualified Docopt.Types as D

-- | Parse a single token from the ARGV stream.
-- | Because each item on the ARGV stream is a a string itself, apply a parser
-- | to each item and derive a token.
parseToken :: P.Parser String Token
parseToken = do
  P.choice $ P.try <$> [
    sopt <* P.eof
  , lopt <* P.eof
  , eoa
  , lit  <* P.eof
  ]

  where
    eoa :: P.Parser String Token
    eoa = do
      P.string "--"
      return $ EOA empty

    -- | Parse a short option
    sopt :: P.Parser String Token
    sopt = do
      P.char '-'
      x  <- alphaNum
      xs <- A.many alphaNum
      arg <- P.choice $ P.try <$> [
        Just <$> do
          many space *> P.char '=' <* many space
          fromCharArray <$> do A.many P.anyChar
      , pure Nothing
      ]
      many space
      pure $ SOpt x xs arg

    -- | Parse a long option
    lopt :: P.Parser String Token
    lopt = do
      P.string "--"
      xs <- fromCharArray <$> do
        A.some alphaNum
      arg <- P.choice $ P.try <$> [
        Just <$> do
          many space *> P.char '=' <* many space
          fromCharArray <$> do A.many P.anyChar
      , pure Nothing
      ]
      many space
      pure $ LOpt xs arg

    -- | Parse a literal
    lit :: P.Parser String Token
    lit = Lit <<< fromCharArray <$> do
      A.many P.anyChar

-- | Reduce the array of arguments (argv) to a list of tokens, by parsing each
-- | item individually.
lex :: (List String) -> Either P.ParseError (List Token)
lex xs = step xs
  where
    step Nil = return Nil
    step (Cons x xs) = do
      tok <- P.runParser x parseToken
      case tok of
        (EOA _) -> do
          -- XXX: Each `x` in `xs` should be properly parsed into a `Value`
          return $ singleton $ EOA (D.StringValue <$> xs)
        _ -> do
          toks <- step xs
          return $ singleton tok ++ toks
