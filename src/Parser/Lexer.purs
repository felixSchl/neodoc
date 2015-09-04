module Docopt.Parser.Lexer where

import Prelude
import Control.Apply ((*>), (<*))
import Control.Alt ((<|>))
import qualified Text.Parsing.Parser as P
import qualified Text.Parsing.Parser.Combinators as P
import qualified Text.Parsing.Parser.Pos as P
import qualified Text.Parsing.Parser.String as P
import qualified Data.List as L
import qualified Data.Array as A
import Data.Char (toString, toLower, toUpper)
import Data.String (fromCharArray)
import Data.List ((:))
import Docopt.Parser.Base

data Token
  = LParen
  | RParen
  | LSquare
  | RSquare
  | TripleDot
  | Dash
  | Name String

prettyPrintToken :: Token -> String
prettyPrintToken LParen      = "("
prettyPrintToken RParen      = ")"
prettyPrintToken LSquare     = "["
prettyPrintToken RSquare     = "]"
prettyPrintToken Dash        = "-"
prettyPrintToken TripleDot   = "..."
prettyPrintToken (Name name) = name

data PositionedToken = PositionedToken
  { sourcePos :: P.Position
  , token     :: Token
  }

instance showToken :: Show Token where
  show = show <<< prettyPrintToken

instance showPositionedToken :: Show PositionedToken where
  show (PositionedToken { sourcePos=pos, token=tok }) =
    (show tok) ++ " at " ++ (show pos)

parseTokens :: P.Parser String (L.List PositionedToken)
parseTokens = do
  P.skipSpaces
  L.many parsePositionedToken
  <* P.eof

parsePositionedToken :: P.Parser String PositionedToken
parsePositionedToken = P.try $ do
  pos <- getPosition
  tok <- parseToken
  return $ PositionedToken { sourcePos: pos, token: tok }

parseToken :: P.Parser String Token
parseToken = P.choice
  [ P.try $ P.char   '('   *> pure LParen
  , P.try $ P.char   ')'   *> pure RParen
  , P.try $ P.char   '['   *> pure LSquare
  , P.try $ P.char   ']'   *> pure RSquare
  , P.try $ P.char   '-'   *> pure Dash
  , P.try $ P.string "..." *> pure TripleDot
  , Name <$> parseName
  ] <* P.skipSpaces

 where
  parseName :: P.Parser String String
  parseName = fromCharArray <$> do
    A.cons
      <$> identStart
      <*> A.many identLetter

  identStart :: P.Parser String Char
  identStart = lower <|> P.oneOf ['_']

  identLetter :: P.Parser String Char
  identLetter = alphaNum <|> P.oneOf ['_', '\'']
