module Docopt.Parser.Lexer where

import Prelude
import Control.Apply ((*>), (<*))
import Control.Alt ((<|>))
import qualified Text.Parsing.Parser as P
import qualified Text.Parsing.Parser.Combinators as P
import qualified Text.Parsing.Parser.Token as P
import qualified Text.Parsing.Parser.Pos as P
import qualified Text.Parsing.Parser.String as P
import qualified Data.List as L
import qualified Data.Array as A
import Data.Char (toString, toLower, toUpper)
import Data.String (fromCharArray)
import Data.List (List(..), (:))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Docopt.Parser.Base

data Token
  = LParen
  | RParen
  | LSquare
  | RSquare
  | LAngle
  | RAngle
  | TripleDot
  | Dash
  | Name String

prettyPrintToken :: Token -> String
prettyPrintToken LParen      = show '('
prettyPrintToken RParen      = show ')'
prettyPrintToken LSquare     = show '['
prettyPrintToken RSquare     = show ']'
prettyPrintToken LAngle      = show '<'
prettyPrintToken RAngle      = show '>'
prettyPrintToken Dash        = show '-'
prettyPrintToken TripleDot   = show "..."
prettyPrintToken (Name name) = show name

data PositionedToken = PositionedToken
  { sourcePos :: P.Position
  , token     :: Token
  }

instance showToken :: Show Token where
  show = show <<< prettyPrintToken

instance eqToken :: Eq Token where
  eq LParen      LParen       = true
  eq RParen      RParen       = true
  eq LSquare     LSquare      = true
  eq RSquare     RSquare      = true
  eq LAngle      LAngle       = true
  eq RAngle      RAngle       = true
  eq Dash        Dash         = true
  eq TripleDot   TripleDot    = true
  eq (Name name) (Name name') = name == name'
  eq _ _                      = false

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
  , P.try $ P.char   '<'   *> pure LAngle
  , P.try $ P.char   '>'   *> pure RAngle
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
  identStart = alpha

  identLetter :: P.Parser String Char
  identLetter = alphaNum <|> P.oneOf ['_', '-']

-- | Parser that  parses a stream of tokens
type TokenParser a = P.Parser (List PositionedToken) a

-- | Test the token at the head of the stream
token :: forall a. (Token -> Maybe a) -> TokenParser a
token test = P.ParserT $ \(P.PState { input: toks, position: pos }) ->
  return $ case toks of
    Cons x@(PositionedToken { token: tok, sourcePos: ppos }) xs ->
      case test tok of
        Just a ->
          let nextpos =
              case xs of
                Cons (PositionedToken { sourcePos: npos }) _ -> npos
                Nil -> ppos
          in
            { consumed: true
            , input: xs
            , result: Right a
            , position: nextpos }
        Nothing -> P.parseFailed toks pos "expected token, met EOF"
    _ -> P.parseFailed toks pos "expected token, met EOF"

-- | Match the token at the head of the stream
match :: forall a. Token -> TokenParser Unit
match tok = token (\tok' -> if (tok' == tok) then Just unit else Nothing)
            P.<?> prettyPrintToken tok
