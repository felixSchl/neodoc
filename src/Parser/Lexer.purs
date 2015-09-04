module Docopt.Parser.Lexer where

import Prelude
import Control.Apply ((*>), (<*))
import Control.Alt ((<|>))
import Control.Monad.State (State(), evalState, get)
import Control.Monad.Trans
import qualified Text.Parsing.Parser as P
import qualified Text.Parsing.Parser.Combinators as P
import qualified Text.Parsing.Parser.Token as P
import qualified Text.Parsing.Parser.Pos as P
import qualified Text.Parsing.Parser.String as P
import qualified Data.List as L
import qualified Data.Array as A
import Data.Char (toString, toLower, toUpper)
import Data.String (fromCharArray, fromChar)
import Data.List (List(..), (:))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Docopt.Parser.Base
import Docopt.Parser.State

data Token
  = LParen
  | RParen
  | LSquare
  | RSquare
  | LAngle
  | RAngle
  | Dash
  | Equal
  | TripleDot
  | LOpt String (Maybe String)
  | SOpt Char (Array Char) (Maybe String)
  | PosOpt String
  | CmdOpt String

prettyPrintToken :: Token -> String
prettyPrintToken LParen        = show '('
prettyPrintToken RParen        = show ')'
prettyPrintToken LSquare       = show '['
prettyPrintToken RSquare       = show ']'
prettyPrintToken LAngle        = show '<'
prettyPrintToken RAngle        = show '>'
prettyPrintToken Dash          = show '-'
prettyPrintToken Equal         = show '='
prettyPrintToken TripleDot     = show "..."
prettyPrintToken (PosOpt name) = show name
prettyPrintToken (CmdOpt name) = show name
prettyPrintToken (LOpt name arg) =
  show $ "--" ++ name
    ++ case arg of
            Just arg -> "=" ++ arg
            Nothing  -> ""
prettyPrintToken (SOpt name stack arg) =
  show $ "-" ++ (fromChar name) ++ (fromCharArray stack)
    ++ case arg of
            Just arg -> "=" ++ arg
            Nothing  -> ""

data PositionedToken = PositionedToken
  { sourcePos :: P.Position
  , token     :: Token
  }

instance showToken :: Show Token where
  show = show <<< prettyPrintToken

instance eqToken :: Eq Token where
  eq LParen        LParen           = true
  eq RParen        RParen           = true
  eq LSquare       LSquare          = true
  eq RSquare       RSquare          = true
  eq LAngle        LAngle           = true
  eq RAngle        RAngle           = true
  eq Dash          Dash             = true
  eq Equal         Equal            = true
  eq TripleDot     TripleDot        = true
  eq (PosOpt n)    (PosOpt n')      = n == n'
  eq (CmdOpt n)    (CmdOpt n')      = n == n'
  eq (LOpt n a)    (LOpt n' a')     = (n == n') && (a == a')
  eq (SOpt n ns a) (SOpt n' ns' a') = (n == n') && (ns' == ns') && (a == a')
  eq _ _                            = false

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
  , PosOpt <$> parsePosOpt
  , CmdOpt <$> parseCmdOpt
  , P.try $ P.char   '<'   *> pure LAngle
  , P.try $ P.char   '>'   *> pure RAngle
  , P.try $ P.char   '-'   *> pure Dash
  , P.try $ P.char   '='   *> pure Equal
  , P.try $ P.string "..." *> pure TripleDot
  ] <* P.skipSpaces

 where

  parseName :: P.Parser String String
  parseName = fromCharArray <$> do
    A.cons
      <$> identStart
      <*> A.many identLetter

  parseAngleName :: P.Parser String String
  parseAngleName =
    P.char '<' *> parseName <* P.char '>'

  parseUpperName :: P.Parser String String
  parseUpperName = fromCharArray <$> do
    A.cons
      <$> upperAlpha
      <*> (A.many $ regex "[A-Z_-]")

  parsePosOpt = parseUpperName <|> parseAngleName

  identStart :: P.Parser String Char
  identStart = alpha

  identLetter :: P.Parser String Char
  identLetter = alphaNum <|> P.oneOf ['_', '-']

-- | Parser that  parses a stream of tokens
type TokenParser a = P.ParserT (List PositionedToken) (State ParserState) a

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

lparen :: TokenParser Unit
lparen = match LParen

rparen :: TokenParser Unit
rparen = match RParen

lsquare :: TokenParser Unit
lsquare = match LSquare

rsquare :: TokenParser Unit
rsquare = match RSquare

langle :: TokenParser Unit
langle = match LAngle

rangle :: TokenParser Unit
rangle = match RAngle

dash :: TokenParser Unit
dash = match Dash

tripleDot :: TokenParser Unit
tripleDot = match TripleDot

equal :: TokenParser Unit
equal = match Equal

runTokenParser :: forall a.
                  (List PositionedToken)
                -> TokenParser a
                -> Either P.ParseError a
runTokenParser s =
  flip evalState
  (ParserState { indentation: 0 })
  <<< P.runParserT
  (P.PState { input: s, position: P.initialPos })
