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
import Data.List (List(..), (:), fromList, many)
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
  | VBar
  | Colon
  | Equal
  | TripleDot
  | LOpt String (Maybe String)
  | SOpt Char (List Char) (Maybe String)
  | ShoutName String
  | AngleName String
  | Name      String

prettyPrintToken :: Token -> String
prettyPrintToken LParen        = show '('
prettyPrintToken RParen        = show ')'
prettyPrintToken LSquare       = show '['
prettyPrintToken RSquare       = show ']'
prettyPrintToken LAngle        = show '<'
prettyPrintToken RAngle        = show '>'
prettyPrintToken Dash          = show '-'
prettyPrintToken VBar          = show '|'
prettyPrintToken Colon         = show ':'
prettyPrintToken Equal         = show '='
prettyPrintToken TripleDot     = show "..."
prettyPrintToken (Name      n) = "Name "      ++ show n
prettyPrintToken (ShoutName n) = "ShoutName " ++ show n
prettyPrintToken (AngleName n) = "AngleName " ++ show n
prettyPrintToken (LOpt n arg) =
  show $ "--" ++ n
    ++ case arg of
            Just arg -> "=" ++ arg
            Nothing  -> ""
prettyPrintToken (SOpt n stack arg) =
  show $ "-" ++ (fromChar n) ++ (fromCharArray $ fromList stack)
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
  eq VBar          VBar             = true
  eq Colon         Colon            = true
  eq Dash          Dash             = true
  eq Equal         Equal            = true
  eq TripleDot     TripleDot        = true
  eq (AngleName n) (AngleName n')   = n == n'
  eq (ShoutName n) (ShoutName n')   = n == n'
  eq (Name n)      (Name n')        = n == n'
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
  , P.try $ P.char   '|'   *> pure VBar
  , P.try $ P.char   ':'   *> pure Colon
  , P.try $ parseLongOption
  , P.try $ parseShortOption
  , AngleName <$> parseAngleName
  , P.try $ P.char   '<'   *> pure LAngle
  , P.try $ P.char   '>'   *> pure RAngle
  , P.try $ P.char   '-'   *> pure Dash
  , P.try $ P.char   '='   *> pure Equal
  , P.try $ P.string "..." *> pure TripleDot
  , ShoutName <$> parseShoutName
  , Name      <$> parseName
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

  parseShoutName :: P.Parser String String
  parseShoutName = fromCharArray <$> do
    A.cons
      <$> upperAlpha
      <*> (A.many $ regex "[A-Z_-]")

  parseShortOption :: P.Parser String Token
  parseShortOption = P.char '-' *> do
    SOpt
      <$> alphaNum
      <*> (many $ P.try alphaNum)
      <*> ((P.try do
            P.char '='
            Just <$> P.choice [ parseAngleName, parseShoutName, parseName ])
          <|> pure Nothing)

  parseLongOption :: P.Parser String Token
  parseLongOption = P.string "--" *> do
    LOpt
      <$> parseName
      <*> ((P.try do
            P.char '='
            Just <$> P.choice [ parseAngleName, parseShoutName, parseName ])
          <|> pure Nothing)

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

name :: TokenParser String
name = token go P.<?> "name"
  where
    go (Name n) = Just n
    go _        = Nothing

angleName :: TokenParser String
angleName = token go P.<?> "<name>"
  where
    go (AngleName n) = Just n
    go _             = Nothing

shoutName :: TokenParser String
shoutName = token go P.<?> "NAME"
  where
    go (ShoutName n) = Just n
    go _             = Nothing

runTokenParser :: forall a.
                  (List PositionedToken)
                -> TokenParser a
                -> Either P.ParseError a
runTokenParser s =
  flip evalState
  (ParserState { indentation: 0 })
  <<< P.runParserT
  (P.PState { input: s, position: P.initialPos })
