module Docopt.Parser.Lexer where

import Debug.Trace
import Prelude
import Control.Apply ((*>), (<*))
import Control.Alt ((<|>))
import Control.Plus (empty)
import Control.Monad.State (State(), evalState, get)
import Control.Monad.Trans
import Data.Foldable (foldl)
import qualified Text.Parsing.Parser as P
import qualified Text.Parsing.Parser.Combinators as P
import qualified Text.Parsing.Parser.Token as P
import qualified Text.Parsing.Parser.Pos as P
import qualified Text.Parsing.Parser.String as P
import qualified Data.List as L
import qualified Data.Array as A
import Data.Char (toString, toLower, toUpper)
import Data.String (fromCharArray, fromChar, trim)
import Data.List (List(..), (:), fromList, many)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Docopt.Parser.Base
import Docopt.Parser.State
import Data.Tuple
import Data.Int
import qualified Data.Int as I
import Global (isNaN, readFloat)

lex :: String -> Either P.ParseError (List PositionedToken)
lex = flip P.runParser parseTokens

data NumberLiteral = FloatLiteral Number | IntLiteral Int

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
  | Comma
  | Equal
  | TripleDot
  | LOpt String (Maybe String)
  | SOpt Char (Array Char) (Maybe String)
  | Default String
  | Name String
  | ShoutName String
  | AngleName String
  | Word String
  | StringLiteral String
  | NumberLiteral NumberLiteral
  | Garbage Char
  | DoubleDash

prettyPrintToken :: Token -> String
prettyPrintToken LParen            = show '('
prettyPrintToken RParen            = show ')'
prettyPrintToken LSquare           = show '['
prettyPrintToken RSquare           = show ']'
prettyPrintToken LAngle            = show '<'
prettyPrintToken RAngle            = show '>'
prettyPrintToken Dash              = show '-'
prettyPrintToken VBar              = show '|'
prettyPrintToken Colon             = show ':'
prettyPrintToken Comma             = show ','
prettyPrintToken Equal             = show '='
prettyPrintToken TripleDot         = "..."
prettyPrintToken DoubleDash        = "--"
prettyPrintToken (Garbage   c)     = "Garbage "   ++ show c
prettyPrintToken (Default   s)     = "Default "   ++ s
prettyPrintToken (Name      n)     = "Name "      ++ show n
prettyPrintToken (ShoutName n)     = "ShoutName " ++ show n
prettyPrintToken (AngleName n)     = "AngleName " ++ show n
prettyPrintToken (StringLiteral s) = "String "    ++ show s
prettyPrintToken (NumberLiteral n) = "Number "    ++ show n
prettyPrintToken (Word w)          = "Word "      ++ show w
prettyPrintToken (LOpt n a)        = "--" ++ n
                                          ++ " " ++ (show a)
prettyPrintToken (SOpt n s a)      = "-"  ++ (fromCharArray (A.cons n s))
                                          ++ " " ++ (show a)

data PositionedToken = PositionedToken
  { sourcePos :: P.Position
  , token     :: Token
  }

instance eqNumberLiteral :: Eq NumberLiteral where
  eq (FloatLiteral n) (FloatLiteral n') = eq n n'
  eq (IntLiteral n)   (IntLiteral n')   = eq n n'

instance showNumberLiteral :: Show NumberLiteral where
  show (FloatLiteral n) = show n
  show (IntLiteral n)   = show n

instance showToken :: Show Token where
  show = show <<< prettyPrintToken

instance eqToken :: Eq Token where
  eq LParen            LParen             = true
  eq RParen            RParen             = true
  eq LSquare           LSquare            = true
  eq RSquare           RSquare            = true
  eq LAngle            LAngle             = true
  eq RAngle            RAngle             = true
  eq VBar              VBar               = true
  eq Colon             Colon              = true
  eq Comma             Comma              = true
  eq Dash              Dash               = true
  eq Equal             Equal              = true
  eq DoubleDash        DoubleDash         = true
  eq TripleDot         TripleDot          = true
  eq (LOpt n a)        (LOpt n' a')       = (n == n') && (a == a')
  eq (SOpt n s a)      (SOpt n' s' a')    = (n == n') && (s' == s') && (a == a')
  eq (StringLiteral s) (StringLiteral s') = s == s'
  eq (AngleName n)     (AngleName n')     = n == n'
  eq (ShoutName n)     (ShoutName n')     = n == n'
  eq (Word w)          (Word w')          = w == w'
  eq (Name n)          (Name n')          = n == n'
  eq (Garbage c)       (Garbage c')       = c == c'
  eq (NumberLiteral n) (NumberLiteral n') = n == n'
  eq _ _                                  = false

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
  , P.try $ default
  , P.try $ P.char   '['   *> pure LSquare
  , P.try $ P.char   ']'   *> pure RSquare
  , P.try $ P.char   '|'   *> pure VBar
  , P.try $ P.char   ':'   *> pure Colon
  , P.try $ P.char   ','   *> pure Comma
  , P.try $ longOption
  , P.try $ shortOption
  , P.try $ AngleName <$> angleName
  , P.try $ P.char   '<'   *> pure LAngle
  , P.try $ P.char   '>'   *> pure RAngle
  , P.try $ P.string "--"  *> pure DoubleDash
  , P.try $ P.char   '-'   *> pure Dash
  , P.try $ P.char   '='   *> pure Equal
  , P.try $ P.string "..." *> pure TripleDot
  , P.try $ numberLiteral
  , P.try $ stringLiteral
  , P.try $ ShoutName <$> (shoutName <* P.notFollowedBy alpha)
  , P.try $ Name      <$> (name      <* P.notFollowedBy alpha)
  , P.try $ Word      <$> (word      <* space)
  , P.try $ Garbage   <$> P.anyChar
  ]
  <* P.skipSpaces

 where

  whitespace :: P.Parser String Unit
  whitespace = do
    P.satisfy \c -> c == '\n' || c == '\r' || c == ' ' || c == '\t'
    pure unit

  default :: P.Parser String Token
  default = Default <<< trim <<< fromCharArray <$>  do
    P.between
      (P.char '[')
      (P.char ']')
      do
        A.many whitespace
        string' "default"
        (void $ P.try $ P.char ':') <|> pure unit
        val <- A.some $ P.noneOf [']']
        A.many whitespace
        pure val

  stringLiteral :: P.Parser String Token
  stringLiteral = StringLiteral <$> do
    P.choice [
      P.between (P.char '\'') (P.char '\'') (p '\'')
    , P.between (P.char '"') (P.char '"') (p '"')
    ]
    where
      p :: Char -> P.Parser String String
      p c = fromCharArray <$> do
        A.many $ P.noneOf [c]

  numberLiteral :: P.Parser String Token
  numberLiteral = NumberLiteral <$> do
    P.choice [
      P.try $ FloatLiteral <$> floatLiteral
    , P.try $ IntLiteral   <$> intLiteral
    ]
    where
      floatLiteral = do
        x <- fromCharArray <$> (A.some num)
        P.char '.'
        xs <- fromCharArray <$> (A.some num)

        -- there is no "safe" version yet, afaik
        let n = readFloat $ x ++ "." ++ xs
        if isNaN n
           then P.fail "Could not parse float"
           else pure n

      intLiteral = do
        x <- fromCharArray <$> (A.some num)
        case (I.fromString x) of
          Just n  -> return n
          Nothing -> P.fail "Could not parse integer"

  name :: P.Parser String String
  name = fromCharArray <$> do
    A.cons
      <$> identStart
      <*> A.many identLetter

  word :: P.Parser String String
  word = fromCharArray <$> do
    (A.some $ P.choice [
      identLetter
    , P.oneOf [ '.', '-', '_' ]
    ])

  angleName :: P.Parser String String
  angleName = do
    P.char '<'
    name <- fromCharArray <$> do
      A.some $ P.choice [
        identLetter
      , P.oneOf [ '-', '|', ':', '[', ']', '(', ')' ]
      ]
    P.char '>'
    pure name

  shoutName :: P.Parser String String
  shoutName = do
    name <- fromCharArray <$> do
      A.cons
        <$> upperAlpha
        <*> (A.many $ regex "[A-Z_-]")
    P.notFollowedBy lowerAlpha
    pure name

  shortOption :: P.Parser String Token
  shortOption = do
    P.char '-'
    x  <- alphaNum
    xs <- A.many alphaNum
    arg <- P.choice [

      -- Parse <flag>=<value>, i.e.: `-foo=bar`
      P.try $ Just <$> do
        -- XXX: Drop the spaces?
        many space *> P.char '=' <* many space
        P.choice [
          P.try angleName
        , P.try shoutName
        , P.try name
        ]

      -- Parse <flag><VALUE>, i.e.: `-foo<bar>`
    , P.try $ Just <$> angleName

      -- Otherwise assume no argument given. We might be proven wrong at a later
      -- stage (parsing / solving), but as far as the lexer is concerned, this
      -- token bears no arguments.
    , pure Nothing
    ]

    -- Ensure the argument is correctly bounded
    P.lookAhead $ P.choice [
      P.eof
    , P.try $ void $ P.whiteSpace
    , P.try $ void $ P.char ','
    , P.try $ void $ P.char '|'
    , P.try $ void $ P.char '['
    , P.try $ void $ P.char '('
    , P.try $ void $ P.string "..."
    ]

    pure $ SOpt x xs arg

  longOption :: P.Parser String Token
  longOption = do
    P.string "--"
    name' <- fromCharArray <$> do
      A.cons
        <$> alphaNum
        <*> (A.many $ P.choice [
              alphaNum
            , P.oneOf [ '-' ] <* P.lookAhead alphaNum
            ])
    arg <- P.choice [

      -- Parse <flag>=<value>, i.e.: `--foo=bar`
      P.try $ Just <$> do
        -- XXX: Drop the spaces?
        many space *> P.char '=' <* many space
        P.choice [
          P.try angleName
        , P.try shoutName
        , P.try name
        ]

      -- Parse <flag><VALUE>, i.e.: `-foo<bar>`
    , P.try $ Just <$> angleName

      -- Otherwise assume no argument given. We might be proven wrong at a later
      -- stage (parsing / solving), but as far as the lexer is concerned, this
      -- token bears no arguments.
    , pure Nothing
    ]

    -- Ensure the argument is correctly bounded
    P.lookAhead $ P.choice [
      P.eof
    , P.try $ void $ P.whiteSpace
    , P.try $ void $ P.char ','
    , P.try $ void $ P.char '|'
    , P.try $ void $ P.char '['
    , P.try $ void $ P.char '('
    , P.try $ void $ P.string "..."
    ]

    pure $ LOpt name' arg

  identStart :: P.Parser String Char
  identStart = alpha

  identLetter :: P.Parser String Char
  identLetter = alphaNum <|> P.oneOf ['_', '-']

  flag :: P.Parser String Char
  flag = lowerAlphaNum

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
        -- XXX: Fix this error message, it makes no sense!
        Nothing -> P.parseFailed toks pos "a better error message!"
    _ -> P.parseFailed toks pos "expected token, met EOF"

-- | Match the token at the head of the stream
match :: forall a. Token -> TokenParser Unit
match tok = token (\tok' -> if (tok' == tok) then Just unit else Nothing)
            P.<?> prettyPrintToken tok

anyToken = token $ Just

eof :: TokenParser Unit
eof = P.notFollowedBy anyToken

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

doubleDash :: TokenParser Unit
doubleDash = match DoubleDash

vbar :: TokenParser Unit
vbar = match VBar

comma :: TokenParser Unit
comma = match Comma

colon :: TokenParser Unit
colon = match Colon

tripleDot :: TokenParser Unit
tripleDot = match TripleDot

equal :: TokenParser Unit
equal = match Equal

garbage :: TokenParser Unit
garbage = token go P.<?> "garbage"
  where
    go (Garbage _) = Just unit
    go _           = Nothing

lopt :: TokenParser { name :: String
                    , arg  :: Maybe String }
lopt = token go P.<?> "long-option"
  where
    go (LOpt n a) = Just { name: n, arg: a }
    go _          = Nothing

sopt :: TokenParser { flag  :: Char
                    , stack :: Array Char
                    , arg   :: Maybe String }
sopt = token go P.<?> "short-option"
  where
    go (SOpt n s a) = Just { flag: n , stack: s , arg: a }
    go _ = Nothing

name :: TokenParser String
name = token go P.<?> "name"
  where
    go (Name n) = Just n
    go _        = Nothing

default :: TokenParser String
default = token go P.<?> "default"
  where
    go (Default s) = Just s
    go _         = Nothing

word :: TokenParser String
word = token go P.<?> "word"
  where
    go (Word n) = Just n
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
  ({ indentation: 0, line: 0 })
  <<< P.runParserT
  (P.PState { input: s, position: P.initialPos })
