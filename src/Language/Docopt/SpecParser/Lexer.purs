module Language.Docopt.SpecParser.Lexer where

import Prelude
import Data.Array as A
import Debug.Trace
import Data.List as L
import Data.Monoid (mempty)
import Data.Functor (($>))
import Control.Alt ((<|>))
import Control.Apply ((*>), (<*))
import Control.Monad.State (State, evalState)
import Control.Monad.State.Trans (StateT())
import Control.MonadPlus (guard)
import Data.Either (Either(..))
import Data.Identity (Identity())
import Data.List (List(..), many, catMaybes, toUnfoldable)
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.String (fromCharArray, trim)
import Data.String.Ext ((^=))
import Language.Docopt.SpecParser.Base (lowerAlphaNum, alphaNum, alpha, space,
                                        lowerAlpha, upperAlpha, string',
                                        getPosition, getInput, spaces, eol)
import Language.Docopt.SpecParser.State (ParserState)
import Text.Parsing.Parser (ParseError, Parser, PState(..), ParserT(..),
                            runParserT, parseFailed, fail, runParser) as P
import Text.Parsing.Parser.Combinators ((<?>), notFollowedBy, try, choice,
                                        lookAhead, optional, between, manyTill
                                        ) as P
import Text.Parsing.Parser.Pos (Position, initialPos) as P
import Text.Parsing.Parser.String (skipSpaces, anyChar, string, char, oneOf,
                                  whiteSpace, eof, noneOf) as P

data Mode = Usage | Descriptions

isDescMode :: Mode -> Boolean
isDescMode Descriptions = true
isDescMode _            = false

isUsageMode :: Mode -> Boolean
isUsageMode Usage = true
isUsageMode _     = false

lex :: Mode -> String -> Either P.ParseError (List PositionedToken)
lex m = flip P.runParser (parseTokens m)

lexDescs :: String -> Either P.ParseError (List PositionedToken)
lexDescs = lex Descriptions

lexUsage :: String -> Either P.ParseError (List PositionedToken)
lexUsage = lex Usage

data Token
  = LParen
  | RParen
  | LSquare
  | RSquare
  | Dash
  | VBar
  | Colon
  | Comma
  | Newline
  | TripleDot
  | Reference String
  | LOpt String (Maybe { name :: String, optional :: Boolean })
  | SOpt Char (Array Char) (Maybe { name :: String, optional :: Boolean })
  | Tag String (Maybe String)
  | Name String
  | ShoutName String
  | AngleName String
  | Garbage Char
  | DoubleDash

prettyPrintToken :: Token -> String
prettyPrintToken LParen        = show '('
prettyPrintToken RParen        = show ')'
prettyPrintToken LSquare       = show '['
prettyPrintToken RSquare       = show ']'
prettyPrintToken Dash          = show '-'
prettyPrintToken VBar          = show '|'
prettyPrintToken Newline       = show '\n'
prettyPrintToken Colon         = show ':'
prettyPrintToken Comma         = show ','
prettyPrintToken TripleDot     = "..."
prettyPrintToken DoubleDash    = "--"
prettyPrintToken (Reference r) = "Reference " <> show r
prettyPrintToken (Garbage   c) = "Garbage "   <> show c
prettyPrintToken (Tag k v)     = "Tag "       <> k <> " "  <> (show v)
prettyPrintToken (Name      n) = "Name "      <> show n
prettyPrintToken (ShoutName n) = "ShoutName " <> show n
prettyPrintToken (AngleName n) = "AngleName " <> show n
prettyPrintToken (LOpt n arg)
  = "--" <> n
         <> (fromMaybe "" do
              a <- arg
              pure $ if a.optional then "[" else ""
                <> a.name
                <> if a.optional then "]" else ""
            )
prettyPrintToken (SOpt n s arg)
  = "-" <> (fromCharArray (A.cons n s))
        <> (fromMaybe "" do
              a <- arg
              pure $ if a.optional then "[" else ""
                <> a.name
                <> if a.optional then "]" else ""
            )

data PositionedToken = PositionedToken
  { sourcePos :: P.Position
  , token     :: Token
  }

instance showToken :: Show Token where
  show = show <<< prettyPrintToken

instance eqToken :: Eq Token where
  eq LParen            LParen             = true
  eq RParen            RParen             = true
  eq LSquare           LSquare            = true
  eq RSquare           RSquare            = true
  eq VBar              VBar               = true
  eq Colon             Colon              = true
  eq Comma             Comma              = true
  eq Dash              Dash               = true
  eq DoubleDash        DoubleDash         = true
  eq TripleDot         TripleDot          = true
  eq Newline           Newline            = true
  eq (Reference r)     (Reference r')     = r == r'
  eq (LOpt n arg)      (LOpt n' arg')
    = (n == n')
    && ((isNothing arg && isNothing arg')
        || (fromMaybe false do
              a  <- arg
              a' <- arg'
              pure $ (a.name == a.name)
                    && (a.optional == a.optional)
            ))
  eq (SOpt n s arg)    (SOpt n' s' arg')
    = (n == n') && (s' == s')
    && ((isNothing arg && isNothing arg')
        || (fromMaybe false do
              a  <- arg
              a' <- arg'
              pure $ (a.name == a.name)
                    && (a.optional == a.optional)
            ))
  eq (AngleName n)     (AngleName n')     = n == n'
  eq (ShoutName n)     (ShoutName n')     = n == n'
  eq (Name n)          (Name n')          = n == n'
  eq (Garbage c)       (Garbage c')       = c == c'
  eq _ _                                  = false

instance showPositionedToken :: Show PositionedToken where
  show (PositionedToken { sourcePos: pos, token: tok }) =
    (show tok) <> " at " <> (show pos)

parseTokens :: Mode -> P.Parser String (L.List PositionedToken)
parseTokens m = do
  P.skipSpaces
  xs <- L.many $ parsePositionedToken m
  P.eof <|> void do
    i <- getInput
    P.fail $ "Unexpected input: " <> i
  pure xs

parsePositionedToken :: Mode -> P.Parser String PositionedToken
parsePositionedToken m = P.try $ do
  pos <- getPosition
  tok <- parseToken m
  pure $ PositionedToken { sourcePos: pos, token: tok }

parseToken :: Mode -> P.Parser String Token
parseToken m = P.choice (P.try <$> A.concat [
    [ P.char   '('   *> pure LParen
    , P.char   ')'   *> pure RParen
    ]
  , if isDescMode m then [ _tag ] else []
  , [ _reference
    , P.char   '['   *> pure LSquare
    , P.char   ']'   *> pure RSquare
    , P.char   '|'   *> pure VBar
    , P.char   ':'   *> pure Colon
    , P.char   ','   *> pure Comma
    , P.string "..." *> pure TripleDot
    , _longOption
    , _shortOption
    , _eoa
    , _stdin
    , AngleName <$> _angleName
    , ShoutName <$> _shoutName
    , Name      <$> _name
    ]
  , if isDescMode m
        then [
          eol      $> Newline
        , Garbage <$> P.anyChar
        ]
        else []
  ])
  <* if isDescMode m
        then void $ spaces -- skip only spaces ' ' and '\t'
        else P.skipSpaces  -- skip spaces *AND* newlines

 where

  white :: P.Parser String Unit
  white = void $ P.oneOf [ '\n', '\r', ' ', '\t' ]

  _stdin :: P.Parser String Token
  _stdin = do
    P.char '-'
    -- Ensure the argument is correctly bounded
    P.eof <|> (P.lookAhead $ P.choice $ P.try <$> [
      void $ white
    , void $ P.char '|'
    , void $ P.char ']'
    , void $ P.char ')'
    , void $ P.string "..."
    ])
    pure Dash

  _eoa :: P.Parser String Token
  _eoa = do
    P.string "--"
    -- Ensure the argument is correctly bounded
    P.eof <|> (P.lookAhead $ P.choice $ P.try <$> [
      void $ white
    , void $ P.char ']'
    , void $ P.char ')'
    ])
    pure DoubleDash

  _reference :: P.Parser String Token
  _reference = Reference <$> do
    P.between (P.char '[') (P.char ']') go

    where
      go = do
        many space
        fromCharArray <<< toUnfoldable <$> do
          flip P.manyTill (P.lookAhead $ P.try end) do
            P.noneOf [ ']' ]
        <* end <* many space

      end = do
        many space
        P.optional $ P.string "-"
        string' "options"
        P.optional $ P.string "..."

  _tag :: P.Parser String Token
  _tag = P.between (P.char '[') (P.char ']') do
    P.choice $ P.try <$> [
      withValue
    , withoutValue
    ]

    where
      withValue = do
        many white
        k <- fromCharArray <$> do A.many (P.noneOf [':'])
        P.char ':'
        many white
        v <- trim <<< fromCharArray <$> do A.some $ P.noneOf [']']
        many white
        pure (Tag k (Just v))
      withoutValue = do
        k <- trim <<< fromCharArray <$> do A.some $ P.noneOf [']']
        pure (Tag k Nothing)

  _shoutName :: P.Parser String String
  _shoutName = do
    n <- fromCharArray <$> do
      A.cons
        <$> upperAlpha
        <*> (A.many (upperAlpha <|> P.oneOf ['-', '_']))
    P.notFollowedBy lowerAlpha
    pure n

  _name :: P.Parser String String
  _name = do
    n  <- alphaNum
    ns <- do
      A.many $ P.try $ do
        P.choice $ P.try <$> [
          identLetter
        , P.char '.' <* (P.notFollowedBy $ P.string "..")
        , P.oneOf [ '-', '_' ]
      ]
    pure $ fromCharArray (n A.: ns)

  _angleName :: P.Parser String String
  _angleName = do
    P.char '<'
    n <- fromCharArray <$> do
      A.some $ P.choice [
        identLetter
        -- disallow swallowing new `<`s in order to avoid creating hard to trace
        -- errors for the user
      , P.try $ P.noneOf [ '<', '>' ]
      ]
    P.char '>'
    pure $ "<" <> n <> ">"

  _shortOption :: P.Parser String Token
  _shortOption = do
    let validChar = alphaNum <|> P.oneOf [ '?' ]

    P.char '-'
    x  <- validChar
    xs <- A.many validChar

    arg <- P.choice $ P.try <$> [

      -- Case 1: -foo=BAR
      Just <$> do
        P.char '='
        n <- P.choice $ P.try <$> [ _angleName, _shoutName, _name ]
        pure  { name:     n
              , optional: false
              }

      -- Case 2: Option[=ARG]
    , Just <$> do
        P.char '['
        P.optional $ P.char '='
        n <- P.choice $ P.try <$> [ _angleName, _shoutName, _name ]
        P.char ']'
        pure  { name:     n
              , optional: true
              }

      -- Case 3: Option<ARG>
    , Just <$> do
        n <- _angleName
        pure { name:     n
              , optional: false
              }

    , pure Nothing
    ]

    -- Ensure the argument is correctly bounded
    P.eof <|> (P.lookAhead $ P.choice $ P.try <$> [
      void $ white
    , void $ P.char '|'
    , void $ P.string "..."
    , void $ P.char ']'
    , void $ P.char ')'
    , void $ P.char ',' -- desc mode only
    ])

    pure $ SOpt x xs arg

  _longOption :: P.Parser String Token
  _longOption = do
    P.string "--"

    name' <- fromCharArray <$> do
      A.cons
        <$> alphaNum
        <*> (A.many $ P.choice [
              alphaNum
            , P.oneOf [ '-' ] <* P.lookAhead alphaNum
            ])

    arg <- P.choice $ P.try <$> [

      -- Case 1: OPTION=ARG
      Just <$> do
        P.char '='
        n <- P.choice $ P.try <$> [ _angleName, _shoutName, _name ]
        pure { name:     n
               , optional: false
               }

      -- Case 2: Option[=ARG]
    , Just <$> do
        P.char '['
        P.optional $ P.char '='
        n <- P.choice $ P.try <$> [ _angleName, _shoutName, _name ]
        P.char ']'
        pure { name:     n
               , optional: true
               }

    , pure Nothing
    ]

    -- Ensure the argument is correctly bounded
    P.eof <|> (P.lookAhead $ P.choice $ P.try <$> [
      void $ white
    , void $ P.char '|'
    , void $ P.string "..."
    , void $ P.char ']'
    , void $ P.char ')'
    , void $ P.char ',' -- desc mode only
    ])

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
  pure $ case toks of
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
match :: Token -> TokenParser Unit
match tok = token (guard <<< (_ == tok)) P.<?> prettyPrintToken tok

anyToken :: P.ParserT (List PositionedToken)
                      (StateT { indentation :: Int , line :: Int } Identity )
                      Token
anyToken = token $ Just

eof :: TokenParser Unit
eof = P.notFollowedBy anyToken P.<?> "EOF"

lparen :: TokenParser Unit
lparen = match LParen

rparen :: TokenParser Unit
rparen = match RParen

lsquare :: TokenParser Unit
lsquare = match LSquare

rsquare :: TokenParser Unit
rsquare = match RSquare

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

newline :: TokenParser Unit
newline = match Newline

tripleDot :: TokenParser Unit
tripleDot = match TripleDot

garbage :: TokenParser Unit
garbage = token go P.<?> "garbage"
  where
    go (Garbage _) = Just unit
    go _           = Nothing

lopt :: TokenParser { name :: String
                    , arg  :: Maybe { name :: String
                                    , optional :: Boolean
                                    } }
lopt = token go P.<?> "long-option"
  where
    go (LOpt n a) = Just { name: n, arg: a }
    go _          = Nothing

sopt :: TokenParser { flag  :: Char
                    , stack :: Array Char
                    , arg   :: Maybe { name :: String
                                     , optional :: Boolean
                                     } }
sopt = token go P.<?> "short-option"
  where
    go (SOpt n s a) = Just { flag: n , stack: s , arg: a }
    go _ = Nothing

name :: TokenParser String
name = token go P.<?> "name"
  where
    go (Name n) = Just n
    go _        = Nothing

tag :: String -> TokenParser String
tag s = token go P.<?> ("tag: " <> s)
  where
    go (Tag k (Just v)) | k ^= s = Just v
    go _                         = Nothing

reference :: TokenParser String
reference = token go P.<?> "reference"
  where
    go (Reference r) = Just r
    go _             = Nothing

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

-- | Return the next token's position w/o consuming anything
nextTokPos :: TokenParser P.Position
nextTokPos = P.ParserT $ \(P.PState { input: toks, position: pos }) ->
  pure $ case toks of
    Cons x@(PositionedToken { token: tok, sourcePos: ppos }) xs ->
      { consumed: false
      , input: toks
      , result: Right ppos
      , position: pos }
    _ -> P.parseFailed toks pos "expected token, met EOF"

runTokenParser :: forall a.
                  (List PositionedToken)
                -> TokenParser a
                -> Either P.ParseError a
runTokenParser s =
  flip evalState ({ indentation: 0, line: 0 })
        <<< P.runParserT (P.PState { input: s, position: P.initialPos })
