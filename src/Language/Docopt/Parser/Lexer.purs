module Language.Docopt.Parser.Lexer where

import Debug.Trace
import Prelude
import Control.Apply ((*>), (<*))
import Control.Alt ((<|>))
import Control.Plus (empty)
import Control.Monad.State (State(), evalState, get)
import Control.Monad.Trans
import Data.Foldable (foldl)
import Data.String as Str
import Text.Parsing.Parser as P
import Text.Parsing.Parser.Combinators as P
import Text.Parsing.Parser.Token as P
import Text.Parsing.Parser.Pos as P
import Text.Parsing.Parser.String as P
import Data.List as L
import Data.Array as A
import Data.Char (toString, toLower, toUpper)
import Data.String (fromCharArray, fromChar, trim)
import Data.List (List(..), (:), fromList, many)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe, fromMaybe, isNothing)
import Data.String.Ext ((^=))
import Language.Docopt.Parser.Base
import Language.Docopt.Parser.State
import Data.Tuple
import Data.Int
import Data.Int as I
import Global (isNaN, readFloat)

data Mode = Usage | Descriptions

isDescMode :: Mode -> Boolean
isDescMode Descriptions = true
isDescMode _ = false

isUsageMode :: Mode -> Boolean
isUsageMode Usage = true
isUsageMode _ = false

lex :: Mode -> String -> Either P.ParseError (List PositionedToken)
lex m = flip P.runParser (parseTokens m)

lexDescs :: String -> Either P.ParseError (List PositionedToken)
lexDescs = lex Descriptions

lexUsage :: String -> Either P.ParseError (List PositionedToken)
lexUsage = lex Usage

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
  | Reference String
  | LOpt String (Maybe { name :: String, optional :: Boolean })
  | SOpt Char (Array Char) (Maybe { name :: String, optional :: Boolean })
  | Tag String (Maybe String)
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
prettyPrintToken (Reference r)     = "Reference " ++ show r
prettyPrintToken (Garbage   c)     = "Garbage "   ++ show c
prettyPrintToken (Tag k v)         = "Tag "       ++ k ++ " "  ++ (show v)
prettyPrintToken (Name      n)     = "Name "      ++ show n
prettyPrintToken (ShoutName n)     = "ShoutName " ++ show n
prettyPrintToken (AngleName n)     = "AngleName " ++ show n
prettyPrintToken (StringLiteral s) = "String "    ++ show s
prettyPrintToken (NumberLiteral n) = "Number "    ++ show n
prettyPrintToken (Word w)          = "Word "      ++ show w
prettyPrintToken (LOpt n arg)
  = "--" ++ n
         ++ (fromMaybe "" do
              a <- arg
              return $ if a.optional then "[" else ""
                ++ a.name
                ++ if a.optional then "]" else ""
            )
prettyPrintToken (SOpt n s arg)
  = "-" ++ (fromCharArray (A.cons n s))
        ++ (fromMaybe "" do
              a <- arg
              return $ if a.optional then "[" else ""
                ++ a.name
                ++ if a.optional then "]" else ""
            )

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
  eq (Reference r)     (Reference r')     = r == r'
  eq (LOpt n arg)      (LOpt n' arg')
    = (n == n')
    && ((isNothing arg && isNothing arg')
        || (fromMaybe false do
              a  <- arg
              a' <- arg'
              return $ (a.name == a.name)
                    && (a.optional == a.optional)
            ))
  eq (SOpt n s arg)    (SOpt n' s' arg')
    = (n == n') && (s' == s')
    && ((isNothing arg && isNothing arg')
        || (fromMaybe false do
              a  <- arg
              a' <- arg'
              return $ (a.name == a.name)
                    && (a.optional == a.optional)
            ))
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

parseTokens :: Mode -> P.Parser String (L.List PositionedToken)
parseTokens m = do
  P.skipSpaces
  L.many $ parsePositionedToken m
  <* P.eof

parsePositionedToken :: Mode -> P.Parser String PositionedToken
parsePositionedToken m = P.try $ do
  pos <- getPosition
  tok <- parseToken m
  return $ PositionedToken { sourcePos: pos, token: tok }

parseToken :: Mode -> P.Parser String Token
parseToken m = P.choice (P.try <$> A.concat [
    [ P.char   '('   *> pure LParen
    , P.char   ')'   *> pure RParen
    ]
  , if isDescMode m then [ tag ] else []
  , [ reference
    , P.char   '['   *> pure LSquare
    , P.char   ']'   *> pure RSquare
    , P.char   '|'   *> pure VBar
    , P.char   ':'   *> pure Colon
    , P.char   ','   *> pure Comma
    , longOption
    , shortOption
    , AngleName <$> angleName
    , P.char   '<'   *> pure LAngle
    , P.char   '>'   *> pure RAngle
    , P.string "--"  *> pure DoubleDash
    , P.char   '-'   *> pure Dash
    , P.char   '='   *> pure Equal
    , P.string "..." *> pure TripleDot
    , numberLiteral
    , stringLiteral
    , ShoutName <$> (shoutName <* P.notFollowedBy alpha)
    , Name      <$> (name      <* P.notFollowedBy alpha)
    , Word      <$> (word      <* space)
    , Garbage   <$> P.anyChar
    ]
  ])
  <* P.skipSpaces

 where

  whitespace :: P.Parser String Unit
  whitespace = do
    P.satisfy \c -> c == '\n' || c == '\r' || c == ' ' || c == '\t'
    pure unit

  reference :: P.Parser String Token
  reference = Reference <$> do
    P.between (P.char '[') (P.char ']') go

    where
      go = do
        many space
        x <- fromCharArray <<< fromList <$> do
          flip P.manyTill (P.lookAhead $ P.try end) do
            P.noneOf [']']
        end
        many space
        return x

      end = do
        many space
        P.optional $ P.string "-"
        string' "options"
        P.optional $ P.string "..."

  tag :: P.Parser String Token
  tag = P.between (P.char '[')
                  (P.char ']')
                  (withValue <|> withoutValue)

    where
      withValue = do
        many whitespace
        k <- fromCharArray <$> do A.many (P.noneOf [':'])
        P.char ':'
        many whitespace
        v <- trim <<< fromCharArray <$> do A.some $ P.noneOf [']']
        many whitespace
        return (Tag k (Just v))
      withoutValue = do
        k <- trim <<< fromCharArray <$> do A.some $ P.noneOf [']']
        return (Tag k Nothing)

  stringLiteral :: P.Parser String Token
  stringLiteral = StringLiteral <$> do
    P.choice [
      P.between (P.char '\'') (P.char '\'') (p '\'')
    , P.between (P.char '"')  (P.char '"')  (p '"')
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
        x <- fromCharArray <$> (A.some digit)
        P.char '.'
        xs <- fromCharArray <$> (A.some digit)

        -- there is no "safe" version yet, afaik
        let n = readFloat $ x ++ "." ++ xs
        if isNaN n
           then P.fail "Could not parse float"
           else pure n

      intLiteral = do
        x <- fromCharArray <$> (A.some digit)
        case (I.fromString x) of
          Just n  -> return n
          Nothing -> P.fail "Could not parse integer"

  name :: P.Parser String String
  name = word

  shoutName :: P.Parser String String
  shoutName = do
    n <- fromCharArray <$> do
      A.cons
        <$> upperAlpha
        <*> (A.many $ regex "[A-Z_-]")
    P.notFollowedBy lowerAlpha
    return n

  word :: P.Parser String String
  word = do
    n  <- alphaNum
    ns <- do
      A.many $ P.try $ do
        P.choice $ P.try <$> [
          identLetter
        , P.char '.' <* (P.notFollowedBy $ P.string "..")
        , P.oneOf [ '-', '_' ]
      ]
    return $ fromCharArray (n A.: ns)

  angleName :: P.Parser String String
  angleName = do
    P.char '<'
    n <- fromCharArray <$> do
      A.some $ P.choice [
        identLetter
      , P.try $ P.noneOf [ '>' ]
      ]
    P.char '>'
    return n

  shortOption :: P.Parser String Token
  shortOption = do
    P.char '-'
    x  <- alphaNum
    xs <- A.many alphaNum
    arg <- P.choice $ P.try <$> [
      Just <$> do
        -- XXX: Drop the spaces?
        many space *> P.char '=' <* many space
        P.choice $ P.try <$> [ angleName, shoutName, name ]
    , Just <$> angleName
    , return Nothing
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

    return $ SOpt x xs do
      argname <- arg -- XXX: IMPLEMENT
      return {
        name:     argname
      , optional: false
      }

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

    arg <- P.choice $ P.try <$> [
      Just <$> do
        -- XXX: Drop the spaces?
        many space *> P.char '=' <* many space
        P.choice $ P.try <$> [ angleName, shoutName, name ]
    , Just <$> angleName
    , return Nothing
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

    return $ LOpt name' do
      argname <- arg -- XXX: IMPLEMENT
      return {
        name:     argname
      , optional: false
      }

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
tag s = token go P.<?> ("tag: " ++ s)
  where
    go (Tag k (Just v)) | k ^= s = Just v
    go _                         = Nothing

reference :: TokenParser String
reference = token go P.<?> "reference"
  where
    go (Reference r) = Just r
    go _             = Nothing

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

-- | Return the next token's position w/o consuming anything
nextTokPos :: TokenParser P.Position
nextTokPos = P.ParserT $ \(P.PState { input: toks, position: pos }) ->
  return $ case toks of
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
  flip evalState
  ({ indentation: 0, line: 0 })
  <<< P.runParserT
  (P.PState { input: s, position: P.initialPos })
