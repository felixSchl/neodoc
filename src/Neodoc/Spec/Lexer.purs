module Neodoc.Spec.Lexer where

import Prelude
import Data.Array as A
import Debug.Profile
import Debug.Trace
import Data.Pretty
import Data.Bifunctor (lmap)
import Data.NonEmpty (NonEmpty, (:|))
import Data.NonEmpty as NonEmpty
import Data.List as L
import Data.Tuple.Nested ((/\))
import Data.Monoid (mempty)
import Data.Functor (($>))
import Control.Alt ((<|>))
import Control.Apply ((*>), (<*))
import Control.Lazy (defer)
import Control.MonadPlus (guard)
import Data.Either (Either(..), fromRight)
import Data.Identity (Identity())
import Data.Foldable (foldMap)
import Control.Monad.State (StateT(..), State(..), evalState)
import Control.Monad.Except (ExceptT(..), throwError)
import Data.List (List(..), many, catMaybes, toUnfoldable, (:), some, reverse)
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.String (fromCharArray, trim, Pattern(..))
import Data.String (singleton, toUpper, split, joinWith) as String
import Data.String.Regex (Regex(), regex)
import Data.String.Regex (test, parseFlags, replace) as Regex
import Data.String.Ext ((^=), (~~))
import Partial.Unsafe (unsafePartial)

import Neodoc.Spec.Token
import Neodoc.Spec.Error (SpecParseError(..))

import Neodoc.Parsing.Parser.Pos as P
import Neodoc.Parsing.Parser (Parser(..), ParserArgs(..), Step(..))
import Neodoc.Parsing.Parser.String (StringParserState)
import Neodoc.Parsing.Parser.String as P
import Neodoc.Parsing.Parser.Combinators ((<?>), (<??>))
import Neodoc.Parsing.Parser.Combinators as P
import Neodoc.Parsing.Parser as P

data Mode = Usage | Descriptions

instance showMode :: Show Mode where
  show Usage        = "Usage"
  show Descriptions = "Descriptions"

-- | Parser that parses strings
type StringParser a
  = Parser
      String
      Unit
      StringParserState
      Unit
      String
      a

-- | Optimal: Faster P.skipSpaces since it does not accumulate into a list.
skipSpaces :: ∀ e c g. StringParser Unit
skipSpaces = (do
    P.satisfy \c -> c == '\n' || c == '\r' || c == ' ' || c == '\t'
    skipSpaces
  ) <|> P.return unit

-- | Optimal: Translate [[<anything>-]options] to @anything
-- | this saves us looking ahead repeatedly when parsing '['.
referenceRegex :: Regex
referenceRegex
  = unsafePartial $ fromRight $
      regex
        "\\[(([^\\]](?!\\s*-?\\s*options\\s*))*?.?)\\s*-?\\s*options\\s*(\\.\\.\\.)?\\s*\\]"
        (Regex.parseFlags "gmi")

-- -- | Optimal: Typeclass-less bind instance
lex :: Mode -> String -> Either SpecParseError (List PositionedToken)
lex m input = profileS "spec-parser::lex" \_->
  -- perform a simple transformation to avoid 'manyTill' and safe some millis
  -- lexing. Hopefully this won't be necessary when purescript-parsing improves
  -- performance, a faster parsing library shows up or the purescript compiler
  -- improves in performance.
  let input' = case m of
                Usage        -> Regex.replace referenceRegex "@$1" input
                Descriptions -> input
   in do
     lmap (SpecParseError <<< (P.extractError id)) $
        P.runParser unit { position: P.initialPos } unit input' (parseTokens m)

lexDescs :: String -> Either SpecParseError (List PositionedToken)
lexDescs = lex Descriptions <<< trimDescSection

lexUsage :: String -> Either SpecParseError (List PositionedToken)
lexUsage = lex Usage

parseTokens :: Mode -> StringParser (L.List PositionedToken)
parseTokens m =
  let tokParser = case m of
                    Usage        -> parseUsageToken
                    Descriptions -> parseDescriptionToken
   in do
    skipSpaces
    xs <- P.many do
      PositionedToken
        <$> P.getPosition
        <*> tokParser
    P.eof <|> void do
      i <- P.getInput
      P.fail $ "Unexpected input: " <> i
    pure xs

parseUsageToken :: StringParser Token
parseUsageToken = P.choice [
    profileA "token: LParen"\_-> P.char   '('   $> LParen
  , profileA "token: RParen"\_-> P.char   ')'   $> RParen
  , profileA "token: RSquare"\_-> P.char   ']'   $> RSquare
  , profileA "token: VBar"\_-> P.char   '|'   $> VBar
  , profileA "token: Colon"\_-> P.char   ':'   $> Colon
  , profileA "token: Comma"\_-> P.char   ','   $> Comma
  , profileA "token: TripleDot"\_-> P.string "..." $> TripleDot
  , profileA "token: LSquare"\_-> P.char   '['   $> LSquare
  , profileA "token: ueference"\_-> _reference
  , profileA "token: longOption"\_-> P.try _longOption
  , profileA "token: shortOption"\_-> P.try _shortOption
  , profileA "token: eoa"\_-> P.try _eoa
  , profileA "token: stdin"\_-> _stdin
  , profileA "token: angleName"\_-> AngleName <$> _angleName
  , profileA "token: maybeShoutName"\_-> maybeShoutName
  ]
  <* skipSpaces -- skip spaces *AND* newlines

parseDescriptionToken :: StringParser Token
parseDescriptionToken = P.choice [
    P.char   ','   $> Comma
  , P.char   '('   $> LParen
  , P.char   ')'   $> RParen
  , P.char   ']'   $> RSquare
  , P.string "..." $> TripleDot
  , P.try _longOption
  , P.try _shortOption
  , AngleName <$> _angleName
  , maybeShoutName
  , P.try _tag
  , P.char '[' $> LSquare
  , _reference
  , P.eol $> Newline
  , Garbage <$> P.anyChar
  ]
  <* P.spaces -- skip only spaces ' ' and '\t'

maybeShoutNameRegex :: Regex
maybeShoutNameRegex
  = unsafePartial $ fromRight $
      regex "[a-zA-Z]" (Regex.parseFlags "gi")

maybeShoutName :: StringParser Token
maybeShoutName = do
  n <- _anyName
  pure if (String.toUpper n == n && Regex.test maybeShoutNameRegex n)
          then ShoutName n
          else Name n

_anyName :: StringParser String
_anyName = do
  foldMap String.singleton <$> do
    (:)
      <$> P.alphaNum
      <*> P.many do
            P.choice [
              identLetter
            , P.oneOf [ '-', '_', '/' ]
            , P.try $ P.char '.' <* (P.notFollowedBy $ P.string "..")
          ]

white :: StringParser Unit
white = void $ P.oneOf [ '\n', '\r', ' ', '\t' ]

_stdin :: StringParser Token
_stdin = do
  P.char '-'
  -- Ensure the argument is correctly bounded
  P.eof <|> (P.lookAhead $ P.choice [
    void $ white
  , void $ P.char '|'
  , void $ P.char ']'
  , void $ P.char ')'
  , void $ P.string "..."
  ])
  pure Dash

_eoa :: StringParser Token
_eoa = do
  P.string "--"
  -- Ensure the argument is correctly bounded
  P.eof <|> (P.lookAhead $ P.choice [
    void $ white
  , void $ P.char ']'
  , void $ P.char ')'
  ])
  pure DoubleDash

_reference :: StringParser Token
_reference = Reference <$> do
  P.char '@'
  foldMap String.singleton <$> P.many (P.noneOf [' ', '\n'])

_tag :: StringParser Token
_tag = P.between (P.char '[') (P.char ']') do
  s <- trim <<< foldMap String.singleton <$> some (P.noneOf [']'])
  case A.uncons (String.split (Pattern ":") s) of
    Nothing -> P.fail "Expected label"
    Just { head: _, tail: xs } | A.length xs == 0 ->  P.fail "Expected label"
    Just { head: x, tail: xs } ->
      let v = trim (String.joinWith ":" xs)
       in pure (Tag x v)

_angleName :: StringParser String
_angleName = do
  P.char '<'
  n <- foldMap String.singleton <$> do
    some $ P.choice [
      identLetter
      -- disallow swallowing new `<`s in order to avoid creating hard to trace
      -- errors for the user
    , P.noneOf [ '<', '>' ]
    ]
  P.char '>'
  pure $ "<" ~~ n ~~ ">"

_shortOption :: StringParser Token
_shortOption = do
  let validChar = P.alphaNum <|> P.oneOf [ '?' ]

  P.char '-'
  x  <- validChar
  xs <- A.fromFoldable <$> P.many validChar

  arg <- P.option Nothing $ P.choice [

    -- Case 1: -foo=BAR
    Just <$> do
      P.char '='
      name <- P.choice [ _angleName, _anyName ]
      pure  { name, optional: false }

    -- Case 2: Option[=ARG]
  , Just <$> do
      P.char '['
      P.optional $ P.char '='
      name <- P.choice [ _angleName, _anyName ]
      P.char ']'
      pure  { name, optional: true }

    -- Case 3: Option<ARG>
  , Just <$> do
      name <- _angleName
      pure { name, optional: false }
  ]

  -- Ensure the argument is correctly bounded
  P.eof <|> (P.lookAhead $ P.choice [
    void $ white
  , void $ P.char '|'
  , void $ P.string "..."
  , void $ P.char ']'
  , void $ P.char ')'
  , void $ P.char ',' -- desc mode only
  ])

  pure $ SOpt (x:|xs) arg

_longOption :: StringParser Token
_longOption = do
  P.string "--"

  name' <- foldMap String.singleton <$> do
    (:)
      <$> P.alphaNum
      <*> (P.many $ P.choice [
            P.alphaNum
          , P.try $ P.char '.'
              <* (P.notFollowedBy $ P.string "..")
              <* P.lookAhead P.alphaNum
          , P.oneOf [ '-', '/' ] <* P.lookAhead P.alphaNum
          ])

  arg <- P.option Nothing $ P.choice [

    -- Case 1: OPTION=ARG
    Just <$> do
      P.char '='
      n <- P.choice [ _angleName, _anyName ]
      pure  { name: n
            , optional: false
            }

    -- Case 2: Option[=ARG]
  , Just <$> do
      P.char '['
      P.optional $ P.char '='
      n <- P.choice [ _angleName, _anyName ]
      P.char ']'
      pure  { name:     n
            , optional: true
            }
  ]

  -- Ensure the argument is correctly bounded
  P.eof <|> (P.lookAhead $ P.choice [
    void $ white
  , void $ P.char '|'
  , void $ P.string "..."
  , void $ P.char ']'
  , void $ P.char ')'
  , void $ P.char ',' -- desc mode only
  ])

  pure $ LOpt name' arg

identStart :: StringParser Char
identStart = P.alpha

identLetter :: StringParser Char
identLetter = P.alphaNum <|> P.oneOf ['_', '-']

flag :: StringParser Char
flag = P.lowerAlphaNum

foreign import trimDescSection :: String -> String
