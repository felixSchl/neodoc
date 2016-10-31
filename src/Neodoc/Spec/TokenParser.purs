module Neodoc.Spec.TokenParser where

import Prelude
import Data.Pretty
import Data.Bifunctor (lmap)
import Data.Tuple.Nested ((/\))
import Data.NonEmpty ((:|))
import Data.Maybe (Maybe(..), isNothing, fromMaybe)
import Data.Either (Either(..))
import Data.List (List(..), (:))
import Data.String.Ext ((^=), (~~))
import Data.Functor (($>))
import Data.NonEmpty (NonEmpty, (:|))
import Data.NonEmpty as NonEmpty
import Control.Monad.State (StateT(..), State(..), evalState)
import Control.Monad.Except (ExceptT(..), throwError)
import Control.MonadPlus (guard)
import Control.Lazy (defer)
import Control.Alt ((<|>))
import Control.Apply ((*>), (<*))

import Neodoc.Spec.Token
import Neodoc.Spec.ParserState as ParserState
import Neodoc.Spec.ParserState (ParserState(..))

import Neodoc.Parsing.Parser (Parser(..), ParserArgs(..), Step(..))
import Neodoc.Parsing.Parser as P
import Neodoc.Parsing.Parser.Combinators ((<?>), (<??>))

-- | Parser that parses a stream of tokens
type TokenParser a
  = Parser
      String
      Unit
      ParserState
      Unit
      (List PositionedToken)
      a

runTokenParser
  :: ∀ a
   . List PositionedToken
  -> TokenParser a
  -> Either String a
runTokenParser s p = lmap (P.extractError id) do
  P.runParser unit ParserState.initialState unit s p

token :: ∀ a. (Token -> Maybe a) -> TokenParser a
token test = Parser \(args@(ParseArgs _ _ _ toks)) ->
  case toks of
    (PositionedToken ppos tok):xs ->
      case test tok of
        Just a ->
          let nextpos = case xs of
                (PositionedToken npos _):_ -> npos
                Nil -> ppos
          in Step true (P.setI xs args) (Right a)
        _ -> Step false args (Left (P.error "Token did not match predicate"))
    _ -> Step false args (Left (P.error "Expected token, but met EOF"))

-- | Match the token at the head of the stream
match :: Token -> TokenParser Unit
match tok = token (guard <<< (_ == tok)) <|> defer \_->
              P.fail $ "Expected " <> pretty tok

eof :: TokenParser Unit
eof = do
  toks <- P.getInput
  case toks of
    _:_ -> P.fail "Expected EOF"
    _   -> pure unit

anyToken :: TokenParser Token
anyToken = token Just

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
garbage = "garbage" <??> token go
  where
    go (Garbage _) = Just unit
    go _           = Nothing

lopt :: TokenParser { name :: String
                    , arg  :: Maybe OptionArgument
                    }
lopt = "long-option" <??> token go
  where
    go (LOpt n a) = Just { name: n, arg: a }
    go _          = Nothing

sopt :: TokenParser { chars :: NonEmpty Array Char
                    , arg   :: Maybe OptionArgument
                    }
sopt = "short-option" <??> token go
  where
    go (SOpt cs a) = Just { chars: cs , arg: a }
    go _ = Nothing

name :: TokenParser String
name = "name" <??> token go
  where
    go (Name n) = Just n
    go _        = Nothing

tag :: String -> TokenParser String
tag s = ("tag: " ~~ s) <??> token go
  where
    go (Tag k v) | k ^= s = Just v
    go _                  = Nothing

reference :: TokenParser String
reference = "reference" <??> token go
  where
    go (Reference r) = Just r
    go _             = Nothing

angleName :: TokenParser String
angleName = "<name>" <??> token go
  where
    go (AngleName n) = Just n
    go _             = Nothing

shoutName :: TokenParser String
shoutName = "NAME" <??> token go
  where
    go (ShoutName n) = Just n
    go _             = Nothing
