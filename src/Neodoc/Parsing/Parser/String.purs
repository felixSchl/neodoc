module Neodoc.Parsing.Parser.String where

import Prelude hiding (between)
import Control.Alt ((<|>))
import Data.String as S
import Data.Char as C
import Data.List (List, many)
import Data.Array as A
import Data.Newtype (wrap)
import Data.Maybe (Maybe(..), isJust, isNothing)
import Data.Either (Either(..))
import Neodoc.Parsing.Parser
import Neodoc.Parsing.Parser.Combinators
import Text.Parsing.Parser.Pos (Position(..), updatePosString) as P

type StringParser e c g = Parser e c StringParserState g String
type StringParserState = { position :: P.Position }

getPosition :: ∀ e c g. StringParser e c g P.Position
getPosition = _.position <$> getState

eof :: ∀ e c g. StringParser e c g Unit
eof = do
  i <- getInput
  unless (S.null i) (fail "Expected EOF")

satisfy :: ∀ e c g. (Char -> Boolean) -> StringParser e c g Char
satisfy f = try do
  c <- anyChar
  if f c then return c
         else fail $ "Character '" <> S.singleton c <> "' did not satisfy predicate"

-- | Match any character.
anyChar :: ∀ e c g. StringParser e c g Char
anyChar = do
  i <- getInput
  case S.uncons i of
    Nothing -> fail "Unexpected EOF"
    Just { head, tail } -> Parser \a ->
      let s = getS a
          s' = s { position = P.updatePosString s.position (S.singleton head) }
       in Step true (setI tail (setS s' a)) (Right head)

-- | Match the specified string.
string :: ∀ e c g. String -> StringParser e c g String
string str = do
  i <- getInput
  case S.indexOf (wrap str) i of
    Just 0 -> Parser \a ->
      let s = getS a
          s' = s { position = P.updatePosString s.position str }
          i' = S.drop (S.length str) i
       in Step true (setI i' (setS s a)) (Right str)
    _ -> fail ("Expected " <> show str)

-- | Match the specified character
char :: ∀ e c g. Char -> StringParser e c g Char
char c = satisfy (_ == c) <?> ("Expected " <> show c)

-- | Match one of the characters in the array.
oneOf :: ∀ e c g. Array Char -> StringParser e c g Char
oneOf ss = satisfy (\c -> isJust (A.elemIndex c ss))
            <?> ("Expected one of " <> show ss)

-- | Match any character not in the array.
noneOf :: ∀ e c g. Array Char -> StringParser e c g Char
noneOf ss =  satisfy (\c -> isNothing (A.elemIndex c ss))
              <?> ("Expected none of " <> show ss)

--------------------------------------------------------------------------------

satisfyCode :: ∀ e c g. (Int -> Boolean) -> StringParser e c g Char
satisfyCode f = satisfy \c -> f (C.toCharCode c)

isDigit :: Int -> Boolean
isDigit c = c > 47 && c < 58

isLowerAlpha :: Int -> Boolean
isLowerAlpha c = c > 96 && c < 123

isUpperAlpha :: Int -> Boolean
isUpperAlpha c = c > 64 && c < 91

lower :: ∀ e c g. StringParser e c g Char
lower = satisfy \c -> c == C.toLower c

upper :: ∀ e c g. StringParser e c g Char
upper = satisfy \c -> c == C.toUpper c

digit :: ∀ e c g. StringParser e c g Char
digit = satisfyCode \c -> c > 47 && c < 58

alpha :: ∀ e c g. StringParser e c g Char
alpha = satisfyCode \c -> (c > 64 && c < 91) || (c > 96 && c < 123)

upperAlpha :: ∀ e c g. StringParser e c g Char
upperAlpha = satisfyCode isUpperAlpha

lowerAlpha :: ∀ e c g. StringParser e c g Char
lowerAlpha = satisfyCode isLowerAlpha

lowerAlphaNum :: ∀ e c g. StringParser e c g Char
lowerAlphaNum = satisfyCode \c -> isLowerAlpha c || isDigit c

upperAlphaNum :: ∀ e c g. StringParser e c g Char
upperAlphaNum = satisfyCode \c -> isUpperAlpha c || isDigit c

alphaNum :: ∀ e c g. StringParser e c g Char
alphaNum = satisfyCode \c -> isUpperAlpha c || isLowerAlpha c || isDigit c

eol :: ∀ e c g. StringParser e c g Unit
eol = (void $ string "\r\n") <|> (void $ char '\n')

space :: ∀ e c g. StringParser e c g Char
space = satisfy \c -> c == ' ' || c == '\t'

spaces  :: ∀ e c g. StringParser e c g (List Char)
spaces = many space

