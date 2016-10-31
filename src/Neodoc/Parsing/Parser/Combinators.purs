module Neodoc.Parsing.Parser.Combinators where

import Prelude
import Data.Foldable (foldl, class Foldable)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Data.List (List(..), (:), reverse)
import Control.Alt ((<|>))
import Control.Plus (empty)
import Data.Either (Either(..))
import Neodoc.Parsing.Parser

option :: ∀ e c s g i a. a -> Parser e c s g i a -> Parser e c s g i a
option a p = p <|> return a

optionMaybe :: ∀ e c s g i a. Parser e c s g i a -> Parser e c s g i (Maybe a)
optionMaybe p = option Nothing (Just <$> p)

try :: ∀ e c s g i a. Parser e c s g i a -> Parser e c s g i a
try p = Parser \(a@(ParseArgs c s g i)) -> case unParser p a of
  Step _ a' e@(Left _) -> Step false (setG (getG a') a) e
  step                 -> step

-- | Parse one of a set of alternatives.
choice :: ∀ f e c s g i a. (Foldable f) => f (Parser e c s g i a) -> Parser e c s g i a
choice = foldl (<|>) empty

-- | Provide an error message in the case of failure.
withErrorMessage :: ∀ e c s g i a. Parser e c s g i a -> String -> Parser e c s g i a
withErrorMessage p msg = p <|> fail ("Expected " <> msg)

infix 3 withErrorMessage as <?>

-- | Flipped `(<?>)`.
asErrorMessage :: ∀ e c s g i a. String -> Parser e c s g i a -> Parser e c s g i a
asErrorMessage = flip (<?>)

infix 3 asErrorMessage as <??>

-- | Fail if the specified parser matches.
notFollowedBy ::  ∀ e c s g i a. Parser e c s g i a -> Parser e c s g i Unit
notFollowedBy p = try $ (try p *> fail "Negated parser succeeded") <|> return unit

-- | Parse a phrase, without modifying the consumed state or stream position.
lookAhead ::  ∀ e c s g i a. Parser e c s g i a -> Parser e c s g i a
lookAhead p = Parser \a -> case unParser p a of
  Step _ _ r -> Step false a r

-- | Optionally parse something, failing quietly.
optional ::  ∀ e c s g i a. Parser e c s g i a -> Parser e c s g i Unit
optional p = void p <|> return unit

-- | Parse a phrase, without modifying the consumed state or stream position.
between
  :: ∀ e c s g i a open close
   . Parser e c s g i open
  -> Parser e c s g i close
  -> Parser e c s g i a
  -> Parser e c s g i a
between open close p = open *> p <* close

-- | Parse phrases delimited by a separator.
-- |
-- | For example:
-- |
-- | ```purescript
-- | digit `sepBy` string ","
-- | ```
sepBy
  :: ∀ e c s g i a sep
   . Parser e c s g i a
  -> Parser e c s g i sep
  -> Parser e c s g i (List a)
sepBy p sep = sepBy1 p sep <|> return Nil

-- | Parse phrases delimited by a separator, requiring at least one match.
sepBy1
  :: ∀ e c s g i a sep
   . Parser e c s g i a
  -> Parser e c s g i sep
  -> Parser e c s g i (List a)
sepBy1 p sep = do
  a  <- p
  as <- many $ sep *> p
  return (a : as)

-- | Parse several phrases until the specified terminator matches.
manyTill
  :: ∀ e c s g i a end
   . Parser e c s g i a
  -> Parser e c s g i end
  -> Parser e c s g i (List a)
manyTill p end = go Nil
  where
  go acc = (end *> return acc) <|> (p >>= \v -> go (v:acc))

-- optimal: tail recursive `many` implementation specialized for lists
many p = reverse <$> go Nil
  where go acc = do
          v <- option Nothing (Just <$> p)
          case v of
            Nothing -> return acc
            Just v  -> go (v:acc)

-- optimal: tail recursive `some` implementation specialized for lists
some p = reverse <$> do
  x <- p
  go (x:Nil)
  where go acc = do
          v <- option Nothing (Just <$> p)
          case v of
            Nothing -> return acc
            Just v  -> go (v:acc)

