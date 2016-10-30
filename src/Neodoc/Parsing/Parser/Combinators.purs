module Neodoc.Parsing.Parser.Combinators where

import Prelude
import Data.Foldable (foldl, class Foldable)
import Data.Tuple.Nested ((/\))
import Control.Alt ((<|>))
import Control.Plus (empty)
import Data.Either (Either(..))
import Neodoc.Parsing.Parser

option :: ∀ e c s g i a. a -> Parser e c s g i a -> Parser e c s g i a
option a p = p <|> pure a

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
notFollowedBy p = try $ (try p *> fail "Negated parser succeeded") <|> pure unit

-- | Parse a phrase, without modifying the consumed state or stream position.
lookAhead ::  ∀ e c s g i a. Parser e c s g i a -> Parser e c s g i a
lookAhead p = Parser \a -> case unParser p a of
  Step _ _ r -> Step false a r

-- | Optionally parse something, failing quietly.
optional ::  ∀ e c s g i a. Parser e c s g i a -> Parser e c s g i Unit
optional p = void p <|> return unit

-- | Parse a phrase, without modifying the consumed state or stream position.
between
  ::  ∀ e c s g i a open close
   . Parser e c s g i open
  -> Parser e c s g i close
  -> Parser e c s g i a
  -> Parser e c s g i a
between open close p = open *> p <* close
