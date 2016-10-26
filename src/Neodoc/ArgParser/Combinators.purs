module Neodoc.ArgParser.Combinators where

import Prelude
import Data.Foldable (foldl, class Foldable)
import Data.Tuple.Nested ((/\))
import Control.Alt ((<|>))
import Control.Plus (empty)
import Data.Either (Either(..))
import Neodoc.ArgParser.Type

option :: ∀ e c s g i a. a -> Parser e c s g i a -> Parser e c s g i a
option a p = p <|> pure a

try :: ∀ e c s g i a. Parser e c s g i a -> Parser e c s g i a
try p = Parser \(a@(c /\ s /\ g /\ i)) ->
  let step = unParser p a
   in case step of
        Step _ _ _ g' _ e@(Left _) -> Step false c s g' i e
        _                         -> step

-- | Parse one of a set of alternatives.
choice :: ∀  f e c s g i a. (Foldable f) => f (Parser e c s g i a) -> Parser e c s g i a
choice = foldl (<|>) empty
