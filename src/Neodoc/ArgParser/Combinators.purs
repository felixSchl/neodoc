module Neodoc.ArgParser.Combinators where

import Prelude
import Data.Foldable (foldl, class Foldable)
import Control.Alt ((<|>))
import Control.Plus (empty)
import Data.Either (Either(..))
import Neodoc.ArgParser.Type

option :: ∀ e c s i a. a -> Parser e c s i a -> Parser e c s i a
option a p = p <|> pure a

try :: ∀ e c s i a. Parser e c s i a -> Parser e c s i a
try p = Parser \c s i ->
  let step = unParser p c s i
   in case step of
        Step _ _ _ _ e@(Left _) -> Step false c s i e
        _                       -> step

-- | Parse one of a set of alternatives.
choice :: ∀  f e c s i a. (Foldable f) => f (Parser e c s i a) -> Parser e c s i a
choice = foldl (<|>) empty
