module Neodoc.ArgParser.Combinators where

import Prelude
import Data.Foldable (foldl, class Foldable)
import Control.Alt ((<|>))
import Control.Plus (empty)
import Data.Either (Either(..))
import Neodoc.ArgParser.Type

option :: ∀ e c s a. a -> Parser e c s a -> Parser e c s a
option a p = p <|> pure a

try :: ∀ e c s a. Parser e c s a -> Parser e c s a
try p = Parser \c s ->
  let step = unParser p c s
   in case step of
        Step _ _ _ e@(Left _) -> Step false c s e
        _                     -> step

-- | Parse one of a set of alternatives.
choice :: ∀  f e c s a. (Foldable f) => f (Parser e c s a) -> Parser e c s a
choice = foldl (<|>) empty
