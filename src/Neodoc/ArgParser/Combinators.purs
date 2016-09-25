module Neodoc.ArgParser.Combinators where

import Prelude
import Control.Alt ((<|>))
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
