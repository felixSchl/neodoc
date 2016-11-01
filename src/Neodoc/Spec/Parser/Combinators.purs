-- |
-- | This module defines commonly used TokenParser-combinators
-- |

module Neodoc.Spec.Parser.Combinators where

import Prelude
import Control.Monad.Trans.Class (lift)
import Control.MonadPlus (guard)
import Data.List (List(..), (:))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Control.Monad.State (State(..), evalState)
import Control.Monad.State as State

import Neodoc.Spec.Lexer
import Neodoc.Spec.Token
import Neodoc.Spec.TokenParser
import Neodoc.Spec.ParserState
import Neodoc.Spec.ParserState as ParserState
import Neodoc.Parsing.Parser as P
import Neodoc.Parsing.Parser.Combinators ((<?>), (<??>))
import Neodoc.Parsing.Parser.Combinators as P
import Neodoc.Parsing.Parser.Pos (Position(..)) as P

-- | Return the next token's position w/o consuming anything
nextTokPos :: TokenParser P.Position
nextTokPos = do
  toks <- P.getInput
  case toks of
    (PositionedToken pos _):xs -> pure pos
    _                          -> P.fail "Expected token, met EOF"

-- |
-- Mark the current indentation level
--
markIndent :: ∀ a. TokenParser a -> TokenParser a
markIndent p =
  let setIndent i = P.modifyState \s -> ParserState.setIndentation i s
   in do
    x <- P.optionMaybe nextTokPos
    P.try $ case x of
      Just (P.Position _ tokCol) -> do
        curCol <- ParserState.getIndentation <$> P.getState
        setIndent tokCol *> p <* setIndent curCol
      _ -> p

-- |
-- Mark a custom indentation level
--
markIndent' :: ∀ a. Int -> TokenParser a -> TokenParser a
markIndent' level p =
  let setIndent i = P.modifyState \s -> ParserState.setIndentation i s
   in do
    curCol <- ParserState.getIndentation <$> P.getState
    P.try $ setIndent level *> p <* setIndent curCol

-- |
-- Check that the current identation level matches a predicate
--
checkIndentation :: (Int -> Int -> Boolean) -> TokenParser Unit
checkIndentation rel = do
  P.Position _ column <- nextTokPos
  current <- ParserState.getIndentation <$> P.getState
  guard (column `rel` current)

-- |
-- Check that the current indentation level is past the current mark
--
indented :: TokenParser Unit
indented = checkIndentation (>=) <?> "indentation"

-- |
-- Check that the current indentation level is before the current mark
--
moreIndented :: TokenParser Unit
moreIndented = checkIndentation (>) <?> "more indentation"

-- |
-- Check that the current indentation level is before the current mark
--
lessIndented :: TokenParser Unit
lessIndented = checkIndentation (<) <?> "less indentation"

-- |
-- Check that the current indentation level is at the same indentation as the
-- current mark
--
sameIndent :: TokenParser Unit
sameIndent = checkIndentation (==) <?> "same indentation"
