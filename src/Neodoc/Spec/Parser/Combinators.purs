-- |
-- | This module defines commonly used TokenParser-combinators
-- |

module Neodoc.Spec.Parser.Combinators where

import Prelude
import Control.Monad.Trans.Class (lift)
import Control.MonadPlus (guard)
import Data.List (List(..), (:))
import Data.Either (Either(..))
import Control.Monad.State (State(..), evalState)
import Control.Monad.State as State
import Text.Parsing.Parser (ParseState(..), ParserT(..)) as P
import Text.Parsing.Parser.Combinators ((<?>), try) as P
import Text.Parsing.Parser.Pos (Position(..)) as P
import Neodoc.Spec.Parser.Base (getInput, getPosition)

import Neodoc.Spec.Lexer
import Neodoc.Spec.Token
import Neodoc.Spec.TokenParser
import Neodoc.Spec.ParserState
import Neodoc.Spec.ParserState as ParserState

traceState :: TokenParser String
traceState = do
  (ParserState indentation line) <- lift State.get
  pure $ "(" <> (show line) <> "|" <> (show indentation) <> ")"

-- |
-- Get the position of the token at the head of the stream.
--
getTokenPosition :: TokenParser P.Position
getTokenPosition = do
  toks <- getInput
  case toks of
    (PositionedToken pos _):xs -> pure pos
    _                          -> getPosition

-- |
-- Mark the current indentation level
--
markIndent :: ∀ a. TokenParser a -> TokenParser a
markIndent p = do
  currentIndent <- ParserState.getIndentation <$> lift State.get
  P.Position { column } <- getTokenPosition
  P.try do
    lift $ State.modify (flip ParserState.setIndentation column)
    a <- p
    lift $ State.modify (flip ParserState.setIndentation currentIndent)
    pure a

-- |
-- Mark the current line
--
markLine :: ∀ a. TokenParser a -> TokenParser a
markLine p = do
  currentLine <- ParserState.getLine <$> lift State.get
  P.Position { line } <- getTokenPosition
  P.try do
    (lift $ State.modify (flip ParserState.setLine line))
      *> p <*
    (lift $ State.modify (flip ParserState.setLine currentLine))

-- |
-- Mark a custom indentation level
--
markIndent' :: ∀ a. Int -> TokenParser a -> TokenParser a
markIndent' level p = do
  current <- ParserState.getIndentation <$> lift State.get
  lift $ State.modify (flip ParserState.setIndentation level)
  a <- p
  lift $ State.modify (flip ParserState.setIndentation current)
  pure a

-- |
-- Check that the current identation level matches a predicate
--
checkIndentation :: (Int -> Int -> Boolean) -> TokenParser Unit
checkIndentation rel = do
  P.Position { column } <- getTokenPosition
  current <- ParserState.getIndentation <$> lift State.get
  guard (column `rel` current)

-- |
-- Check that the current indentation level is past the current mark
--
indented :: TokenParser Unit
indented = checkIndentation (>=) P.<?> "indentation"

-- |
-- Check that the current indentation level is before or at the current mark
--
lessOrEquallyIndented :: TokenParser Unit
lessOrEquallyIndented = checkIndentation (<=) P.<?> "less or equal indentation"

-- |
-- Check that the current indentation level is before the current mark
--
moreIndented :: TokenParser Unit
moreIndented = checkIndentation (>) P.<?> "more indentation"

-- |
-- Check that the current indentation level is before the current mark
--
lessIndented :: TokenParser Unit
lessIndented = checkIndentation (<) P.<?> "less indentation"

-- |
-- Check that the current indentation level is at the same indentation as the
-- current mark
--
sameIndent :: TokenParser Unit
sameIndent = checkIndentation (==) P.<?> "same indentation"

-- |
-- Check that the current line matches a predicate
--
checkLine :: (Int -> Int -> Boolean) -> TokenParser Unit
checkLine rel = do
  P.Position { line } <- getTokenPosition
  current <- ParserState.getLine <$> lift State.get
  guard (line `rel` current)

sameLine :: TokenParser Unit
sameLine = checkLine (==) P.<?> "same line"

successiveLine :: TokenParser Unit
successiveLine = checkLine (>) P.<?> "successive line"

