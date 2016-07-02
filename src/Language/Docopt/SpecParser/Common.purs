-- |
-- | This module defines commonly used TokenParser-combinators
-- |

module Language.Docopt.SpecParser.Common where

import Prelude
import Control.Monad.Trans (lift)
import Control.MonadPlus (guard)
import Control.Monad.Transformerless.State (State, evalState)
import Control.Monad.Transformerless.State (get, modify) as State
import Data.List (List(..))
import Data.Either (Either(..))
import Language.Docopt.SpecParser.Lexer (TokenParser, PositionedToken(..))
import Language.Docopt.SpecParser.State (ParserState)
import Text.Parsing.Parser (PState(..), ParserT(..), Result(..)) as P
import Text.Parsing.Parser.Combinators ((<?>), try) as P
import Text.Parsing.Parser.Pos (Position(..)) as P

traceState :: TokenParser String
traceState = do
  (st :: ParserState) <- lift State.get
  pure $ "(" <> (show st.line) <> "|" <> (show st.indentation) <> ")"

-- |
-- Get the position of the token at the head of the stream.
--
getTokenPosition :: TokenParser P.Position
getTokenPosition = P.ParserT $ \(P.PState s pos) ->
  pure case s of
    Cons (PositionedToken spos _) _ ->
      P.Result s (Right spos) false pos
    otherwise ->
      P.Result s (Right pos) false pos

-- |
-- Mark the current indentation level
--
markIndent :: forall a. TokenParser a -> TokenParser a
markIndent p = do
  { indentation: current } <- lift State.get
  P.Position _ col <- getTokenPosition
  P.try do
    lift $ State.modify \st -> ((st { indentation = col }) :: ParserState)
    a <- p
    lift $ State.modify \st -> ((st { indentation = current }) :: ParserState)
    pure a

-- |
-- Mark the current line
--
markLine :: forall a. TokenParser a -> TokenParser a
markLine p = do
  { line: current } <- lift State.get
  P.Position pos _ <- getTokenPosition
  P.try do
    lift $ State.modify \st -> ((st { line = pos }) :: ParserState)
    a <- p
    lift $ State.modify \st -> ((st { line = current }) :: ParserState)
    pure a

-- |
-- Mark a custom indentation level
--
markIndent' :: forall a. Int -> TokenParser a -> TokenParser a
markIndent' level p = do
  { indentation: current } <- lift State.get
  lift $ State.modify \st -> ((st { indentation = level }) :: ParserState)
  a <- p
  lift $ State.modify \st -> ((st { indentation = current }) :: ParserState)
  pure a

-- |
-- Check that the current identation level matches a predicate
--
checkIndentation :: (Int -> Int -> Boolean) -> TokenParser Unit
checkIndentation rel = do
  P.Position _ col <- getTokenPosition
  { indentation: current } <- lift State.get
  guard (col `rel` current)

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
  P.Position line _ <- getTokenPosition
  { line: current } <- lift State.get
  guard (line `rel` current)

sameLine :: TokenParser Unit
sameLine = checkLine (==) P.<?> "same line"

successiveLine :: TokenParser Unit
successiveLine = checkLine (>) P.<?> "successive line"
