-- |
-- | This module defines commonly used TokenParser-combinators
-- |

module Docopt.Spec.Parser.Common where

import Prelude
import Debug.Trace
import Control.Monad.Trans (lift)
import Control.MonadPlus (guard)
import Control.Monad.State
import Data.List (List(..))
import Data.Either (Either(..))
import Docopt.Spec.Parser.Lexer
import Docopt.Spec.Parser.State
import Docopt.Spec.Parser.Base
import qualified Text.Parsing.Parser as P
import qualified Text.Parsing.Parser.Combinators as P
import qualified Text.Parsing.Parser.Token as P
import qualified Text.Parsing.Parser.Pos as P
import qualified Text.Parsing.Parser.String as P

traceState :: TokenParser Unit
traceState = do
  (st :: ParserState) <- lift get
  debug $ "(" ++ (show st.line) ++ "|" ++ (show st.indentation) ++ ")"

-- |
-- Get the position of the token at the head of the stream.
--
getTokenPosition :: TokenParser P.Position
getTokenPosition = P.ParserT $ \(P.PState { input: s, position: pos }) ->
  return case s of
    Cons (PositionedToken { sourcePos: spos }) _ ->
      { input: s, result: Right spos, consumed: false, position: pos }
    _ ->
      { input: s, result: Right pos, consumed: false, position: pos }

-- |
-- Mark the current indentation level
--
markIndent :: forall a. TokenParser a -> TokenParser a
markIndent p = do
  { indentation: current } <- lift get
  P.Position { column: pos } <- getTokenPosition
  lift $ modify \st -> ((st { indentation = pos }) :: ParserState)
  a <- p
  lift $ modify \st -> ((st { indentation = current }) :: ParserState)
  return a

-- |
-- Mark the current line
--
markLine :: forall a. TokenParser a -> TokenParser a
markLine p = do
  { line: current } <- lift get
  P.Position { line: pos } <- getTokenPosition
  lift $ modify \st -> ((st { line = pos }) :: ParserState)
  a <- p
  lift $ modify \st -> ((st { line = current }) :: ParserState)
  return a

-- |
-- Mark a custom indentation level
--
markIndent' :: forall a. Int -> TokenParser a -> TokenParser a
markIndent' level p = do
  { indentation: current } <- lift get
  lift $ modify \st -> ((st { indentation = level }) :: ParserState)
  a <- p
  lift $ modify \st -> ((st { indentation = current }) :: ParserState)
  return a

-- |
-- Check that the current identation level matches a predicate
--
checkIndentation :: (Int -> Int -> Boolean) -> TokenParser Unit
checkIndentation rel = do
  P.Position { column: col } <- getTokenPosition
  { indentation: current } <- lift get
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
sameIndent = checkIndentation (==) P.<?> "no indentation"

-- |
-- Check that the current line matches a predicate
--
checkLine :: (Int -> Int -> Boolean) -> TokenParser Unit
checkLine rel = do
  P.Position { line: line } <- getTokenPosition
  { line: current } <- lift get
  guard (line `rel` current)

sameLine :: TokenParser Unit
sameLine = checkLine (==) P.<?> "same line"

successiveLine :: TokenParser Unit
successiveLine = checkLine (>) P.<?> "successive line"
