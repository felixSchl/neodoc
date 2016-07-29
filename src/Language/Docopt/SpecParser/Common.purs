-- |
-- | This module defines commonly used TokenParser-combinators
-- |

module Language.Docopt.SpecParser.Common where

import Prelude
import Control.Monad.Trans (lift)
import Control.MonadPlus (guard)
import Control.Monad.Transformerless.State (State, evalState)
import Control.Monad.Transformerless.State (get, modify) as State
import Data.List (List(..), (:))
import Data.Either (Either(..))
import Language.Docopt.SpecParser.Lexer (TokenParser, PositionedToken(..))
import Language.Docopt.SpecParser.State
import Language.Docopt.SpecParser.State as ParserState
import Text.Parsing.Parser (PState(..), ParserT(..), Result(..)) as P
import Text.Parsing.Parser.Combinators ((<?>), try) as P
import Text.Parsing.Parser.Pos (Position(..)) as P

traceState :: TokenParser String
traceState = do
  (ParserState indentation line) <- lift State.get
  pure $ "(" <> (show line) <> "|" <> (show indentation) <> ")"

-- |
-- Get the position of the token at the head of the stream.
--
getTokenPosition :: TokenParser P.Position
getTokenPosition = P.ParserT $ \(P.PState s pos) ->
  pure case s of
    (PositionedToken spos _):_ -> P.Result s (Right spos) false pos
    otherwise                  -> P.Result s (Right pos)  false pos

-- |
-- Mark the current indentation level
--
markIndent :: forall a. TokenParser a -> TokenParser a
markIndent p = do
  currentIndent <- ParserState.getIndentation <$> lift State.get
  P.Position _ col <- getTokenPosition
  P.try do
    lift $ State.modify (flip ParserState.setIndentation col)
    a <- p
    lift $ State.modify (flip ParserState.setIndentation currentIndent)
    pure a

-- |
-- Mark the current line
--
markLine :: forall a. TokenParser a -> TokenParser a
markLine p = do
  currentLine <- ParserState.getLine <$> lift State.get
  P.Position tokLine _ <- getTokenPosition
  P.try do
    lift $ State.modify (flip ParserState.setLine tokLine)
    a <- p
    lift $ State.modify (flip ParserState.setLine currentLine)
    pure a

-- |
-- Mark a custom indentation level
--
markIndent' :: forall a. Int -> TokenParser a -> TokenParser a
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
  P.Position _ col <- getTokenPosition
  current <- ParserState.getIndentation <$> lift State.get
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
  current <- ParserState.getLine <$> lift State.get
  guard (line `rel` current)

sameLine :: TokenParser Unit
sameLine = checkLine (==) P.<?> "same line"

successiveLine :: TokenParser Unit
successiveLine = checkLine (>) P.<?> "successive line"
