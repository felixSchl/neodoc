-- |
-- | This module defines commonly used TokenParser-combinators
-- |

module Docopt.Parser.Common where

import Prelude
import Debug.Trace
import Control.Monad.Trans (lift)
import Control.MonadPlus (guard)
import Control.Monad.State
import Data.List (List(..))
import Data.Either (Either(..))
import Docopt.Parser.Lexer
import Docopt.Parser.State
import Docopt.Parser.Base
import qualified Text.Parsing.Parser as P
import qualified Text.Parsing.Parser.Combinators as P
import qualified Text.Parsing.Parser.Token as P
import qualified Text.Parsing.Parser.Pos as P
import qualified Text.Parsing.Parser.String as P

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
  ParserState { indentation: current } <- lift get
  P.Position { column: pos } <- getTokenPosition
  lift $ modify \(ParserState st) -> ParserState { indentation: pos }
  a <- p
  lift $ modify \(ParserState st) -> ParserState { indentation: current }
  return a

-- |
-- Mark a custom  indentation level
--
markIndent' :: forall a. Int -> TokenParser a -> TokenParser a
markIndent' level p = do
  ParserState { indentation: current } <- lift get
  lift $ modify \(ParserState st) -> ParserState { indentation: level }
  a <- p
  lift $ modify \(ParserState st) -> ParserState { indentation: current }
  return a

-- |
-- Check that the current identation level matches a predicate
--
checkIndentation :: (Int -> Int -> Boolean) -> TokenParser Unit
checkIndentation rel = do
  P.Position { column: col } <- getTokenPosition
  ParserState { indentation: current } <- lift get
  guard (col `rel` current)

-- |
-- Check that the current indentation level is past the current mark
--
indented :: TokenParser Unit
indented = checkIndentation (>=) P.<?> "indentation"

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
