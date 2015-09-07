module Docopt.Parser.Common where

import Prelude
import Control.Monad.Trans (lift)
import Control.MonadPlus (guard)
import Control.Monad.State
import Docopt.Parser.Lexer
import Docopt.Parser.State
import Docopt.Parser.Base
import qualified Text.Parsing.Parser as P
import qualified Text.Parsing.Parser.Combinators as P
import qualified Text.Parsing.Parser.Token as P
import qualified Text.Parsing.Parser.Pos as P
import qualified Text.Parsing.Parser.String as P

-- |
-- Mark the current indentation level
--
mark :: forall a. TokenParser a -> TokenParser a
mark p = do
  ParserState { indentation: current } <- lift get
  P.Position { column: pos } <- getPosition
  lift $ modify \(ParserState st) -> ParserState { indentation: pos }
  a <- p
  lift $ modify \(ParserState st) -> ParserState { indentation: current }
  return a

-- |
-- Check that the current identation level matches a predicate
--
checkIndentation :: (Int -> Int -> Boolean) -> TokenParser Unit
checkIndentation rel = do
  P.Position { column: col } <- getPosition
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
lessIndented :: TokenParser Unit
lessIndented = checkIndentation (<) P.<?> "less indentation"

-- |
-- Check that the current indentation level is at the same indentation as the
-- current mark
--
same :: TokenParser Unit
same = checkIndentation (==) P.<?> "no indentation"
