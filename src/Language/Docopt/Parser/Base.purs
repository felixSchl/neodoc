-- |
-- | This module defines commonly used Parser-combinators
-- |

module Language.Docopt.Parser.Base where

import Prelude
import Control.Alt ((<|>))
import Control.Apply ((*>), (<*))
import Control.Monad.State (get)
import Control.Monad.Trans (lift)
import qualified Text.Parsing.Parser as P
import qualified Text.Parsing.Parser.Combinators as P
import qualified Text.Parsing.Parser.Pos as P
import qualified Text.Parsing.Parser.String as P
import qualified Data.String.Regex as Regex
import qualified Data.Array as A
import Data.Array ((:))
import Data.Char (toString, toLower, toUpper)
import Data.String (fromCharArray, toCharArray, fromChar)
import Data.Maybe hiding (maybe)
import Data.Either
import Debug.Trace
import Data.Tuple (Tuple(..))
import Control.MonadPlus (guard)

debug :: forall a m. (Show a, Monad m) => a -> m Unit
debug x = traceShow x $ const $ pure unit

-- | Return the current parser position
getPosition :: forall a m. (Monad m) => P.ParserT a m P.Position
getPosition = P.ParserT $ \(P.PState { input: s, position: pos }) ->
  return { input: s, result: Right pos, consumed: false, position: pos }

sof :: forall a m. (Monad m) => P.ParserT a m Unit
sof = do
  P.Position { column, line } <- getPosition
  guard $ column == 1 && line == 1

-- | Return the current parser column
getCol :: forall a m. (Monad m) => P.ParserT a m Int
getCol = do
  P.Position { column: col } <- getPosition
  return col

-- | Return the current parser row
-- | XXX: Use either `line` or `row` - not both!
getRow :: forall a m. (Monad m) => P.ParserT a m Int
getRow = do
  P.Position { line: row } <- getPosition
  return row

-- | Return the current parser row
-- | XXX: Use either `line` or `row` - not both!
getLine = getRow

tryMaybe :: forall s m a. (Monad m) => P.ParserT s m a -> P.ParserT s m (Maybe a)
tryMaybe p = (Just <$> p) <|> (pure Nothing)

-- | Return the current parser position
getInput :: forall a m. (Monad m, Show a) => P.ParserT a m a
getInput = P.ParserT $ \(P.PState { input: s, position: pos }) ->
  return { input: s, result: Right s, consumed: false, position: pos }

traceInput :: forall a m. (Show a, Monad m) => P.ParserT a m Unit
traceInput = getInput >>= debug

regex :: forall m. (Monad m) => String -> P.ParserT String m Char
regex s = P.satisfy \c ->
  Regex.test (Regex.regex s Regex.noFlags) (toString c)

lower :: forall m. (Monad m) => P.ParserT String m Char
lower = P.satisfy \c -> c == toLower c

upper :: forall m. (Monad m) => P.ParserT String m Char
upper = P.satisfy \c -> c == toUpper c

digit :: forall m. (Monad m) => P.ParserT String m Char
digit = regex "[0-9]"

alpha :: forall m. (Monad m) => P.ParserT String m Char
alpha = regex "[a-zA-Z]"

upperAlpha :: forall m. (Monad m) => P.ParserT String m Char
upperAlpha = regex "[A-Z]"

lowerAlpha :: forall m. (Monad m) => P.ParserT String m Char
lowerAlpha = regex "[a-z]"

lowerAlphaNum :: forall m. (Monad m) => P.ParserT String m Char
lowerAlphaNum = regex "[0-9a-z]"

upperAlphaNum :: forall m. (Monad m) => P.ParserT String m Char
upperAlphaNum = regex "[0-9A-Z]"

alphaNum :: forall m. (Monad m) => P.ParserT String m Char
alphaNum = alpha <|> digit

space :: forall m. (Monad m) => P.ParserT String m Char
space = P.char ' '

eol :: forall m. (Monad m) => P.ParserT String m Unit
eol = (void $ P.string "\r\n") <|> (void $ P.char '\n')

string' :: forall m. (Monad m) => String -> P.ParserT String m String
string' s = fromCharArray <$> A.foldM step [] (toCharArray s)
  where
    step acc x = do
      (acc ++) <<< A.singleton <$> P.satisfy \c -> toLower c == toLower x
