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

debug :: forall a m. (Show a, Monad m) => a -> m Unit
debug x = traceShow x $ const $ pure unit

-- | Return the current parser position
getPosition :: forall a m. (Monad m) => P.ParserT a m P.Position
getPosition = P.ParserT $ \(P.PState { input: s, position: pos }) ->
  return { input: s, result: Right pos, consumed: false, position: pos }

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

tryMaybe :: forall s m a. (Monad m) => P.ParserT s m a -> P.ParserT s m (Maybe a)
tryMaybe p = (Just <$> p) <|> (pure Nothing)

-- | Return the current parser position
getInput :: forall a m. (Monad m, Show a) => P.ParserT a m a
getInput = P.ParserT $ \(P.PState { input: s, position: pos }) ->
  return { input: s, result: Right s, consumed: false, position: pos }

traceInput :: forall a m. (Show a, Monad m) => P.ParserT a m Unit
traceInput = getInput >>= debug

regex :: String -> P.Parser String Char
regex s = P.satisfy \c ->
  Regex.test (Regex.regex s Regex.noFlags) (toString c)

lower :: P.Parser String Char
lower = P.satisfy \c -> c == toLower c

upper :: P.Parser String Char
upper = P.satisfy \c -> c == toUpper c

digit :: P.Parser String Char
digit = regex "[0-9]"

alpha :: P.Parser String Char
alpha = regex "[a-zA-Z]"

upperAlpha :: P.Parser String Char
upperAlpha = regex "[A-Z]"

lowerAlpha :: P.Parser String Char
lowerAlpha = regex "[a-z]"

lowerAlphaNum :: P.Parser String Char
lowerAlphaNum = regex "[0-9a-z]"

upperAlphaNum :: P.Parser String Char
upperAlphaNum = regex "[0-9A-Z]"

alphaNum :: P.Parser String Char
alphaNum = alpha <|> digit

space :: P.Parser String Char
space = P.char ' '

eol :: P.Parser String Unit
eol = (void $ P.string "\r\n") <|> (void $ P.char '\n')

string' :: String -> P.Parser String String
string' s = fromCharArray <$> A.foldM step [] (toCharArray s)
  where
    step acc x = do
      (acc ++) <<< A.singleton <$> P.satisfy \c -> toLower c == toLower x
