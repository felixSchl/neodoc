module Docopt.Parser.Base where

import Prelude
import Control.Alt ((<|>))
import qualified Text.Parsing.Parser as P
import qualified Text.Parsing.Parser.Combinators as P
import qualified Text.Parsing.Parser.Pos as P
import qualified Text.Parsing.Parser.String as P
import qualified Data.String.Regex as Regex
import Data.Char (toString, toLower, toUpper)
import Data.Maybe
import Data.Either
import Debug.Trace

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

-- | Return the current parser position
getInput :: forall a m. (Monad m, Show a) => P.ParserT a m a
getInput = P.ParserT $ \(P.PState { input: s, position: pos }) ->
  return { input: s, result: Right s, consumed: false, position: pos }

regex :: String -> P.Parser String Char
regex s = P.satisfy \c ->
  Regex.test (Regex.regex s Regex.noFlags) (toString c)

lower :: P.Parser String Char
lower = P.satisfy \c -> c == toLower c

upper :: P.Parser String Char
upper = P.satisfy \c -> c == toUpper c

num :: P.Parser String Char
num = regex "[0-9]"

alpha :: P.Parser String Char
alpha = regex "[a-zA-Z]"

upperAlpha :: P.Parser String Char
upperAlpha = regex "[A-Z]"

lowerAlpha :: P.Parser String Char
lowerAlpha = regex "[a-z]"

alphaNum :: P.Parser String Char
alphaNum = alpha <|> num

space :: P.Parser String Char
space = P.char ' '
