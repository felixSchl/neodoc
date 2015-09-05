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

debug :: forall a b. (Show a) => a -> P.Parser b Unit
debug x = traceShow x \_ -> pure unit

debugInput :: forall a. (Show a) => P.Parser a Unit
debugInput = P.ParserT $ \(P.PState { input: s, position: pos }) ->
  traceShow s \_ ->
    return { input: s, result: Right unit, consumed: false, position: pos }

-- | Return the current parser position
getPosition :: forall a. P.Parser a P.Position
getPosition = P.ParserT $ \(P.PState { input: s, position: pos }) ->
  return { input: s, result: Right pos, consumed: false, position: pos }

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
