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

-- | Return the current parser position
getPosition :: forall a. P.Parser a P.Position
getPosition = P.ParserT $ \(P.PState { input: s, position: pos }) ->
  return { input: s, result: Right pos, consumed: true, position: pos }

-- | Match a char against a regex
regex :: String -> P.Parser String Char
regex s = P.satisfy \c ->
  Regex.test (Regex.regex s Regex.noFlags) (toString c)

-- -- | Parse any lower case character
lower :: P.Parser String Char
lower = P.satisfy \c -> c == toLower c

-- -- | Parse any upper case character
upper :: P.Parser String Char
upper = P.satisfy \c -> c == toUpper c

-- | Parse any number
num :: P.Parser String Char
num = regex "[0-9]"

-- | Parse any alphabetical character
alpha :: P.Parser String Char
alpha = regex "[a-zA-Z]"

-- | Parse any number or alphabetical character
alphaNum = alpha <|> num
