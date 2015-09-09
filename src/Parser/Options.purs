-- | This module parses the options section of the Docopt source string.
-- |
-- | ```
-- |   -h --help     Show this screen.
-- |   --version     Show version.
-- |   --speed=<kn>  Speed in knots [default: 10].
-- |   --moored      Moored (anchored) mine.
-- |   --drifting    Drifting mine.
-- | ```
-- |
-- | XXX:
-- | Because of the structure of the string, lexing and parsing must be
-- | interwoven to some degree. It would be great, however to re-use the
-- | existing TokenParsers written for the usage section somehow. Maybe
-- | a `pre-lex -> lex -> parse` method is justified? Or just ignore all
-- | unknown tokens.
-- |
-- | Known tokens would be [ SOpt, LOpt, Comma, '--', ':', '[default', ']'
-- |                       , StringLiteral, NumericLiteral ]
-- |
-- | During parsing, we would look for either:
-- |
-- | [ Sopt, Comma, Lopt ]
-- | [ Sopt, Lopt ]
-- | [ Lopt ]
-- | [ -- ]
-- |
-- | All following tokens are then part of the description, mostly garbage.
-- | However, we keep an eye open for '[default' and ']' and the StringLiteral
-- | and NumericLiterals contained in it.

module Docopt.Parser.Options where

import Prelude
import Control.Lazy (defer)
import Control.Alt ((<|>))
import Control.Apply ((*>), (<*))
import Data.List (List(..), some, (:), toList)
import qualified Text.Parsing.Parser as P
import qualified Text.Parsing.Parser.Combinators as P
import qualified Text.Parsing.Parser.Pos as P
import qualified Text.Parsing.Parser.String as P
import Data.Either
import Data.Maybe hiding (maybe)
import Docopt.Parser.Base
import Docopt.Parser.Lexer


