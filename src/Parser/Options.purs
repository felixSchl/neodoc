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
import Data.Maybe
import Docopt.Parser.Base
import Docopt.Parser.Lexer
