module Docopt.Parser.Gen where

import Prelude
import Data.Either
import Docopt.Parser.Usage
import Data.List (List(..), fromList)
import qualified Text.Parsing.Parser as P
import qualified Text.Parsing.Parser.Combinators as P
import qualified Text.Parsing.Parser.Pos as P
import qualified Text.Parsing.Parser.String as P

generateParser :: List Usage -> Either String (P.Parser String Unit)
generateParser usages = do
  pure $ pure unit
