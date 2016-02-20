module Docopt.Gen
( module Docopt.Gen.Types
, module Docopt.Gen.Pretty
, run
) where

import Prelude
import Data.List (List(..))
import Data.Either (Either(..))
import Docopt.Gen.Types (ValueMapping(), Token())
import qualified Text.Parsing.Parser as P
import Docopt.Types
import Docopt.Gen.Lexer (lex)
import Docopt.Gen.Parser (mkApplicationParser)
import Data.Foldable (foldl)
import Control.Alt ((<|>))
import Control.Plus (empty)
import Docopt.Gen.Types
import Docopt.Gen.Pretty

run :: List Application
    -> List String
    -> Either P.ParseError (List ValueMapping)
run as xs = do
  toks <- lex xs
  P.runParser toks parser

  where
    parser :: P.Parser (List Token) (List ValueMapping)
    parser = foldl (<|>) empty (mkApplicationParser <$> as)
