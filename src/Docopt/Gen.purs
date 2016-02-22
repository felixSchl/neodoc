module Docopt.Gen
( module Docopt.Gen.Types
, module Docopt.Gen.Pretty
, run
) where

import Prelude
import Data.List (List(..))
import Data.Either (Either(..))
import Data.Map (Map(..))
import qualified Data.Map as Map
import Docopt.Gen.Types (ValueMapping(), Token())
import qualified Text.Parsing.Parser as P
import Docopt.Gen.Lexer (lex)
import Docopt.Gen.Parser (mkApplicationParser)
import Data.Foldable (foldl)
import Control.Alt ((<|>))
import Control.Plus (empty)
import Docopt.Gen.Types
import Docopt.Gen.Pretty
import qualified Docopt.Types as D

run :: List D.Application
    -> List String
    -> Either P.ParseError (Map D.Argument D.Value)
run as xs = do
  toks <- lex xs
  P.runParser toks parser

  where
    parser :: P.Parser (List Token) (Map D.Argument D.Value)
    parser = foldl (<|>) empty (mkApplicationParser <$> as)
