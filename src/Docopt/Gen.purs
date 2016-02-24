module Docopt.Gen(
  module Docopt.Gen.Types
, module Docopt.Gen.Pretty
, genParser
, runParser
) where

import Prelude
import Data.List (List(..))
import Data.Either (Either(..))
import Data.Map (Map())
import qualified Data.Map as Map
import Data.Tuple (Tuple(..))
import qualified Text.Parsing.Parser as P
import Data.Foldable (foldl)
import Control.Alt ((<|>))
import Control.Plus (empty)
import Docopt.Gen.Types
import Docopt.Gen.Pretty
import Docopt.Gen.Lexer (lex)
import Docopt.Gen.Types (Token(), ValueMapping())
import qualified Docopt.Types as D
import qualified Docopt.Gen.Parser as Gen
import qualified Docopt.Gen.Reduce as Gen

genParser :: List D.Application
          -> CliParser (Tuple D.Branch (List ValueMapping))
genParser as = foldl (<|>) empty (Gen.genParser <$> as)

runParser :: List String
          -> CliParser (Tuple D.Branch (List ValueMapping))
          -> Either P.ParseError (Map D.Argument D.Value)
runParser xs p = do
  toks <- lex xs
  vals <- P.runParser toks p
  return $ Gen.reduce vals

