module Docopt.ParserGen (
    module Docopt.ParserGen.Types
  , module Docopt.ParserGen.Pretty
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

import qualified Docopt.Types as D
import qualified Docopt.ParserGen.Types  as G
import qualified Docopt.ParserGen.Pretty as G
import qualified Docopt.ParserGen.Parser as G
import qualified Docopt.ParserGen.Trans  as G
import qualified Docopt.ParserGen.Lexer  as G

genParser :: List D.Application
          -> G.Parser (Tuple D.Branch (List G.ValueMapping))
genParser as = foldl (<|>) empty (G.genParser <$> as)

runParser :: List String
          -> G.Parser (Tuple D.Branch (List G.ValueMapping))
          -> Either P.ParseError (Map D.Argument D.Value)
runParser xs p = do
  toks <- G.lex xs
  vals <- P.runParser toks p
  return $ G.reduce vals

