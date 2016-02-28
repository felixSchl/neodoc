module Language.Docopt.ParserGen (
    module Language.Docopt.ParserGen.Token
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

import qualified Language.Docopt.Value    as D
import qualified Language.Docopt.Errors   as D
import qualified Language.Docopt.Argument as D
import qualified Language.Docopt.Usage    as D
import qualified Language.Docopt.ParserGen.Token        as G
import qualified Language.Docopt.ParserGen.Parser       as G
import qualified Language.Docopt.ParserGen.Trans        as G
import qualified Language.Docopt.ParserGen.Lexer        as G
import qualified Language.Docopt.ParserGen.ValueMapping as G

genParser :: D.Program
          -> G.Parser (Tuple D.Branch (List G.ValueMapping))
genParser us = foldl (<|>) empty (G.genUsageParser <$> us)

runParser :: List String
          -> G.Parser (Tuple D.Branch (List G.ValueMapping))
          -> Either P.ParseError (Map D.Argument D.Value)
runParser xs p = do
  toks <- G.lex xs
  vals <- P.runParser toks p
  return $ G.reduce vals

