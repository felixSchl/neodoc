module Language.Docopt.ParserGen (
    module Language.Docopt.ParserGen.Token
  , genParser
  , runParser
  ) where

import Prelude
import Debug.Trace
import Data.List (List(..))
import Data.Either (Either(..))
import Data.StrMap as StrMap
import Data.StrMap (StrMap())
import Data.Map as Map
import Data.Map (Map())
import Data.List (toList)
import Data.Tuple (Tuple(..), uncurry)
import Text.Parsing.Parser as P
import Data.Foldable (foldl)
import Control.Alt ((<|>))
import Control.Plus (empty)

import Language.Docopt.Value    as D
import Language.Docopt.Errors   as D
import Language.Docopt.Argument as D
import Language.Docopt.Usage    as D
import Language.Docopt.ParserGen.Token        as G
import Language.Docopt.ParserGen.Parser       as G
import Language.Docopt.ParserGen.Lexer        as G
import Language.Docopt.ParserGen.ValueMapping as G

-- | Generate a parser for a given program specification.
genParser :: D.Program
          -> G.Parser (Tuple D.Branch (List G.ValueMapping))
genParser us = foldl (<|>) empty (G.genUsageParser <$> us)

-- | Run a parser against user input.
runParser :: Array String
          -> G.Parser (Tuple D.Branch (List G.ValueMapping))
          -> Either P.ParseError (Tuple D.Branch (List G.ValueMapping))
runParser argv p = do
  toks <- G.lex (toList argv)
  P.runParser toks p
