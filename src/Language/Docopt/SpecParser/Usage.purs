module Language.Docopt.SpecParser.Usage (
    run
  , module Reexports
  ) where

import Prelude
import Data.List (List)
import Data.Either (Either)
import Text.Parsing.Parser (ParseError) as P

import Language.Docopt.SpecParser.Lexer as Lexer
import Language.Docopt.SpecParser.Usage.Parser as UsageParser
import Language.Docopt.SpecParser.Usage.Parser (parse) as Reexports
import Language.Docopt.SpecParser.Usage.Usage as Reexports

import Language.Docopt.SpecParser.Usage.Usage (Usage(..))

run
  :: String  -- ^ The usage section text
  -> Boolean -- ^ Enable "smart-options"
  -> Either P.ParseError {
      program :: String
    , usages  :: List Usage
    }
run x enableSmartOpts = Lexer.lexUsage x >>= UsageParser.parse enableSmartOpts
