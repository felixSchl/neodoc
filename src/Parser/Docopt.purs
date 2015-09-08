-- |
-- | This module defines the entry point and surface area of the Docopt
-- | compiler.
-- |

module Docopt.Parser.Docopt where

import Prelude
import Data.Either
import qualified Text.Parsing.Parser as P
import qualified Text.Parsing.Parser.Combinators as P
import qualified Text.Parsing.Parser.Pos as P
import qualified Text.Parsing.Parser.String as P
import qualified Docopt.Parser.Lexer as Lexer
import qualified Docopt.Parser.Usage as Usage
import qualified Docopt.Parser.Scanner as Scanner
import Docopt.Parser.Base (debug)

docopt :: String -> Either P.ParseError Unit
docopt input = do

  debug "Scanning..."
  Scanner.Docopt usageSrc _ <- P.runParser input Scanner.scanDocopt
  debug usageSrc

  debug "Lexing..."
  usageToks         <- P.runParser usageSrc Lexer.parseTokens
  debug usageToks

  debug "Parsing..."
  usage             <- Lexer.runTokenParser usageToks Usage.parseUsage
  debug usage
