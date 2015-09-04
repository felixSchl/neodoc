module Test.Main where

import Prelude
import Control.Monad.Eff.Console (log)
import Text.Parsing.Parser (runParser)
import qualified Docopt.Parser.Lexer as Lexer
import qualified Data.String as Str

main = do
  let x = runParser
            "(<a>)"
            Lexer.parseTokens
  log $ show x
