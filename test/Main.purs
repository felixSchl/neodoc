module Test.Main where

import Prelude
import Control.Monad.Eff.Console (log)
import Text.Parsing.Parser (runParser)
import qualified Docopt.Parser.Docopt as Docopt
import qualified Docopt.Parser.Lexer as Lexer
import qualified Data.String as Str

input =
"""
Naval-Fate.

Usage:
  naval_fate -vvv

Options:
  -h --help     Show this screen.
  --version     Show version.
  --speed=<kn>  Speed in knots [default: 10].
  --moored      Moored (anchored) mine.
  --drifting    Drifting mine.
"""

main = do

  let x = Docopt.docopt input
  log $ show x

  -- let x = runParser
  --           input
  --           Docopt.prelex
  -- log $ show x
  --
  -- let x = runParser
  --           "<arg-name> HELLO-WORLD"
  --           Lexer.parseTokens
  --     y = flip Lexer.runTokenParser (Lexer.lparen) <$> x
  -- -- log $ show x
  -- log  show x
  -- log $ show y
