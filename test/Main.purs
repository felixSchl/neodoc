module Test.Main where

import Prelude
import Control.Monad.Eff.Console (log)
import Text.Parsing.Parser (runParser)
import qualified Docopt.Parser.Lexer as Lexer
import qualified Data.String as Str

main = do
  let x = runParser
            "<abc>"
            Lexer.parseTokens
      y = flip Lexer.runTokenParser (Lexer.lparen) <$> x
  log $ show x
  log $ show y
