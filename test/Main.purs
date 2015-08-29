module Test.Main where

import Prelude
import Control.Monad.Eff.Console (log)
import Text.Parsing.Parser (runParser)
import qualified Docopt.Parsers.Meta as Docopt

runUsage s i = flip runParser (Docopt.usage s) i

main = do
  let x = runUsage "naval-fate" "naval-fate -vvv"
  log $ show x
