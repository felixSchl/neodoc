module Test.Main where

import Prelude
import Control.Monad.Eff.Console (log)
import Text.Parsing.Parser (runParser)
import qualified Docopt.Parsers.Meta as Docopt
import qualified Data.String as Str

runUsage s i = flip runParser (Docopt.usage s) i
runMeta s i = flip runParser (Docopt.meta s) i -- (Str.split "\n" i)

main = do
  let x = runMeta "naval-fate" $
"""
Usage:
  naval-fate -vvv
             -v
"""
  log $ show x
