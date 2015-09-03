module Test.Main where

import Prelude
import Control.Monad.Eff.Console (log)
import Text.Parsing.Parser (runParser)
import qualified Docopt.Parsers.Meta as Docopt
import qualified Data.String as Str


runMeta s i = flip runParser (Docopt.meta s) i

main = do
  let x = runMeta "naval-fate" $
"""
Usage:
  naval-fate [ --verbose <x>    --blah ]

Options:
  -vBLAH, --verbose=BLAH  foo bar qux baz see u mma aa
"""
  log $ show x
