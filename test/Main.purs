module Test.Main where

import Prelude
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Trans (lift)
import Text.Parsing.Parser (runParser)
import qualified Docopt.Parser.Docopt as Docopt
import qualified Docopt.Parser.Lexer as Lexer
import qualified Docopt.Parser.Scanner as Scanner
import Data.Either (either)

import Test.Spec (describe, it)
import Test.Spec.Runner (run)
import Test.Spec.Reporter.Console (consoleReporter)

main = run [consoleReporter] do
  describe "docopt scanner" do
    it "breaks apart the input" do
      either
        (throwError <<< error <<< show)
        (const $ pure unit)
        (Scanner.scanDocopt
          """
          Naval Fate.

          Usage:
          naval_fate -xvzf FILE
          """
        )

-- source =
-- """
-- Naval-Fate. Usage: varies.
--
-- UsAgE: 
--
--   naval_fate ask <question> | (foo
--     bar)
--   naval_fate   run <command>
--
-- The program can be used in many ways.
-- Consider, for example:
-- Blah.
--
-- Options:
--
--   -h --help     Show this screen.
--   --version     Show version.
--   --speed=<kn>  Speed in knots [default: 10].
--   --moored      Moored (anchored) mine.
--   --drifting    Drifting mine.
-- """
--
-- main = do
--   let x = Docopt.docopt
--             source
--             "naval_fate"
--   log $ show x
