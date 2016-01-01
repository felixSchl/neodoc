module Test.Spec.SolverSpec (solverSpec) where

import Prelude
import Debug.Trace
import Control.Bind ((=<<))
import Data.List (List(..))
import Control.Plus (empty)

import Test.Assert (assert)
import Test.Spec (describe, it)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Assert.Simple
import Test.Support (vliftEff, runEitherEff)

import Docopt.Spec.Solver (solve)
import qualified Docopt.Spec.Parser.Usage as U
import qualified Docopt.Spec.Parser.Desc as D
import Docopt.Spec.Parser.Scanner (scan)
import Docopt.Spec.Parser.Lexer (lex)
import Text.Wrap (dedent)

solverSpec =
  describe "solver" do
    it "should have tests..." do
        pure unit
