module Test.Spec.SolverSpec (solverSpec) where

import Prelude
import Debug.Trace
import Control.Bind((=<<))
import Data.List (List(..))

import Test.Assert (assert)
import Test.Spec (describe, it)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Assert.Simple
import Test.Support (vliftEff, runEitherEff)

import Docopt.Solver (solve)
import qualified Docopt.Parser.Usage as U
import qualified Docopt.Parser.Options as O
import Docopt.Parser.Scanner (scan)
import Docopt.Parser.Lexer (lex)
import Docopt.Textwrap (dedent)

solverSpec =
  describe "solver" do
    it "should have tests..." do
      vliftEff do
        { usage:   usage
        , options: options } <- scan'
          """
          Usage:
            foo (blah|bar)
          Options:
            -b, --bar
          """
        usages <- runEitherEff (U.parse =<< lex usage)
        let solved = solve usages Nil
        traceShowA solved
        pure unit

 where
  scan' = runEitherEff <<< scan <<< dedent
