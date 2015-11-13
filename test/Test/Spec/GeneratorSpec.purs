module Test.Spec.GeneratorSpec (generatorSpec) where

import Prelude
import Control.Monad.Aff (liftEff')
import Debug.Trace

import Docopt
import Docopt.Parser.Usage (Usage(..))
import qualified Docopt.Parser.Usage as Usage
import qualified Docopt.Parser.Options as Options
import qualified Docopt.Textwrap as Textwrap
import qualified Docopt.Parser.Lexer as Lexer
import qualified Docopt.Parser.Scanner as Scanner
import Docopt.Parser.Base (debug)

import Test.Assert (assert)
import Test.Spec (describe, it)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Assert.Simple
import Test.Support (vliftEff, runMaybeEff, runEitherEff)

import Control.Monad.State (State(), evalState)

generatorSpec =
  describe "options parser" do
    it "should have some tests..." do
      pure unit
