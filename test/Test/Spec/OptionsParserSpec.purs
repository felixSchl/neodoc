module Test.Spec.OptionsParserSpec (optionsParserSpec) where

import Prelude
import Control.Monad.Aff (liftEff')
import Debug.Trace

import Docopt
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

optionsParserSpec =
  describe "options parser" do
    it "should have some tests..." do
      vliftEff do
        options <- runEitherEff do
          toks <- Lexer.lex $ Textwrap.dedent
            """
            -f, --foo
            """
          Options.parse toks
        traceShowA options

        -- assertEqual 1 (length usage)
        -- (Usage.Usage _ u) <- runMaybeEff $ usage !! 0
        -- g <- runMaybeEff $ u !! 0
        -- flip assertEqual g (Cons (Usage.Command "bar") Nil)

