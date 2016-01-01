module Test.Spec.DescParserSpec (descParserSpec) where

import Prelude
import Control.Monad.Aff (liftEff')
import Debug.Trace

import Docopt
import qualified Docopt.Spec.Parser.Usage as Usage
import qualified Docopt.Spec.Parser.Desc as Desc
import qualified Docopt.Spec.Parser.Lexer as Lexer
import qualified Docopt.Spec.Parser.Scanner as Scanner
import Docopt.Spec.Parser.Base (debug)
import Text.Wrap (dedent)

import Test.Assert (assert)
import Test.Spec (describe, it)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Assert.Simple
import Test.Support (vliftEff, runMaybeEff, runEitherEff)

descParserSpec =
  describe "options parser" do
    it "should have some tests..." do
      vliftEff do
        options <- runEitherEff do
          toks <- Lexer.lex $ dedent
            """
            -f, --foo
              this is som much text about -foo, and -f, and --foo
              --bar, baz
              [default: 100.0]
            -f       x [default: 200.0]
            --foo    -a [default: 300.0]
            -b
            --foo<x> / [default: 400.0]
            """
          traceShowA toks
          Desc.parse toks
        traceShowA options

        -- assertEqual 1 (length usage)
        -- (Usage.Usage _ u) <- runMaybeEff $ usage !! 0
        -- g <- runMaybeEff $ u !! 0
        -- flip assertEqual g (Cons (Usage.Command "bar") Nil)

