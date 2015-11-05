module Test.Spec.OptionsParserSpec (optionsParserSpec) where

import Prelude
import Control.Monad.Aff (liftEff')
import Debug.Trace

import Test.Assert (assert)
import Test.Spec (describe, it)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Assert.Simple
import Test.Support (vliftEff, runMaybeEff, runEitherEff)

optionsParserSpec =
  describe "options parser" do
    it "should have some tests..." do
      vliftEff do
        traceShowA "here"
        assert false
        pure unit
