module Test.Spec.ScannerSpec (scannerSpec) where

import Prelude
import Debug.Trace
import Control.Monad.Aff
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (liftEff)
import Data.List (List(..), length, (!!), take)
import Data.Either (Either(..), isRight, isLeft, either)
import Data.Either (fromRight)
import Data.Maybe (Maybe(..), fromJust)
import Partial.Unsafe (unsafePartial)

import Language.Docopt
import Language.Docopt.Scanner as Scanner
import Text.Wrap (dedent)

import Test.Assert (assert)
import Test.Spec (describe, it)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Support (vliftEff)

scannerSpec = \_ ->
  describe "The scanner" do
    it "should scan sections" do
      let docopt = unsafePartial $ fromRight $ Scanner.scan $
          dedent
            """
            Usage: foo
            Options: bar
            Advanced Options: qux
            """
      vliftEff do
        assert $ docopt.usage == "       foo\n"
        assert $ length docopt.options == 2
        assert $ unsafePartial $ fromJust (docopt.options !! 0)
          == "         bar\n"
        assert $ unsafePartial $ fromJust (docopt.options !! 1)
          == "                  qux\n"

    it "should scan sections with new line after colon" do
      let docopt = unsafePartial $ fromRight $ Scanner.scan $
          dedent
            """
            Usage:
              foo
            Options:
              bar
            Advanced Options:
              qux
            """
      vliftEff do
        assert $ docopt.usage == "      \n  foo\n"
        assert $ length docopt.options == 2
        assert $ unsafePartial $ fromJust (docopt.options !! 0)
            == "        \n  bar\n"
        assert $ unsafePartial $ fromJust (docopt.options !! 1)
            == "                 \n  qux\n"

    it "should fail w/o a usage section" do
      let result = Scanner.scan $
          dedent
            """
            Options: bar
            """
      vliftEff do
        assert $ isLeft result

    it "should fail w/o any sections" do
      let result = Scanner.scan $
          dedent
            """
            """
      vliftEff do
        assert $ isLeft result
