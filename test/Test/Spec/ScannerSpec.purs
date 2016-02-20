module Test.Spec.ScannerSpec (scannerSpec) where

import Prelude
import Control.Monad.Aff
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (liftEff)
import Data.List (List(..), length, (!!), take, toList)
import Data.Either (Either(..), isRight, isLeft, either)
import Data.Either.Unsafe (fromLeft, fromRight)
import Data.Maybe.Unsafe (fromJust)
import Data.Maybe (Maybe(..))

import Docopt
import qualified Docopt.Spec.Parser.Scanner as Scanner
import Text.Wrap (dedent)

import Test.Assert (assert)
import Test.Spec (describe, it)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Assert.Simple
import Test.Support (vliftEff)

scannerSpec = do
  describe "scanner" do
    it "should scan sections" do
      let docopt = fromRight $ Scanner.scan $
          dedent
            """
            Usage: foo
            Options: bar
            Advanced Options: qux
            """
      vliftEff do
        assert $ docopt.usage == "foo\n"
        assert $ length docopt.options == 2
        assert $ fromJust (docopt.options !! 0) == " bar\n"
        assert $ fromJust (docopt.options !! 1) == " qux\n"

    it "should scan sections with new line after colon" do
      let docopt = fromRight $ Scanner.scan $
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
        assert $ docopt.usage == "foo\n"
        assert $ length docopt.options == 2
        assert $ fromJust (docopt.options !! 0) == "\n  bar\n"
        assert $ fromJust (docopt.options !! 1) == "\n  qux\n"

    it "should fail w/o a usage section" do
      let result = Scanner.scan $
          dedent
            """
            Options: bar
            """
      vliftEff do
        assert $ isLeft result

    it "should fail multiple usage sections" do
      let result = Scanner.scan $
          dedent
            """
            Usage: bar
            Options: foo
            Usage: qux
            """
      vliftEff do
        assert $ isLeft result

    it "should fail if usage section is not the first section" do
      let result = Scanner.scan $
          dedent
            """
            Options: foo
            Usage: qux
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
