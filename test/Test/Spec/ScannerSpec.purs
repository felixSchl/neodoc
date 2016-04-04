module Test.Spec.ScannerSpec (scannerSpec) where

import Prelude
import Debug.Trace
import Control.Monad.Aff
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (liftEff)
import Data.List (List(..), length, (!!), take, toList)
import Data.Either (Either(..), isRight, isLeft, either)
import Data.Either.Unsafe (fromLeft, fromRight)
import Data.Maybe.Unsafe (fromJust)
import Data.Maybe (Maybe(..))

import Language.Docopt
import qualified Language.Docopt.Scanner as Scanner
import Text.Wrap (dedent)

import Test.Assert (assert)
import Test.Spec (describe, it)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Assert.Simple
import Test.Support (vliftEff)

scannerSpec = \_ ->
  describe "The scanner" do
    it "should scan sections" do
      let docopt = fromRight $ Scanner.scan $
          dedent
            """
            Usage: foo
            Options: bar
            Advanced Options: qux
            """
      vliftEff do
                              -- "Usage: foo\n"
        assert $ docopt.usage == "       foo\n"
        assert $ length docopt.options == 2
                                                -- "Options: bar\n"
        assert $ fromJust (docopt.options !! 0) == "         bar\n"
                                                -- "Advanced Options: qux\n"
        assert $ fromJust (docopt.options !! 1) == "                  qux\n"

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
                               -- Usage:
        assert $ docopt.usage == "      \n  foo\n"
        assert $ length docopt.options == 2
        assert $ fromJust (docopt.options !! 0)
            --  Options:
            == "        \n  bar\n"
        assert $ fromJust (docopt.options !! 1)
            --  Advanced Options:
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
