module Test.Spec.DocoptSpec (docoptSpec) where

import Prelude
import Debug.Trace
import Data.List (List(..), toList, concat, last, init)
import qualified Text.Parsing.Parser as P
import Data.Traversable (traverse)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Text.Wrap (dedent)

import Test.Assert (assert)
import Test.Spec (describe, it)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Assert.Simple

import Test.Support (vliftEff, runEitherEff, prettyPrintMap)
import qualified Test.Support.Usage  as U
import qualified Test.Support.Docopt as D
import qualified Test.Support.Desc   as Desc

import Language.Docopt (runDocopt, prettyPrintValue)

docoptSpec = \_ ->
  describe "Docopt" do
    it "..." do
      vliftEff do
        runEitherEff do
          output <- runDocopt
            """
            Usage:
              foo push -h=<host[:port]> -o <file> FILE... -- ARGS

            Options:
              -o, --output=ILE
                The file to write to

              -h, --host=<host[:port]>
                The host to connect to
                [default: http://localhost:3000]
            """
            [ "push"
            , "-o", "~/foo/bar"
            , "-h", "http://localhost:5000"
            , "x", "y"
            , "--", "0", "1", "3"
            ]
          traceA (prettyPrintMap output show prettyPrintValue)
          return unit
