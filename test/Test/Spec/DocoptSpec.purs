module Test.Spec.DocoptSpec (docoptSpec) where

import Prelude
import Debug.Trace
import Data.List (List(..), toList, concat, last, init)
import qualified Text.Parsing.Parser as P
import Data.Traversable (traverse)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))

import Test.Assert (assert)
import Test.Spec (describe, it)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Assert.Simple

import Test.Support (vliftEff, runEitherEff)
import qualified Test.Support.Usage as U
import qualified Test.Support.Docopt as D
import qualified Test.Support.Desc as Desc

import Docopt
import Docopt.Spec.Parser.Scanner (scan)
import qualified Docopt.Spec.Parser.Usage as Usage
import qualified Docopt.Spec.Parser.Desc  as Desc
import qualified Docopt.Spec.Solver       as Solver
import qualified Docopt.Gen               as Gen

import Text.Wrap (dedent)

toScanErr :: forall a. Either P.ParseError a -> Either DocoptError a
toScanErr  = lmap DocoptScanError

toParseErr :: forall a. Either P.ParseError a -> Either DocoptError a
toParseErr = lmap DocoptParseError

toSolveErr :: forall a. Either SolveError a -> Either DocoptError a
toSolveErr = lmap DocoptSolveError

docoptSpec =
  describe "docopt" do
    it "..." do
      vliftEff do

        traceShowA $ init (toList ['a'] :: List Char)

        runEitherEff do
          output <- runDocopt
            """
            Usage: foo -o FILE FILE...

            Options:
            -o, --output=FILE The file to write to
            """
            [ "-o", "~/foo/bar", "x", "y" ]
          traceShowA output
