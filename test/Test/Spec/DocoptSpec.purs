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

import Language.Docopt
import Language.Docopt.Parser.Scanner (scan)
import qualified Language.Docopt.Parser.Usage as Usage
import qualified Language.Docopt.Parser.Desc  as Desc
import qualified Language.Docopt.Solver       as Solver
import qualified Language.Docopt.ParserGen         as Gen

import Text.Wrap (dedent)

toScanErr :: forall a. Either P.ParseError a -> Either DocoptError a
toScanErr  = lmap DocoptScanError

toParseErr :: forall a. Either P.ParseError a -> Either DocoptError a
toParseErr = lmap DocoptParseError

toSolveErr :: forall a. Either SolveError a -> Either DocoptError a
toSolveErr = lmap DocoptSolveError

docoptSpec = \_ ->
  describe "Docopt" do
    it "..." do
      vliftEff do
        runEitherEff do
          output <- runDocopt
            """
            Usage:
              foo -h=<host[:port]> -o FILE FILE... -- ARGS

            Options:
              -o, --output=FILE
                The file to write to

              -h, --host=<host[:port]>
                The host to connect to
                [default: http://localhost:3000]
            """
            [ "-o", "~/foo/bar"
            , "-h", "http://localhost:5000"
            , "x", "y"
            , "--", "0"," 1", "3"
            ]
          return unit
