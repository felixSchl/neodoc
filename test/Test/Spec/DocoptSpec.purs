module Test.Spec.DocoptSpec (docoptSpec) where

import Prelude
import Debug.Trace
import Data.List (List(..), toList, concat, last, init)
import Text.Parsing.Parser as P
import Data.Traversable (traverse)
import Data.Bifunctor (lmap)
import Data.StrMap as StrMap
import Data.StrMap (StrMap())
import Data.Tuple (Tuple(..))
import Data.Either (Either(..))
import Text.Wrap (dedent)
import Node.Process (getEnv)

import Test.Assert (assert)
import Test.Spec (describe, it)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Assert.Simple

import Test.Support (vliftEff, runEitherEff, prettyPrintMap)
import Test.Support.Usage  as U
import Test.Support.Docopt as D
import Test.Support.Desc   as Desc

import Language.Docopt (runDocopt)
import Language.Docopt.Value as D

docoptSpec = \_ ->
  describe "Docopt" do
    it "..." do
      vliftEff do
        let env = StrMap.fromFoldable [
                    Tuple "FOO_OUTPUT" "BAR"
                  ]
        runEitherEff do
          output <- runDocopt env
            """
            Usage:
              foo push [options...]

            Options:
              -o, --output=ILE
                The file to write to
                [env: FOO_OUTPUT]

              -h, --host=<host[:port]>
                The host to connect to
                [default: http://localhost:3000]

              -f, --force
                Force the operation.

            """
            [ "push"
            -- , "-o", "~/foo/bar" (provide from env)
            , "-hhttp://localhost:5000"
            , "x"
            , "x", "y"
            , "--", "0", "1", "3"
            ]
          traceA (prettyPrintMap output show D.prettyPrintValue)
          return unit
