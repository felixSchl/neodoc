module Test.Spec.GeneratorSpec (generatorSpec) where

import Prelude
import Debug.Trace
import Control.Monad.Aff (liftEff')
import Control.Monad.State (State(), evalState)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.List (List(..), toList)

import Docopt
import Docopt.Parser.Usage (Usage(..))
import qualified Docopt.Parser.Usage as Usage
import qualified Docopt.Parser.Options as Options
import qualified Docopt.Textwrap as Textwrap
import qualified Docopt.Parser.Lexer as Lexer
import qualified Docopt.Parser.Scanner as Scanner
import Docopt.Generate (lexArgv, mkBranchParser, runCliParser)
import Docopt.Parser.Base (debug)

import Test.Assert (assert)
import Test.Spec (describe, it)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Assert.Simple
import Test.Support (vliftEff, runMaybeEff, runEitherEff)

-- short hand to create a Command
co :: String -> Argument
co = Command

-- short hand to create a Positional argument
po :: String -> Boolean -> Argument
po = Positional

-- short hand to create an Option argument
opt :: (Maybe Flag)
    -> (Maybe Name)
    -> (Maybe OptionArgument)
    -> IsRepeatable
    -> Argument
opt = Option

-- short hand to create a group
gr :: Boolean -> (Array (Array Argument)) -> IsRepeatable -> Argument
gr b xs = Group b (toList $ br <$> xs)

-- short hand to create a optional group
gro :: (Array (Array Argument)) -> IsRepeatable -> Argument
gro = gr true

-- short hand to create a required group
grr :: (Array (Array Argument)) -> IsRepeatable -> Argument
grr = gr false

-- short hand to create a whole branch
br :: (Array Argument) -> Branch
br xs = Branch (toList xs)

generatorSpec = describe "generator" do

  -- XXX: Move this
  describe "cli lexer" do
    it "should lex cli input..." do
      vliftEff do
        res <- runEitherEff do
          lexArgv (toList [
            "-foo"
          , "-bazQUX"
          , "-baz=QUX"
          , "bar"
          , "--foobar=QUX"
          ])
        traceShowA res

  describe "generator" do
    it "should have some tests..." do
      let branch = br [
            co "foo"
          , opt (Just 'f') (Just "foo") Nothing true
          ]
          parser = mkBranchParser branch
      vliftEff do
        res <- runEitherEff do
          toks <- lexArgv (toList [
            "foo", "-fobar"
          ])
          flip runCliParser parser toks
        traceShowA res
        pure unit
