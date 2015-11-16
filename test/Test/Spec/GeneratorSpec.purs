module Test.Spec.GeneratorSpec (generatorSpec) where

import Prelude
import Debug.Trace
import Control.Monad.Aff (liftEff')
import Control.Monad.State (State(), evalState)
import Data.Maybe (Maybe(..))
import Data.List (List(..), toList)

import Docopt
import Docopt.Parser.Usage (Usage(..))
import qualified Docopt.Parser.Usage as Usage
import qualified Docopt.Parser.Options as Options
import qualified Docopt.Textwrap as Textwrap
import qualified Docopt.Parser.Lexer as Lexer
import qualified Docopt.Parser.Scanner as Scanner
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
    -> (Maybe String)
    -> (Maybe Default)
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

generatorSpec =
  describe "options parser" do
    it "should have some tests..." do
      pure unit
