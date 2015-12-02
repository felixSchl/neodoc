module Test.Spec.GeneratorSpec (generatorSpec) where

import Prelude
import Data.Tuple (Tuple(..))
import Debug.Trace
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (EXCEPTION())
import Control.Monad.Aff (liftEff')
import Control.Monad.State (State(), evalState)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.List (List(..), toList, length, fromList)
import qualified Data.Array as A

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
opt :: Flag
      -> Name
      -> (Maybe OptionArgument)
      -> IsRepeatable
      -> Argument
opt f name = Option (Just f) (Just name)

sopt :: Flag
      -> (Maybe OptionArgument)
      -> IsRepeatable
      -> Argument
sopt f = Option (Just f) Nothing

lopt :: Name
      -> (Maybe OptionArgument)
      -> IsRepeatable
      -> Argument
lopt name = Option Nothing (Just name)

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

oa :: String -> Value -> Maybe OptionArgument
oa n v = Just $ OptionArgument n (Just v)

oa_ :: String -> Maybe OptionArgument
oa_ n = Just $ OptionArgument n Nothing

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

      let cmdfoo = co "foo"
          optfoo = opt 'f' "foo" (oa_ "FOZ") true
          optbar = opt 'b' "bar" (oa  "BAZ" (StringValue "defbaz")) true
      let branch = [ cmdfoo, optfoo, optbar ]
      let input =
            [ "foo"
            , "-f", "fox"
            , "--bar", "baxxer"
            , "-b", "bax"
            , "-b" ]
      let expected =
        [ Tuple cmdfoo (BoolValue true)
        , Tuple optfoo (StringValue "fox")
        , Tuple optbar (StringValue "baxxer")
        , Tuple optbar (StringValue "bax")
        , Tuple optbar (StringValue "defbaz") ]

      vliftEff do
        validate branch input expected

    where
      validate :: forall eff. Array Argument
                            -> Array String
                            -> Array (Tuple Argument Value)
                            -> Eff (err :: EXCEPTION | eff) Unit
      validate args argv expected = do
        res <- runEitherEff do
          toks <- lexArgv (toList argv)
          flip runCliParser (mkBranchParser (br args)) toks

        assertEqual (fromList res) (expected)
