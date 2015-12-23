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
import Data.Foldable (for_, intercalate)
import Control.Monad.Eff.Exception (error, throwException)

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

type Input    = Array String
type Output   = Array (Tuple Argument Value)
data Test = Test (Array Argument) (Array Case)
data Case = Case Input Output

test :: Array Argument -> Array Case -> Test
test = Test

kase :: Input -> Output -> Case
kase = Case

generatorSpec = describe "The generator" do

  -- Some options that will be used for these tests
  let cmd_foo          = co "foo"
      opt_f_foo_FOZ__r = opt 'f' "foo" (oa_ "FOZ") true
      opt_q_qux___r    = opt 'q' "qux" Nothing true
      opt_b_baz___r    = opt 'b' "baz" Nothing true
      opt_o_out        = opt 'o' "out" Nothing false

  let testCases = [
      test
        [ cmd_foo, opt_o_out, opt_q_qux___r, opt_b_baz___r, opt_f_foo_FOZ__r ]
        [ kase
            [ "foo" , "--out", "-qqq", "--foo=ox", "--baz" ]
            [ Tuple cmd_foo          (BoolValue true)
            , Tuple opt_o_out        (BoolValue true)
            , Tuple opt_q_qux___r    (BoolValue true)
            , Tuple opt_q_qux___r    (BoolValue true)
            , Tuple opt_q_qux___r    (BoolValue true)
            , Tuple opt_f_foo_FOZ__r (StringValue "ox")
            , Tuple opt_b_baz___r    (BoolValue true)
            ]
        , kase
            [ "foo", "-q", "-o", "--qux", "--baz", "-f=ox" ]
            [ Tuple cmd_foo          (BoolValue true)
            , Tuple opt_q_qux___r    (BoolValue true)
            , Tuple opt_o_out        (BoolValue true)
            , Tuple opt_q_qux___r    (BoolValue true)
            , Tuple opt_b_baz___r    (BoolValue true)
            , Tuple opt_f_foo_FOZ__r (StringValue "ox")
            ]
        , kase
            [ "foo", "--baz", "-o", "-f=ox" ]
            [ Tuple cmd_foo          (BoolValue true)
            , Tuple opt_b_baz___r    (BoolValue true)
            , Tuple opt_o_out        (BoolValue true)
            , Tuple opt_f_foo_FOZ__r (StringValue "ox")
            ]
        , kase
            [ "foo", "-o" ]
            [ Tuple cmd_foo          (BoolValue true)
            , Tuple opt_o_out        (BoolValue true)
            ]
        ]
  ]

  for_ testCases \(Test branch kases) -> do
    describe (prettyPrintBranch $ br branch) do
      for_ kases \(Case input expected) ->
        it (intercalate " " input ++ " -> " ++ prettyPrintExpected expected) do
          vliftEff do
            validate branch input expected

    where

      prettyPrintExpected :: Output -> String
      prettyPrintExpected expected = ("\n\t" ++) $ intercalate "\n\t" $
                      flip map expected \(Tuple arg val) ->
                        prettyPrintArg arg ++ ": " ++ prettyPrintValue val

      validate :: forall eff. Array Argument
                            -> Input
                            -> Output
                            -> Eff (err :: EXCEPTION | eff) Unit
      validate args argv expected = do
        result <- fromList <$> runEitherEff do
          toks <- lexArgv (toList argv)
          runCliParser toks $ mkBranchParser $ br args

        if (expected /= result)
          then throwException $ error $
            "Unexpected output:\n" ++ prettyPrintExpected result
          else return unit
