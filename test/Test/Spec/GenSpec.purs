module Test.Spec.GenSpec (
  genSpec
, genTransSpec
)
where

import Prelude
import Data.Tuple (Tuple(..))
import Debug.Trace
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (EXCEPTION())
import Control.Monad.State (State(), evalState)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..), either)
import Data.List (List(..), toList, length, fromList, singleton)
import Data.Map (Map(..))
import qualified Data.Map as Map
import qualified Data.Array as A
import Data.Foldable (for_, intercalate)
import Control.Monad.Eff.Exception (error, throwException, catchException
                                   , message)
import qualified Text.Parsing.Parser as P

import Docopt
import Docopt.Spec.Parser.Usage (Usage(..))
import qualified Docopt.Spec.Parser.Usage as Usage
import qualified Docopt.Spec.Parser.Lexer as Lexer
import qualified Docopt.Spec.Parser.Scanner as Scanner
import Docopt.Gen (genParser, runParser)
import Docopt.Gen.Lexer (lex)
import Docopt.Spec.Parser.Base (debug)

import Test.Assert (assert)
import Test.Spec (describe, it, Spec())
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Assert.Simple
import Test.Support (vliftEff, runMaybeEff, runEitherEff)

-- short hand to create a Command
co :: String -> Argument
co = Command

-- short hand to create a Positional argument
pos :: String -> Boolean -> Argument
pos = Positional

-- short hand to create a end-of-arguments marker
eoa :: Argument
eoa = EOA

-- short hand to create an Option argument
opt' :: Flag
      -> Name
      -> (Maybe OptionArgument)
      -> IsRepeatable
      -> Argument
opt' f n = Option (Just f) (Just n)

opt_ :: Flag
      -> Name
      -> Argument
opt_ f n = opt' f n Nothing false

optR_ :: Flag
      -> Name
      -> Argument
optR_ f n = opt' f n Nothing true

opt :: Flag
    -> Name
    -> OptionArgument
    -> Argument
opt f n a = opt' f n (Just a) false

optR :: Flag
     -> Name
     -> OptionArgument
     -> Argument
optR f n a = opt' f n (Just a) true

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
gr :: Boolean -> IsRepeatable -> (Array (Array Argument)) -> Argument
gr b r xs = Group b (toList $ br <$> xs) r

-- short hand to create a optional group
gro :: IsRepeatable -> (Array (Array Argument)) -> Argument
gro = gr true

-- short hand to create a required group
grr :: IsRepeatable -> (Array (Array Argument)) -> Argument
grr = gr false

-- short hand to create a whole branch
br :: (Array Argument) -> Branch
br xs = Branch (toList xs)

oa :: String -> Value -> OptionArgument
oa n v = OptionArgument n (Just v)

oa_ :: String -> OptionArgument
oa_ n = OptionArgument n Nothing

type Input    = Array String
type Output   = Map Argument Value
data Test = Test (Array Argument) (Array Case)
data Case = Case Input (Either String Output)

test :: Array Argument -> Array Case -> Test
test = Test

pass :: Input -> (Array (Tuple Argument Value)) -> Case
pass i o = Case i (Right $ Map.fromList $ toList o)

fail :: Input -> String -> Case
fail i e = Case i (Left e)

(#=) = Tuple
infixr 0 #=

genSpec = \_ -> describe "The generator" do

  -- Some options that will be used for these tests
  let cmd_foo          = co    "foo"
      opt_f_foo_FOZ__r = optR  'f' "foo" (oa_ "FOZ")
      opt_q_qux___r    = optR_ 'q' "qux"
      opt_b_baz        = opt   'b' "baz"   (oa "BAZ" $ StringValue "ax")
      opt_o_out        = opt_  'o' "out"
      opt_i_input      = opt_  'i' "input"
      pos_arg_r        = pos    "qux" true
      cmd_baz          = co     "baz"

  let testCases = [

      test [ pos_arg_r ]
        [ pass
            [ "a", "b", "c" ]
            [ pos_arg_r #= ArrayValue [
                            StringValue "a"
                          , StringValue "b"
                          , StringValue "c"
                          ]
            ]
        , fail [ "--foo", "baz" ]
            "Expected positional argument: \"qux...\""
        , fail
            [ "a", "--foo", "-f=10" ]
            "Trailing input: --foo, -f=10"
        ]

    , test [ pos_arg_r, eoa ]
        [ pass
            [ "a", "b", "c", "--" ]
            [ pos_arg_r #= ArrayValue [
                              StringValue "a"
                            , StringValue "b"
                            , StringValue "c"
                            ]
            , eoa #= ArrayValue []
            ]
        , pass
            [ "a", "b", "c", "--", "--", "--" ]
            [ pos_arg_r #= ArrayValue [
                              StringValue "a"
                            , StringValue "b"
                            , StringValue "c"
                            ]
            , eoa #= ArrayValue [
                        StringValue "--"
                      , StringValue "--"
                      ]
            ]
        ]

    , test
        [ grr false [] ]
        [ pass [] [] ]

    , test
        [ gro false [] ]
        [ pass [] [] ]

    , test
        [ grr false [[
            opt 'i' "input" (oa_ "FILE")
          ]]
        ]
        [ fail [] "Missing required options: (-i, --input=FILE)"
        , pass
            [ "-i", "bar" ]
            [ opt 'i' "input" (oa_ "FILE") #= (StringValue "bar") ]
        ]

    , test
        [ grr false [[
            opt 'i' "input" (oa_ "FILE")
          ]]
        , opt 'o' "output" (oa_ "FILE")
        ]
        [ fail [] "Missing required options: -o, --output=FILE, (-i, --input=FILE)"
        , fail [ "-i", "bar" ] "Missing required options: -o, --output=FILE"
        , pass [ "-i", "bar", "-o", "bar" ]
            [ opt 'i' "input"  (oa_ "FILE") #= (StringValue "bar")
            , opt 'o' "output" (oa_ "FILE") #= (StringValue "bar") ]
          -- group should be interchangable if it's only of options:
        , pass [ "-o", "bar", "-i", "bar" ]
            [ opt 'i' "input"  (oa_ "FILE") #= (StringValue "bar")
            , opt 'o' "output" (oa_ "FILE") #= (StringValue "bar") ]
        ]

    , test
        [ grr false [[
            grr false [[
              opt 'i' "input" (oa_ "FILE")
            ]]
          , opt 'r' "redirect" (oa_ "FILE")
          ]]
        , opt 'o' "output" (oa_ "FILE")
        ]
        [ fail []
            "Missing required options: -o, --output=FILE, ((-i, --input=FILE) -r, --redirect=FILE)"
        , fail [ "-i", "bar", "-r", "bar" ]
            "Missing required options: -o, --output=FILE"
        , pass [ "-i", "bar", "-r", "bar", "-o", "bar" ]
            [ opt 'i' "input"  (oa_ "FILE")   #= (StringValue "bar")
            , opt 'r' "redirect" (oa_ "FILE") #= (StringValue "bar")
            , opt 'o' "output" (oa_ "FILE")   #= (StringValue "bar") ]
          -- group should be interchangable if it's only of options:
        , pass [ "-o", "bar", "-r", "bar", "-i", "bar" ]
            [ opt 'i' "input"  (oa_ "FILE")   #= (StringValue "bar")
            , opt 'r' "redirect" (oa_ "FILE") #= (StringValue "bar")
            , opt 'o' "output" (oa_ "FILE")   #= (StringValue "bar") ]
        ]

    , test
        [ grr false [[
            opt 'i' "input" (oa_ "FILE")
          , pos "env" false
          ]]
        , opt 'o' "output" (oa_ "FILE")
        ]
        [ fail [] "Missing required options: -i, --input=FILE"
          -- XXX: Would be cool to show the reason the group did not parse!
        , fail [ "-i", "bar" ] "Expected positional argument: \"env\""
        , pass [ "-i", "bar", "x", "-o", "bar" ]
            [ opt 'i' "input"  (oa_ "FILE") #= (StringValue "bar")
            , pos "env" false               #= (StringValue "x")
            , opt 'o' "output" (oa_ "FILE") #= (StringValue "bar") ]
          -- group should NOT be interchangable if it contains non-options:
        , fail [ "-o", "bar", "x", "-i", "bar" ]
            "Missing required options: -i, --input=FILE"
        ]

    , test
        [ cmd_foo
        , opt_o_out
        , opt_q_qux___r
        , opt_b_baz
        , opt_i_input
        , opt_f_foo_FOZ__r
        , cmd_baz
        ]

        [ pass
            [ "foo" , "--out", "--input", "--qux", "--foo=ox", "baz" ]
            [ cmd_foo          #= BoolValue true
            , opt_o_out        #= BoolValue true
            , opt_i_input      #= BoolValue true
            , opt_q_qux___r    #= ArrayValue [ BoolValue true ]
            , opt_f_foo_FOZ__r #= ArrayValue [ StringValue "ox" ]
            , cmd_baz          #= BoolValue true
            -- should have added default value that was not provided above:
            , opt_b_baz        #= StringValue "ax"
            ]

        , pass
            [ "foo" , "--out", "-qqq", "--foo=ox", "--baz=ax", "--input", "baz" ]
            [ cmd_foo       #= BoolValue true
            , opt_o_out     #= BoolValue true
            , opt_q_qux___r #= ArrayValue [
                                  BoolValue true
                                , BoolValue true
                                , BoolValue true
                                ]
            , opt_f_foo_FOZ__r #= ArrayValue [StringValue "ox"]
            , opt_b_baz        #= StringValue "ax"
            , opt_i_input      #= BoolValue true
            , cmd_baz          #= BoolValue true
            ]

        , pass
            [ "foo", "-q", "-o", "--qux", "-i", "--baz=ax", "-f=ox", "baz" ]
            [ cmd_foo          #= BoolValue true
            , opt_q_qux___r    #= ArrayValue [
                                        BoolValue true
                                      , BoolValue true
                                     ]
            , opt_o_out        #= BoolValue true
            , opt_i_input      #= BoolValue true
            , opt_b_baz        #= StringValue "ax"
            , opt_f_foo_FOZ__r #= ArrayValue [StringValue "ox"]
            , cmd_baz          #= BoolValue true
            ]

        , pass
            [ "foo", "--baz=ax", "-o", "-f=ox", "-i", "baz" ]
            [ cmd_foo          #= BoolValue true
            , opt_b_baz        #= StringValue "ax"
            , opt_o_out        #= BoolValue true
            , opt_f_foo_FOZ__r #= ArrayValue [StringValue "ox"]
            , opt_i_input      #= BoolValue true
            , cmd_baz          #= BoolValue true
            ]

        , pass
            [ "foo", "-o", "-i", "-bax", "baz" ]
            [ cmd_foo     #= BoolValue true
            , opt_o_out   #= BoolValue true
            , opt_i_input #= BoolValue true
            , opt_b_baz   #= StringValue "ax"
            , cmd_baz     #= BoolValue true
            -- should have added default value that was not provided above:
            , opt_b_baz  #= StringValue "ax"
            ]

        , fail
            [ "foo" ]
            -- TODO: Create a more sophisticated way to test this
            $ "Missing required options: "
                ++ "-o, --out, "
                ++ "-i, --input"
        , fail
            [ "foo", "-o", "-i", "-bax" ]
            -- TODO: Create a more sophisticated way to test this
            "Expected command: \"baz\""
        ]

    , test
        [ gro false [[ cmd_foo ]] ]
        [ fail [ "goo" ] "Trailing input: \"goo\"" ]
    , test
        [ grr false [[ cmd_foo ]] ]
        [ fail [ "goo" ] "Expected command: \"foo\"" ]
  ]

  for_ testCases \(Test branch kases) -> do
    describe (prettyPrintBranch $ br branch) do
      for_ kases \(Case input expected) ->
            let msg = either
                  (\e -> "Should fail with \"" ++ e ++ "\"")
                  (\e -> prettyPrintOutput e)
                  expected
            in it (intercalate " " input ++ " -> " ++ msg) do
                  vliftEff do
                    validate branch input expected

    where

      prettyPrintOutput :: Output -> String
      prettyPrintOutput expected = ("\n\t" ++) $ intercalate "\n\t" $
                      (Map.toList expected) <#> \(Tuple arg val) ->
                        prettyPrintArg arg ++ ": " ++ prettyPrintValue val

      validate :: forall eff. Array Argument
                            -> Input
                            -> Either String Output
                            -> Eff (err :: EXCEPTION | eff) Unit
      validate args argv expected = do
        let result = do
              runParser
                (toList argv)
                (genParser $ singleton $ Application $ singleton $ br args)
        case result of
          Left (e@(P.ParseError { message: msg })) ->
            either
              (\e' ->
                if (msg /= e')
                  then throwException $ error $
                    "Unexpected error:\n" ++ msg
                  else return unit)
              (const $ throwException $ error $ show e)
              expected
          Right r -> do
            either
              (throwException <<< error <<< show)
              (\r' ->
                if (r /= r')
                  then throwException $ error $
                    "Unexpected output:\n"
                      ++ prettyPrintOutput r
                  else return unit)
              expected

genTransSpec = \_ ->
  describe "The output transformer" do
    it "..." do
      pure unit

