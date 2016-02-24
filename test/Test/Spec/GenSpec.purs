module Test.Spec.GenSpec (genSpec) where

import Prelude
import Debug.Trace
import Data.Tuple (Tuple(..))
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (EXCEPTION())
import Data.Maybe (Maybe(..))
import Data.Either (Either(..), either)
import Data.List (List(..), toList, length, fromList, singleton)
import Data.Map (Map(..))
import qualified Data.Map as Map
import qualified Data.Array as A
import Data.Foldable (for_, intercalate)
import Control.Monad.Eff.Exception (error, throwException)
import qualified Text.Parsing.Parser as P

import Docopt
import Docopt.Gen (genParser, runParser)
import qualified Docopt.Gen.Trans as Trans

import Test.Assert (assert)
import Test.Spec (describe, it, Spec())
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Assert.Simple
import Test.Support (vliftEff, runMaybeEff, runEitherEff)
import Test.Support.Docopt

data Test = Test (Array Argument) (Array Case)
data Case = Case (Array String) (Either String (Map Argument Value))

test :: Array Argument -> Array Case -> Test
test = Test

pass :: Array String -> (Array (Tuple Argument Value)) -> Case
pass i o = Case i (Right $ Map.fromList $ toList o)

fail :: Array String -> String -> Case
fail i e = Case i (Left e)

(:>) = Tuple
infixr 0 :>

genSpec = \_ -> describe "The generator" do

  -- Some options that will be used for these tests
  let cmd_foo          = co    "foo"
      opt_f_foo_FOZ__r = optR  'f' "foo" (oa_ "FOZ")
      opt_q_qux___r    = optR_ 'q' "qux"
      opt_b_baz        = opt   'b' "baz"   (oa "BAZ" $ str "ax")
      opt_o_out        = opt_  'o' "out"
      opt_i_input      = opt_  'i' "input"
      pos_arg_r        = po     "qux" true
      cmd_baz          = co     "baz"

  let testCases = [

      test [ pos_arg_r ]
        [ pass
            [ "a", "b", "c" ]
            [ pos_arg_r :> array [ str "a" , str "b" , str "c" ] ]
        , fail [ "--foo", "baz" ]
            "Expected positional argument: \"qux...\""
        , fail
            [ "a", "--foo", "-f=10" ]
            "Trailing input: --foo, -f=10"
        ]

    , test [ pos_arg_r, eoa ]
        [ pass
            [ "a", "b", "c", "--" ]
            [ pos_arg_r :> array [ str "a" , str "b" , str "c" ]
            , eoa       :> array []
            ]
        , pass
            [ "a", "b", "c", "--", "--", "--" ]
            [ pos_arg_r :> array [ str "a" , str "b" , str "c" ]
            , eoa       :> array [ str "--" , str "--" ]
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
            [ opt 'i' "input" (oa_ "FILE") :> (str "bar") ]
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
            [ opt 'i' "input"  (oa_ "FILE") :> str "bar"
            , opt 'o' "output" (oa_ "FILE") :> str "bar" ]
          -- group should be interchangable if it's only of options:
        , pass [ "-o", "bar", "-i", "bar" ]
            [ opt 'i' "input"  (oa_ "FILE") :> str "bar"
            , opt 'o' "output" (oa_ "FILE") :> str "bar" ]
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
            [ opt 'i' "input"  (oa_ "FILE")   :> str "bar"
            , opt 'r' "redirect" (oa_ "FILE") :> str "bar"
            , opt 'o' "output" (oa_ "FILE")   :> str "bar" ]
          -- group should be interchangable if it's only of options:
        , pass [ "-o", "bar", "-r", "bar", "-i", "bar" ]
            [ opt 'i' "input"  (oa_ "FILE")   :> str "bar"
            , opt 'r' "redirect" (oa_ "FILE") :> str "bar"
            , opt 'o' "output" (oa_ "FILE")   :> str "bar" ]
        ]

    , test
        [ grr false [[
            opt 'i' "input" (oa_ "FILE")
          , po  "env" false
          ]]
        , opt 'o' "output" (oa_ "FILE")
        ]
        [ fail [] "Missing required options: -i, --input=FILE"
          -- XXX: Would be cool to show the reason the group did not parse!
        , fail [ "-i", "bar" ] "Expected positional argument: \"env\""
        , pass [ "-i", "bar", "x", "-o", "bar" ]
            [ opt 'i' "input"  (oa_ "FILE") :> str "bar"
            , po  "env" false               :> str "x"
            , opt 'o' "output" (oa_ "FILE") :> str "bar" ]
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
            [ cmd_foo          :> bool true
            , opt_o_out        :> bool true
            , opt_i_input      :> bool true
            , opt_q_qux___r    :> array [ bool true ]
            , opt_f_foo_FOZ__r :> array [ str "ox" ]
            , cmd_baz          :> bool true
            -- should have added default value that was not provided above:
            , opt_b_baz        :> str "ax"
            ]

        , pass
            [ "foo" , "--out", "-qqq", "--foo=ox", "--baz=ax", "--input", "baz" ]
            [ cmd_foo          :> bool true
            , opt_o_out        :> bool true
            , opt_q_qux___r    :> array [ bool true , bool true , bool true ]
            , opt_f_foo_FOZ__r :> array [str "ox"]
            , opt_b_baz        :> str "ax"
            , opt_i_input      :> bool true
            , cmd_baz          :> bool true
            ]

        , pass
            [ "foo", "-q", "-o", "--qux", "-i", "--baz=ax", "-f=ox", "baz" ]
            [ cmd_foo          :> bool true
            , opt_q_qux___r    :> array [ bool true , bool true ]
            , opt_o_out        :> bool true
            , opt_i_input      :> bool true
            , opt_b_baz        :> str "ax"
            , opt_f_foo_FOZ__r :> array [ str "ox" ]
            , cmd_baz          :> bool true
            ]

        , pass
            [ "foo", "--baz=ax", "-o", "-f=ox", "-i", "baz" ]
            [ cmd_foo          :> bool true
            , opt_b_baz        :> str "ax"
            , opt_o_out        :> bool true
            , opt_f_foo_FOZ__r :> array [ str "ox" ]
            , opt_i_input      :> bool true
            , cmd_baz          :> bool true
            ]

        , pass
            [ "foo", "-o", "-i", "-bax", "baz" ]
            [ cmd_foo     :> bool true
            , opt_o_out   :> bool true
            , opt_i_input :> bool true
            , opt_b_baz   :> str "ax"
            , cmd_baz     :> bool true
            -- should have added default value that was not provided above:
            , opt_b_baz   :> str "ax"
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
                  prettyPrintOut
                  expected
            in it (intercalate " " input ++ " -> " ++ msg) do
                  vliftEff do
                    validate branch input expected

    where

      prettyPrintOut :: Map Argument Value -> String
      prettyPrintOut m = "\n\t" ++ (prettyPrintMap m prettyPrintArg)

      prettyPrintMap :: forall a. Map a Value -> (a -> String) -> String
      prettyPrintMap m p = intercalate "\n\t" $
        Map.toList m <#> \(Tuple arg val) ->
          p arg ++ " => " ++ prettyPrintValue val

      validate :: forall eff. Array Argument
                            -> Array String
                            -> Either String (Map Argument Value)
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
                      ++ prettyPrintOut r
                  else return unit)
              expected
