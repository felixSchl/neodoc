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

import Test.Assert (assert)
import Test.Spec (describe, it, Spec())
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Assert.Simple
import Test.Support (vliftEff, runMaybeEff, runEitherEff)

import Language.Docopt.Errors
import Language.Docopt.Argument
import Language.Docopt.Value
import Language.Docopt.Usage
import Language.Docopt.ParserGen (genParser, runParser)
import qualified Language.Docopt.Argument as D
import qualified Test.Support.Docopt      as D

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
  let cmd_foo          = D.co    "foo"
      opt_f_foo_FOZ__r = D.optR  'f' "foo" (D.oa_ "FOZ")
      opt_q_qux___r    = D.optR_ 'q' "qux"
      opt_b_baz        = D.opt   'b' "baz" (D.oa "BAZ" $ D.str "ax")
      opt_o_out        = D.opt_  'o' "out"
      opt_i_input      = D.opt_  'i' "input"
      pos_arg_r        = D.poR    "qux"
      cmd_baz          = D.co     "baz"

  let testCases = [

      test [ pos_arg_r ]
        [ pass
            [ "a", "b", "c" ]
            [ pos_arg_r :> D.array [ D.str "a" , D.str "b" , D.str "c" ] ]
        , fail [ "--foo", "baz" ]
            "Expected positional argument: \"qux...\""
        , fail
            [ "a", "--foo", "-f=10" ]
            "Trailing input: --foo, -f=10"
        ]

    , test [ pos_arg_r, D.eoa ]
        [ pass
            [ "a", "b", "c", "--" ]
            [ pos_arg_r :> D.array [ D.str "a" , D.str "b" , D.str "c" ]
            , D.eoa     :> D.array []
            ]
        , pass
            [ "a", "b", "c", "--", "--", "--" ]
            [ pos_arg_r :> D.array [ D.str "a" , D.str "b" , D.str "c" ]
            , D.eoa     :> D.array [ D.str "--" , D.str "--" ]
            ]
        ]

    , test
        [ D.grr false [] ]
        [ pass [] [] ]

    , test
        [ D.gro false [] ]
        [ pass [] [] ]

    , test
        [ D.grr false [[
            D.opt 'i' "input" (D.oa_ "FILE")
          ]]
        ]
        [ fail [] "Missing required options: (-i, --input=FILE)"
        , pass
            [ "-i", "bar" ]
            [ D.opt 'i' "input" (D.oa_ "FILE") :> (D.str "bar") ]
        ]

    , test
        [ D.grr false [[
            D.opt 'i' "input" (D.oa_ "FILE")
          ]]
        , D.opt 'o' "output" (D.oa_ "FILE")
        ]
        [ fail [] "Missing required options: -o, --output=FILE, (-i, --input=FILE)"
        , fail [ "-i", "bar" ] "Missing required options: -o, --output=FILE"
        , pass [ "-i", "bar", "-o", "bar" ]
            [ D.opt 'i' "input"  (D.oa_ "FILE") :> D.str "bar"
            , D.opt 'o' "output" (D.oa_ "FILE") :> D.str "bar" ]
          -- group should be interchangable if it's only of options:
        , pass [ "-o", "bar", "-i", "bar" ]
            [ D.opt 'i' "input"  (D.oa_ "FILE") :> D.str "bar"
            , D.opt 'o' "output" (D.oa_ "FILE") :> D.str "bar" ]
        ]

    , test
        [ D.grr false [[
            D.grr false [[
              D.opt 'i' "input" (D.oa_ "FILE")
            ]]
          , D.opt 'r' "redirect" (D.oa_ "FILE")
          ]]
        , D.opt 'o' "output" (D.oa_ "FILE")
        ]
        [ fail []
            "Missing required options: -o, --output=FILE, ((-i, --input=FILE) -r, --redirect=FILE)"
        , fail [ "-i", "bar", "-r", "bar" ]
            "Missing required options: -o, --output=FILE"
        , pass [ "-i", "bar", "-r", "bar", "-o", "bar" ]
            [ D.opt 'i' "input"  (D.oa_ "FILE")   :> D.str "bar"
            , D.opt 'r' "redirect" (D.oa_ "FILE") :> D.str "bar"
            , D.opt 'o' "output" (D.oa_ "FILE")   :> D.str "bar" ]
          -- group should be interchangable if it's only of options:
        , pass [ "-o", "bar", "-r", "bar", "-i", "bar" ]
            [ D.opt 'i' "input"  (D.oa_ "FILE")   :> D.str "bar"
            , D.opt 'r' "redirect" (D.oa_ "FILE") :> D.str "bar"
            , D.opt 'o' "output" (D.oa_ "FILE")   :> D.str "bar" ]
        ]

    , test
        [ D.grr false [[
            D.opt 'i' "input" (D.oa_ "FILE")
          , D.po  "env"
          ]]
        , D.opt 'o' "output" (D.oa_ "FILE")
        ]
        [ fail [] "Missing required options: -i, --input=FILE"
          -- XXX: Would be cool to show the reason the group did not parse!
        , fail [ "-i", "bar" ] "Expected positional argument: \"env\""
        , pass [ "-i", "bar", "x", "-o", "bar" ]
            [ D.opt 'i' "input"  (D.oa_ "FILE") :> D.str "bar"
            , D.po  "env"                       :> D.str "x"
            , D.opt 'o' "output" (D.oa_ "FILE") :> D.str "bar" ]
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
            [ cmd_foo          :> D.bool true
            , opt_o_out        :> D.bool true
            , opt_i_input      :> D.bool true
            , opt_q_qux___r    :> D.array [ D.bool true ]
            , opt_f_foo_FOZ__r :> D.array [ D.str "ox" ]
            , cmd_baz          :> D.bool true
            -- should have added default value that was not provided above:
            , opt_b_baz        :> D.str "ax"
            ]

        , pass
            [ "foo" , "--out", "-qqq", "--foo=ox", "--baz=ax", "--input", "baz" ]
            [ cmd_foo          :> D.bool true
            , opt_o_out        :> D.bool true
            , opt_q_qux___r    :> D.array [ D.bool true , D.bool true , D.bool true ]
            , opt_f_foo_FOZ__r :> D.array [D.str "ox"]
            , opt_b_baz        :> D.str "ax"
            , opt_i_input      :> D.bool true
            , cmd_baz          :> D.bool true
            ]

        , pass
            [ "foo", "-q", "-o", "--qux", "-i", "--baz=ax", "-f=ox", "baz" ]
            [ cmd_foo          :> D.bool true
            , opt_q_qux___r    :> D.array [ D.bool true , D.bool true ]
            , opt_o_out        :> D.bool true
            , opt_i_input      :> D.bool true
            , opt_b_baz        :> D.str "ax"
            , opt_f_foo_FOZ__r :> D.array [ D.str "ox" ]
            , cmd_baz          :> D.bool true
            ]

        , pass
            [ "foo", "--baz=ax", "-o", "-f=ox", "-i", "baz" ]
            [ cmd_foo          :> D.bool true
            , opt_b_baz        :> D.str "ax"
            , opt_o_out        :> D.bool true
            , opt_f_foo_FOZ__r :> D.array [ D.str "ox" ]
            , opt_i_input      :> D.bool true
            , cmd_baz          :> D.bool true
            ]

        , pass
            [ "foo", "-o", "-i", "-bax", "baz" ]
            [ cmd_foo     :> D.bool true
            , opt_o_out   :> D.bool true
            , opt_i_input :> D.bool true
            , opt_b_baz   :> D.str "ax"
            , cmd_baz     :> D.bool true
            -- should have added default value that was not provided above:
            , opt_b_baz   :> D.str "ax"
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
        [ D.gro false [[ cmd_foo ]] ]
        [ fail [ "goo" ] "Trailing input: \"goo\"" ]
    , test
        [ D.grr false [[ cmd_foo ]] ]
        [ fail [ "goo" ] "Expected command: \"foo\"" ]
  ]

  for_ testCases \(Test branch kases) -> do
    describe (prettyPrintBranch $ D.br branch) do
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
                (genParser $ singleton $ Usage $ singleton $ D.br args)
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
