module Test.Spec.ParserGenSpec (parserGenSpec) where

import Prelude
import Debug.Trace
import Data.Tuple (Tuple(..))
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (EXCEPTION())
import Data.Maybe (Maybe(..))
import Data.Either (Either(..), either)
import Data.List (List(..), toList, length, fromList, singleton)
import Data.Map (Map(..))
import Data.StrMap as StrMap
import Data.StrMap (StrMap())
import Data.Tuple (uncurry)
import Data.Map as Map
import Data.Array as A
import Data.Foldable (for_, intercalate)
import Control.Monad.Eff.Exception (error, throwException)
import Text.Parsing.Parser as P

import Test.Assert (assert)
import Test.Spec (describe, it, Spec())
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Assert.Simple
import Test.Support (vliftEff, runMaybeEff, runEitherEff)

import Language.Docopt.Errors
import Language.Docopt.Argument
import Language.Docopt.Value
import Language.Docopt.Usage
import Language.Docopt.Env (Env())
import Language.Docopt.ParserGen (genParser, runParser)
import Language.Docopt.Argument as D
import Language.Docopt.Env      as Env
import Language.Docopt.Trans    as T
import Test.Support.Docopt      as D

data Test = Test (List (Array Argument)) (Array Case)
data Case = Case (Array String)
                 (Array (Tuple String String))
                 (Either String (Map Argument Value))

test :: Array Argument -> Array Case -> Test
test a = Test (singleton a)

test' :: Array (Array Argument) -> Array Case -> Test
test' as = Test (toList as)

pass :: Array String -> (Array (Tuple Argument Value)) -> Case
pass i o = Case i [] (Right $ Map.fromList $ toList o)

pass' :: Array String
      -> (Array (Tuple String String))
      -> (Array (Tuple Argument Value))
      -> Case
pass' i e o = Case i e (Right $ Map.fromList $ toList o)

fail :: Array String -> String -> Case
fail i e = Case i [] (Left e)

fail' :: Array String
      -> (Array (Tuple String String))
      -> String
      -> Case
fail' i e err = Case i e (Left err)

(:>) = Tuple
infixr 0 :>

parserGenSpec = \_ -> describe "The generator" do

  -- Some options that will be used for these tests
  let testCases = [

      test
        [ D.poR "qux" ]
        [ pass
            [ "a", "b", "c" ]
            [ D.poR "qux" :> D.array [ D.str "a"
                                     , D.str "b"
                                     , D.str "c"
                                     ]
            ]
        , fail [ "--foo", "baz" ]
            "Expected positional argument: \"qux...\""
        , fail
            [ "a", "--foo", "-f=10" ]
            "Trailing input: --foo, -f=10"
        ]

    , test
        [ D.poR "qux", D.eoa ]
        [ pass
            [ "a", "b", "c", "--" ]
            [ D.poR "qux" :> D.array [ D.str "a"
                                     , D.str "b"
                                     , D.str "c"
                                     ]
            , D.eoa :> D.array []
            ]
        , pass
            [ "a", "b", "c", "--", "--", "--" ]
            [ D.poR "qux" :> D.array [ D.str "a"
                                     , D.str "b"
                                     , D.str "c"
                                     ]
            , D.eoa :> D.array [ D.str "--" , D.str "--" ]
            ]
        ]

    , test
        [ D.grr false [] ]
        [ pass [] [] ]

    , test
        [ D.gro false [] ]
        [ pass [] [] ]

    , test
        [ D.opt 'h' "host" (D.oa "host[:port]"
                           (D.str "http://localhost:3000"))
        ]
        [ pass
            [ "-hhttp://localhost:5000" ]
            [ D.opt 'h'
                    "host"
                    (D.oa "host[:port]" (D.str "http://localhost:3000"))
              :> D.str "http://localhost:5000"
            ]
        ]

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
        [ fail []
          $ "Missing required options: "
              ++ "-o, --output=FILE, (-i, --input=FILE)"

        , fail [ "-i", "bar" ]
          $ "Missing required options: -o, --output=FILE"

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
          $ "Missing required options: "
              ++ "-o, --output=FILE, "
              ++ "((-i, --input=FILE) -r, --redirect=FILE)"

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
        [ D.optE 'o' "out" (D.oa_ "FOO") "FOO" ]
        [ pass'
            []
            [ "FOO" :> "BAR" ]
            [  D.optE 'o' "out" (D.oa_ "FOO") "FOO" :> D.str "BAR" ]
        ]

    , test
        [ D.optE 'o' "out" (D.oa "FOO" (D.str "ADLER")) "FOO" ]
        [ pass'
            []
            [ "FOO" :> "BAR" ]
            [  D.optE 'o' "out" (D.oa "FOO" (D.str "ADLER")) "FOO" :> D.str "BAR" ]
        ]

    , test'
        [ [ D.sopt_ 'a' ], [ D.sopt_ 'b' ] ]
        [ pass
            [ "-a" ]
            [ D.sopt_ 'a' :> D.bool true ]
        , pass
            [ "-b" ]
            [ D.sopt_ 'b' :> D.bool true ]
        , pass
            []
            []
        ]

    , test'
        [ [ D.co "a" ], [ D.co "b" ] ]
        [ pass
            [ "a" ]
            [ D.co "a" :> D.bool true ]
        , pass
            [ "b" ]
            [ D.co "b" :> D.bool true ]
        , pass
            []
            []
        ]

    , test
        [ D.co    "foo"
        , D.opt_  'o' "out"
        , D.optR_ 'q' "qux"
        , D.opt   'b' "baz" (D.oa "BAZ" $ D.str "ax")
        , D.opt_  'i' "input"
        , D.optR  'f' "foo" (D.oa_ "FOZ")
        , D.co     "baz"
        ]
        [ pass
            [ "foo", "--out", "--input", "--qux", "--foo=ox", "baz" ]
            [ D.co    "foo"                   :> D.bool true
            , D.opt_  'o' "out"               :> D.bool true
            , D.opt_  'i' "input"             :> D.bool true
            , D.optR_ 'q' "qux"               :> D.array [ D.bool true ]
            , D.optR  'f' "foo" (D.oa_ "FOZ") :> D.array [ D.str "ox" ]
            , D.co    "baz"                   :> D.bool true
            -- should have added default value that was not provided above:
            , D.opt 'b' "baz" (D.oa "BAZ" $ D.str "ax") :> D.str "ax"
            ]

        , pass
            [ "foo" , "--out", "-qqq", "--foo=ox", "--baz=ax", "--input", "baz" ]
            [ D.co    "foo"     :> D.bool true
            , D.opt_  'o' "out" :> D.bool true
            , D.optR_ 'q' "qux" :> D.array [ D.bool true
                                           , D.bool true
                                           , D.bool true
                                           ]
            , D.optR  'f' "foo" (D.oa_ "FOZ") :> D.array [ D.str "ox"
                                                         ]
            , D.opt   'b' "baz" (D.oa "BAZ" $ D.str "ax") :> D.str "ax"
            , D.opt_  'i' "input" :> D.bool true
            , D.co    "baz"       :> D.bool true
            ]

        , pass
            [ "foo", "-q", "-o", "--qux", "-i", "--baz=ax", "-f=ox", "baz" ]
            [ D.co    "foo"       :> D.bool true
            , D.optR_ 'q' "qux"   :> D.array [ D.bool true , D.bool true ]
            , D.opt_  'o' "out"   :> D.bool true
            , D.opt_  'i' "input" :> D.bool true
            , D.opt   'b' "baz" (D.oa "BAZ" $ D.str "ax") :> D.str "ax"
            , D.optR  'f' "foo" (D.oa_ "FOZ") :> D.array [ D.str "ox" ]
            , D.co    "baz"       :> D.bool true
            ]

        , pass
            [ "foo", "--baz=ax", "-o", "-f=ox", "-i", "baz" ]
            [ D.co    "foo"       :> D.bool true
            , D.opt   'b' "baz" (D.oa "BAZ" $ D.str "ax") :> D.str "ax"
            , D.opt_  'o' "out"   :> D.bool true
            , D.optR  'f' "foo" (D.oa_ "FOZ") :> D.array [ D.str "ox" ]
            , D.opt_  'i' "input" :> D.bool true
            , D.co     "baz"      :> D.bool true
            , D.optR_ 'q' "qux"   :> D.array [ D.bool false ]
            ]

        , pass
            [ "foo", "-o", "-i", "-bax", "baz" ]
            [ D.co    "foo"       :> D.bool true
            , D.opt_  'o' "out"   :> D.bool true
            , D.opt_  'i' "input" :> D.bool true
            , D.opt   'b' "baz" (D.oa "BAZ" $ D.str "ax") :> D.str "ax"
            , D.co    "baz"       :> D.bool true
            -- should have added default value that was not provided above:
            , D.opt   'b' "baz" (D.oa "BAZ" $ D.str "ax") :> D.str "ax"
            , D.optR_ 'q' "qux" :> D.array [ D.bool false ]
            ]

        , fail
            [ "foo" ]
            -- TODO: Create a more sophisticated way to test this
            "Expected command: \"baz\""
        , fail
            [ "foo", "-o", "-i", "-bax" ]
            -- TODO: Create a more sophisticated way to test this
            "Expected command: \"baz\""
        ]

    , test
        [ D.gro false [[ D.co "foo" ]] ]
        [ fail [ "goo" ] "Trailing input: \"goo\"" ]
    , test
        [ D.grr false [[ D.co "foo" ]] ]
        [ fail [ "goo" ] "Expected command: \"foo\"" ]
  ]

  for_ testCases \(Test bs kases) -> do
    describe (intercalate " | " $ prettyPrintBranch <<< D.br <$> bs) do
      for_ kases \(Case input env expected) ->
            let msg = either
                  (\e -> "Should fail with \"" ++ e ++ "\"")
                  prettyPrintOut
                  expected
            in it (intercalate " " input ++ " -> " ++ msg) do
                  vliftEff do
                    validate bs
                             input
                             (Env.fromFoldable env)
                             expected

    where

      prettyPrintOut :: Map Argument Value -> String
      prettyPrintOut m = "\n\t" ++ (prettyPrintMap m prettyPrintArg)

      prettyPrintMap :: forall a. Map a Value -> (a -> String) -> String
      prettyPrintMap m p = intercalate "\n\t" $
        Map.toList m <#> \(Tuple arg val) ->
          p arg ++ " => " ++ prettyPrintValue val

      validate :: forall eff.  List (Array Argument)
                            -> Array String
                            -> Env
                            -> Either String (Map Argument Value)
                            -> Eff (err :: EXCEPTION | eff) Unit
      validate args argv env expected = do
        let result = uncurry (T.reduce env)
                <$> runParser
                      env
                      argv
                      (genParser $ singleton $ Usage $ D.br <$> args)

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
