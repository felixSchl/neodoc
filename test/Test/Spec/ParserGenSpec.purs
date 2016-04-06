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
                 (Either String (StrMap Value))

test :: Array Argument -- ^ The (flat) specification
     -> Array Case     -- ^ The array of test cases to cover
     -> Test
test a = Test (singleton a)

test' :: Array (Array Argument) -- ^ The specification
      -> Array Case             -- ^ The array of test cases to cover
      -> Test
test' as = Test (toList as)

pass :: Array String                  -- ^ ARGV
      -> (Array (Tuple String Value)) -- ^ The expected output
      -> Case
pass i o = Case i [] (Right $ StrMap.fromList $ toList o)

pass' :: Array String                  -- ^ ARGV
      -> (Array (Tuple String String)) -- ^ The environment
      -> (Array (Tuple String Value))  -- ^ The expected output
      -> Case
pass' i e o = Case i e (Right $ StrMap.fromList $ toList o)

fail :: Array String -- ^ ARGV
      -> String      -- ^ The expected error message
      -> Case
fail i e = Case i [] (Left e)

fail' :: Array String                  -- ^ ARGV
      -> (Array (Tuple String String)) -- ^ The environment
      -> String                        -- ^ The expected error message
      -> Case
fail' i e err = Case i e (Left err)

(:>) = Tuple
infixr 0 :>

parserGenSpec = \_ -> describe "The parser generator" do

  -- Some options that will be used for these tests
  let testCases = [
      test
        [ D.poR "qux" ]
        [ pass
            [ "a", "b", "c" ]
            [ "<qux>" :> D.array [ D.str "a", D.str "b", D.str "c" ]
            , "QUX"   :> D.array [ D.str "a", D.str "b", D.str "c" ]
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
            [ "<qux>" :> D.array [ D.str "a", D.str "b", D.str "c" ]
            , "QUX"   :> D.array [ D.str "a", D.str "b", D.str "c" ]
            , "--"    :> D.array []
            ]
        , pass
            [ "a", "b", "c", "--", "--", "--" ]
            [ "<qux>" :> D.array [ D.str "a", D.str "b", D.str "c" ]
            , "QUX"   :> D.array [ D.str "a", D.str "b", D.str "c" ]
            , "--"    :> D.array [ D.str "--" , D.str "--" ]
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
            [ "-h"     :> D.str "http://localhost:5000"
            , "--host" :> D.str "http://localhost:5000"
            ]
        ]

    , test
        [ D.optE 'h' "host" (D.oa "FOO" (D.str "BAR")) "HOST" ]
        [ pass'
            []
            [ "HOST" :> "HOME" ]
            [ "-h"     :> D.str "HOME"
            , "--host" :> D.str "HOME"
            ]
        ]

    , test
        [ D.grr false [[ D.opt 'i' "input" (D.oa_ "FILE") ]]
        ]
        [ fail [] "Missing required options: -i|--input=FILE"
        , pass
            [ "-i", "bar" ]
            [ "-i"      :> D.str "bar"
            , "--input" :> D.str "bar" ]
        ]

    , test
        [ D.grr false [[ D.opt 'i' "input" (D.oa_ "FILE") ]]
        ]
        [ fail [] "Missing required options: -i|--input=FILE"
        , pass
            [ "-i", "bar" ]
            [ "-i"      :> D.str "bar"
            , "--input" :> D.str "bar" ]
        ]

    , test
        [ D.grr false [[ D.opt 'i' "input" (D.oa_ "FILE") ]]
        , D.opt 'o' "output" (D.oa_ "FILE")
        ]
        [ fail []
          $ "Missing required options: -o|--output=FILE, -i|--input=FILE"

        , fail [ "-i", "bar" ]
          $ "Missing required options: -o|--output=FILE"

        , pass [ "-i", "bar", "-o", "bar" ]
            [ "--input"  :> D.str "bar"
            , "-i"       :> D.str "bar"
            , "--output" :> D.str "bar"
            , "-o"       :> D.str "bar" ]

          -- group should be interchangable if it's only of options:
        , pass [ "-o", "bar", "-i", "bar" ]
            [ "--input"  :> D.str "bar"
            , "-i"       :> D.str "bar"
            , "--output" :> D.str "bar"
            , "-o"       :> D.str "bar" ]
        ]

    , test
        [ D.grr false [[
            D.grr false [[
              D.opt 'i' "input" (D.oa_ "FILE")
            ]]
          , D.optE 'r' "redirect" (D.oa_ "FILE") "QUX"
          ]]
        , D.opt 'o' "output" (D.oa_ "FILE")
        ]
        [ fail []
          $ "Missing required options: -i|--input=FILE, -o|--output=FILE, -r|--redirect=FILE"

        , fail [ "-i", "bar", "-r", "bar" ]
            "Missing required options: -o|--output=FILE"

        , pass [ "-i", "bar", "-r", "bar", "-o", "bar" ]
            [ "--input"    :> D.str "bar"
            , "-i"         :> D.str "bar"
            , "--redirect" :> D.str "bar"
            , "-r"         :> D.str "bar"
            , "--output"   :> D.str "bar"
            , "-o"         :> D.str "bar" ]

          -- group should be interchangable if it's only of options:
        , pass [ "-o", "bar", "-r", "bar", "-i", "bar" ]
            [ "--input"    :> D.str "bar"
            , "-i"         :> D.str "bar"
            , "--redirect" :> D.str "bar"
            , "-r"         :> D.str "bar"
            , "--output"   :> D.str "bar"
            , "-o"         :> D.str "bar" ]

        , pass'
            [ "-o", "bar", "-i", "bar" ]
            [ "QUX" :> "BAR" ]
            [ "--input"    :> D.str "bar"
            , "-i"         :> D.str "bar"
            , "--redirect" :> D.str "BAR"
            , "-r"         :> D.str "BAR"
            , "--output"   :> D.str "bar"
            , "-o"         :> D.str "bar" ]
        ]

    , test
        [ D.grr false [[
            D.opt 'i' "input" (D.oa_ "FILE")
          , D.po  "env"
          ]]
        , D.opt 'o' "output" (D.oa_ "FILE")
        ]
        [ fail [] "Missing required options: -i|--input=FILE"
          -- XXX: Would be cool to show the reason the group did not parse!
        , fail [ "-i", "bar" ] "Expected positional argument: \"env\""
        , pass [ "-i", "bar", "x", "-o", "bar" ]
            [ "--input"  :> D.str "bar"
            , "-i"       :> D.str "bar"
            , "<env>"    :> D.str "x"
            , "ENV"      :> D.str "x"
            , "--output" :> D.str "bar"
            , "-o"       :> D.str "bar" ]
          -- group should NOT be interchangable if it contains non-options:
        , fail [ "-o", "bar", "x", "-i", "bar" ]
            "Missing required options: -i|--input=FILE"
        ]

    , test
        [ D.optE 'o' "out" (D.oa_ "FOO") "FOO" ]
        [ pass'
            []
            [ "FOO" :> "BAR" ]
            [  "--out" :> D.str "BAR"
            ,  "-o"    :> D.str "BAR" ]
        ]

    , test
        [ D.optE 'o' "out" (D.oa "FOO" (D.str "ADLER")) "FOO" ]
        [ pass'
            []
            [ "FOO" :> "BAR" ]
            [  "--out" :> D.str "BAR"
            ,  "-o"    :> D.str "BAR" ]
        ]

    , test'
        [ [ D.sopt_ 'a' ], [ D.sopt_ 'b' ] ]
        [ pass
            [ "-a" ]
            [ "-a" :> D.bool true ]
        , pass
            [ "-b" ]
            [ "-b" :> D.bool true ]
        , pass
            []
            -- XXX: This is awkward:
            [ "-b" :> D.bool false ]
        ]

    , test'
        [ [ D.co "a" ], [ D.co "b" ] ]
        [ pass
            [ "a" ]
            [ "a" :> D.bool true ]
        , pass
            [ "b" ]
            [ "b" :> D.bool true ]
        , fail
            [ "a", "b" ]
            "Trailing input: \"b\""
        , fail
            [ "b", "a" ]
            "Trailing input: \"a\""
        , fail
            []
            -- XXX: Could this be better?
            "Expected command: \"b\""
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
            [ "foo"     :> D.bool true
            , "--out"   :> D.bool true
            , "-o"      :> D.bool true
            , "--input" :> D.bool true
            , "-i"      :> D.bool true
            , "--qux"   :> D.int 1
            , "-q"      :> D.int 1
            , "--foo"   :> D.array [ D.str "ox" ]
            , "-f"      :> D.array [ D.str "ox" ]
            , "baz"     :> D.bool true
            , "--baz"   :> D.str "ax"
            , "-b"      :> D.str "ax"
            ]

        , pass
            [ "foo" , "--out", "-qqq", "--foo=ox", "--baz=ax", "--input", "baz" ]
            [ "foo"     :> D.bool true
            , "--out"   :> D.bool true
            , "-o"      :> D.bool true
            , "--qux"   :> D.int 3
            , "-q"      :> D.int 3
            , "--foo"   :> D.array [ D.str "ox" ]
            , "-f"      :> D.array [ D.str "ox" ]
            , "--baz"   :> D.str "ax"
            , "-b"      :> D.str "ax"
            , "--input" :> D.bool true
            , "-i"      :> D.bool true
            , "baz"     :> D.bool true
            ]

        , pass
            [ "foo", "-q", "-o", "--qux", "-i", "--baz=ax", "-f=ox", "baz" ]
            [ "foo"     :> D.bool true
            , "--qux"   :> D.int 2
            , "-q"      :> D.int 2
            , "--out"   :> D.bool true
            , "-o"      :> D.bool true
            , "--input" :> D.bool true
            , "-i"      :> D.bool true
            , "--baz"   :> D.str "ax"
            , "-b"      :> D.str "ax"
            , "--foo"   :> D.array [ D.str "ox" ]
            , "-f"      :> D.array [ D.str "ox" ]
            , "baz"     :> D.bool true
            ]

        , pass
            [ "foo", "--baz=ax", "-o", "-f=ox", "-i", "baz" ]
            [ "foo"     :> D.bool true
            , "--baz"   :> D.str "ax"
            , "-b"      :> D.str "ax"
            , "--out"   :> D.bool true
            , "-o"      :> D.bool true
            , "--foo"   :> D.array [ D.str "ox" ]
            , "-f"      :> D.array [ D.str "ox" ]
            , "--input" :> D.bool true
            , "-i"      :> D.bool true
            , "baz"     :> D.bool true
            , "--qux"   :> D.int 0
            , "-q"      :> D.int 0
            ]

        , pass
            [ "foo", "-o", "-i", "-bax", "baz" ]
            [ "foo"     :> D.bool true
            , "--out"   :> D.bool true
            , "-o"      :> D.bool true
            , "--input" :> D.bool true
            , "-i"      :> D.bool true
            , "--baz"   :> D.str "ax"
            , "-b"      :> D.str "ax"
            , "baz"     :> D.bool true
            , "--baz"   :> D.str "ax"
            , "-b"      :> D.str "ax"
            , "--qux"   :> D.int 0
            , "-q"      :> D.int 0
            ]

        , fail
            [ "foo" ]
            "Expected command: \"baz\""
        , fail
            [ "foo", "-o", "-i", "-bax" ]
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
                premsg = if A.length input > 0
                            then intercalate " " input
                            else "(no input)"
            in it (premsg ++ " -> " ++ msg) do
                  vliftEff do
                    validate bs
                             input
                             (Env.fromFoldable env)
                             expected

    where

      prettyPrintOut :: StrMap Value -> String
      prettyPrintOut m = "\n\t" ++ prettyPrintStrMap m

      prettyPrintStrMap :: StrMap Value -> String
      prettyPrintStrMap m = intercalate "\n\t" $
        StrMap.toList m <#> \(Tuple arg val) ->
          arg ++ " => " ++ prettyPrintValue val

      validate :: forall eff.  List (Array Argument)
                            -> Array String
                            -> Env
                            -> Either String (StrMap Value)
                            -> Eff (err :: EXCEPTION | eff) Unit
      validate args argv env expected = do
        let prg = singleton $ Usage $ D.br <$> args
            result = uncurry (T.reduce prg env)
                <$> runParser
                      env
                      argv
                      (genParser prg false)

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
              (\e ->
                throwException $ error $
                  "Missing expected exception:\n"
                    ++ show e
                    ++ "\n\ninstead received output:\n"
                    ++ prettyPrintOut r)
              (\r' ->
                if (r /= r')
                  then throwException $ error $
                    "Unexpected output:\n"
                      ++ prettyPrintOut r
                  else return unit)
              expected
