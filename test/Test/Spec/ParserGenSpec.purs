module Test.Spec.CompilerSpec (parserGenSpec) where

import Prelude
import Debug.Trace
import Data.Tuple (Tuple(..))
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (EXCEPTION())
import Data.Maybe (Maybe(..), fromMaybe)
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
import Test.Support.Arguments

import Language.Docopt (parseDocopt)
import Language.Docopt.Errors
import Language.Docopt.Argument
import Language.Docopt.Value
import Language.Docopt.Usage
import Language.Docopt.Env (Env())
import Language.Docopt.Compiler (genParser, runParser)
import Language.Docopt.Argument   as D
import Language.Docopt.Env        as Env
import Language.Docopt.Trans.Flat as T
import Test.Support.Docopt        as D

type Options =  { customEOA    :: Array String
                , optionsFirst :: Boolean
                }

type Test = { help  :: String
            , cases :: Array Case
            }

type Case = { argv     :: Array String
            , env      :: Array (Tuple String String)
            , expected :: Either String (StrMap Value)
            , options  :: Maybe Options
            }

test :: String     -- ^ The help text
     -> Array Case -- ^ The array of test cases to cover
     -> Test
test help ks = { help: help, cases: ks }

pass ::  Maybe Options                -- ^ The options
      -> Array String                 -- ^ ARGV
      -> (Array (Tuple String Value)) -- ^ The expected output
      -> Case
pass opts i o = { argv:     i
                , env:      []
                , options:  opts
                , expected: Right $ StrMap.fromList $ toList o
                }

pass' :: Maybe Options                 -- ^ The options
      -> Array String                  -- ^ ARGV
      -> (Array (Tuple String String)) -- ^ The environment
      -> (Array (Tuple String Value))  -- ^ The expected output
      -> Case
pass' opts i e o = { argv:     i
                   , env:      e
                   , options:  opts
                   , expected: Right $ StrMap.fromList $ toList o
                   }

fail :: Array String -- ^ ARGV
      -> String      -- ^ The expected error message
      -> Case
fail i e = { argv: i, env: [],  expected: Left e, options: Nothing }

fail' :: Array String                  -- ^ ARGV
      -> (Array (Tuple String String)) -- ^ The environment
      -> String                        -- ^ The expected error message
      -> Case
fail' i e err = { argv: i, env: e, expected: Left err, options: Nothing }

(:>) = Tuple
infixr 0 :>

parserGenSpec = \_ -> describe "The parser generator" do
  it "" do
    pure unit

  -- Some options that will be used for these tests
  let testCases = [
      test
        "usage: prog <qux>..."
        [ pass Nothing
            [ "a", "b", "c" ]
            [ "<qux>" :> D.array [ D.str "a", D.str "b", D.str "c" ]
            ]
        , fail [ "--foo", "baz" ]
            "Expected <qux>..., but got --foo"
        , fail
            [ "a", "--foo", "-f=10" ]
            "Unmatched option: --foo"
        ]

    , test
        "usage: prog <qux>... --"
        [ pass Nothing
            [ "a", "b", "c", "--" ]
            [ "<qux>" :> D.array [ D.str "a", D.str "b", D.str "c" ]
            , "--"    :> D.array []
            ]
        , pass Nothing
            [ "a", "b", "c", "--", "--", "--" ]
            [ "<qux>" :> D.array [ D.str "a", D.str "b", D.str "c" ]
            , "--"    :> D.array [ D.str "--" , D.str "--" ]
            ]
        ]

    , test
        """
        usage: prog [options]
        options:
          -h, --host <host[:port]> [default: "http://localhost:3000"]
        """
        [ pass
            Nothing
            [ "-hhttp://localhost:5000" ]
            [ "-h"     :> D.str "http://localhost:5000"
            , "--host" :> D.str "http://localhost:5000"
            ]
        ]

    , test
        """
        usage: prog [options]
        options:
          -h, --host FOO [default: BAR] [env: HOST]
        """
        [ pass'
            Nothing
            []
            [ "HOST" :> "HOME" ]
            [ "-h"     :> D.str "HOME"
            , "--host" :> D.str "HOME"
            ]
        ]

    , test
        """
        usage: prog (-iFILE)
        options:
          -i, --input FILE
        """
        [ fail [] "Expected option(s): -i|--input=FILE"
        , pass Nothing
            [ "-i", "bar" ]
            [ "-i"      :> D.str "bar"
            , "--input" :> D.str "bar" ]
        ]

    , test
        """
        usage: prog (-iFILE) -oFILE
        options:
          -i, --input FILE
          -o, --output FILE
        """
        [ fail []
          $ "Expected option(s): -i|--input=FILE, -o|--output=FILE"

        , fail [ "-i", "bar" ]
          $ "Expected option(s): -o|--output=FILE"

        , pass Nothing
            [ "-i", "bar", "-o", "bar" ]
            [ "--input"  :> D.str "bar"
            , "-i"       :> D.str "bar"
            , "--output" :> D.str "bar"
            , "-o"       :> D.str "bar" ]

          -- group should be interchangable if it's only of options:
        , pass Nothing
            [ "-o", "bar", "-i", "bar" ]
            [ "--input"  :> D.str "bar"
            , "-i"       :> D.str "bar"
            , "--output" :> D.str "bar"
            , "-o"       :> D.str "bar" ]
        ]

    , test
        """
        usage: prog ((-iFILE) -rFILE) -oFILE
        options:
          -i, --input FILE
          -o, --output FILE
          -r, --redirect FILE [env: QUX]
        """
        [ fail []
          $ "Expected option(s): -i|--input=FILE -r|--redirect=FILE, -o|--output=FILE"

        , fail [ "-i", "bar", "-r", "bar" ]
            "Expected option(s): -o|--output=FILE"

        , pass Nothing
            [ "-i", "bar", "-r", "bar", "-o", "bar" ]
            [ "--input"    :> D.str "bar"
            , "-i"         :> D.str "bar"
            , "--redirect" :> D.str "bar"
            , "-r"         :> D.str "bar"
            , "--output"   :> D.str "bar"
            , "-o"         :> D.str "bar" ]

          -- group should be interchangable if it's only of options:
        , pass Nothing
            [ "-o", "bar", "-r", "bar", "-i", "bar" ]
            [ "--input"    :> D.str "bar"
            , "-i"         :> D.str "bar"
            , "--redirect" :> D.str "bar"
            , "-r"         :> D.str "bar"
            , "--output"   :> D.str "bar"
            , "-o"         :> D.str "bar" ]

        , pass' Nothing
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
        """
        usage: prog ((-i FILE) <env>) -oFILE
        options:
          -i, --input FILE
          -o, --output FILE
          -r, --redirect FILE
        """
        [ fail [] "Expected option(s): -i|--input=FILE"
          -- XXX: Would be cool to show the reason the group did not parse!
        , fail [ "-i", "bar" ] "Expected <env>"
        , pass Nothing
            [ "-i", "bar", "x", "-o", "bar" ]
            [ "--input"  :> D.str "bar"
            , "-i"       :> D.str "bar"
            , "<env>"    :> D.str "x"
            , "--output" :> D.str "bar"
            , "-o"       :> D.str "bar" ]
          -- group should NOT be interchangable if it contains non-options:
        , fail [ "-o", "bar", "x", "-i", "bar" ]
            "Expected option(s): -i|--input=FILE"
        ]

    , test
        """
        usage: prog [options]
        options:
          -o, --out FOO [env: FOO]
        """
        [ pass' Nothing
            []
            [ "FOO"    :> "BAR" ]
            [  "--out" :> D.str "BAR"
            ,  "-o"    :> D.str "BAR" ]
        ]

    , test
        """
        usage: prog [options]
        options:
          -o, --out FOO [default: "ADLER"] [env: FOO]
        """
        [ pass' Nothing
            []
            [ "FOO" :> "BAR" ]
            [  "--out" :> D.str "BAR"
            ,  "-o"    :> D.str "BAR" ]
        ]

    , test
        """
        usage: prog -a
           or: prog -b
        """
        [ pass Nothing
            [ "-a" ]
            [ "-a" :> D.bool true ]
        , pass Nothing
            [ "-b" ]
            [ "-b" :> D.bool true ]
        , pass Nothing
            []
            []
        ]

    , test
        """
        usage: prog a
           or: prog b
        """
        [ pass Nothing
            [ "a" ]
            [ "a" :> D.bool true ]
        , pass Nothing
            [ "b" ]
            [ "b" :> D.bool true ]
        , fail
            [ "a", "b" ]
            "Unmatched command: b"
        , fail
            [ "b", "a" ]
            "Unmatched command: a"
        , fail [] ""
        ]

    , test
        """
        usage: prog foo -io -q... -b=BAZ -f=FOZ... baz
        options:
          -i, --input
          -o, --out
          -q, --qux...
          -b, --baz BAZ [default: "ax"]
          -f, --foo FOZ...
        """
        [ pass Nothing
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

        , pass Nothing
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

        , pass Nothing
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

        , pass Nothing
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
            ]

        , pass Nothing
            [ "foo", "-o", "-i", "-bax", "-f=ox", "baz" ]
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
            , "--foo"   :> D.array [ D.str "ox" ]
            , "-f"      :> D.array [ D.str "ox" ]
            ]

        , fail
            [ "foo" ]
            "Expected option(s): -f|--foo=FOZ..."
        , fail
            [ "foo", "-o", "-i", "-bax" ]
            "Expected option(s): -f|--foo=FOZ..."
        ]

    , test
        """
        usage: prog [foo]
        """
        [ fail [ "goo" ] "Unmatched command: goo" ]

    , test
        """
        usage: prog (foo)
        """
        [ fail [ "goo" ] "Expected foo, but got goo" ]
  ]

  for_ testCases \(({ help, cases })) -> do
    describe help do
      for_ cases \(({ argv, env, expected, options })) ->
            let msg = either (\e -> "Should fail with \"" <> e <> "\"")
                              prettyPrintOut
                              expected
                premsg = if A.length argv > 0
                            then intercalate " " argv
                            else "(no input)"
            in it (premsg <> " -> " <> msg) do
                  vliftEff do
                    { specification } <- runEitherEff do
                      parseDocopt help { smartOptions: true }
                    validate (specification)
                             argv
                             (Env.fromFoldable env)
                             options
                             expected

    where

      prettyPrintOut :: StrMap Value -> String
      prettyPrintOut m = "\n\t" <> prettyPrintStrMap m

      prettyPrintStrMap :: StrMap Value -> String
      prettyPrintStrMap m = intercalate "\n\t" $
        StrMap.toList m <#> \(Tuple arg val) ->
          arg <> " => " <> prettyPrintValue val

      validate :: forall eff.  List Usage
                            -> Array String
                            -> Env
                            -> Maybe Options
                            -> Either String (StrMap Value)
                            -> Eff (err :: EXCEPTION | eff) Unit
      validate spec argv env options expected = do
        let result = uncurry (T.reduce spec env)
                <$> runParser
                      env
                      argv
                      (genParser spec (fromMaybe {
                        optionsFirst: false
                      , customEOA:    []
                      } options))

        case result of
          Left (e@(P.ParseError { message: msg })) ->
            either
              (\e' ->
                if (msg /= e')
                  then throwException $ error $
                    "Unexpected error:\n" <> msg
                  else return unit)
              (const $ throwException $ error $ show e)
              expected
          Right r -> do
            either
              (\e ->
                throwException $ error $
                  "Missing expected exception:"
                    <> "\n"
                    <> show e
                    <> "\n"
                    <> "\n"
                    <> "instead received output:"
                    <> "\n"
                    <> prettyPrintOut r)
              (\r' ->
                if (r /= r')
                  then throwException $ error $
                    "Unexpected output:\n"
                      <> prettyPrintOut r
                  else return unit)
              expected
