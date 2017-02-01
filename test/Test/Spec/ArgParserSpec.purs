module Test.Spec.ArgParserSpec (argParserSpec) where

import Prelude
import Debug.Trace
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Control.Monad.Eff (Eff())
import Control.Monad (when)
import Control.Monad.Eff.Exception (EXCEPTION())
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Either (Either(..), either, isLeft)
import Data.List (List(..), fromFoldable, length, singleton, concat)
import Data.Traversable (for)
import Data.Map (Map(..))
import Data.StrMap as StrMap
import Data.StrMap (StrMap())
import Data.Tuple (uncurry)
import Data.Map as Map
import Data.Array as A
import Data.Foldable (for_, intercalate)
import Control.Monad.Eff.Exception (error, throwException)
import Data.String.Chalk as Chalk
import Data.TemplateString.Unsafe ((<~>))
import Data.Bifunctor (lmap, rmap)
import Text.Wrap (dedent)
import Partial.Unsafe
import Data.Pretty (pretty)

import Test.Assert (assert)
import Test.Spec (describe, it, Spec())
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Support (vliftEff, runMaybeEff, runEitherEff)
import Test.Support.Arguments
import Test.Support.Value as V

import Neodoc.Value
import Neodoc.Env (Env)
import Neodoc.Env as Env
import Neodoc.Error.Class (capture) as Error
import Neodoc.Spec as Neodoc
import Neodoc.Spec (Spec(..))
import Neodoc.Spec.Parser as Spec
import Neodoc.Spec.Lexer as Lexer
import Neodoc.Scanner as Scanner
import Neodoc.Solve as Solver
import Neodoc.Solve.Error (SolveError(..))
import Neodoc.Solve.ExpandOptions (expandOptions, ExpandedOptionsLayout(..)
, ExpandedOptionsLayoutArg(..))
import Neodoc.Data.SolvedLayout (SolvedLayout(..))
import Neodoc.ArgParser as ArgParser
import Neodoc.ArgParser (ArgParseResult(..))
import Neodoc.Evaluate as Evaluate

-- hack to easily isolate tests
isolate :: Boolean
isolate = false

type Options =  { stopAt            :: Array String
                , optionsFirst      :: Boolean
                , smartOptions      :: Boolean
                , requireFlags      :: Boolean
                , laxPlacement      :: Boolean
                , repeatableOptions :: Boolean
                , allowUnknown      :: Boolean
                }

defaultOptions :: Options
defaultOptions = {
  stopAt: []
, optionsFirst: false
, smartOptions: true
, requireFlags: false
, laxPlacement: false
, repeatableOptions: false
, allowUnknown: false
}

type Test = { help  :: String
            , cases :: Array Case
            , skip  :: Boolean -- hack to easily isolate tests
            }

type Case = { argv     :: Array String
            , env      :: Array (Tuple String String)
            , expected :: Either String (StrMap Value)
            , options  :: Maybe Options
            }

test :: String     -- ^ The help text
     -> Array Case -- ^ The array of test cases to cover
     -> Test
test help ks = { help: help, cases: ks, skip: isolate }

-- hack to easily isolate tests
test2 :: String    -- ^ The help text
      -> Array Case -- ^ The array of test cases to cover
      -> Test
test2 help ks = { help: help, cases: ks, skip: false }

pass ::  Maybe Options                -- ^ The options
      -> Array String                 -- ^ ARGV
      -> (Array (Tuple String Value)) -- ^ The expected output
      -> Case
pass opts i o = { argv:     i
                , env:      []
                , options:  opts
                , expected: Right $ StrMap.fromFoldable $ fromFoldable o
                }

pass' :: Maybe Options                 -- ^ The options
      -> Array String                  -- ^ ARGV
      -> (Array (Tuple String String)) -- ^ The environment
      -> (Array (Tuple String Value))  -- ^ The expected output
      -> Case
pass' opts i e o = { argv:     i
                   , env:      e
                   , options:  opts
                   , expected: Right $ StrMap.fromFoldable $ fromFoldable o
                   }

fail  :: Maybe Options -- ^ The options
      -> Array String  -- ^ ARGV
      -> String        -- ^ The expected error message
      -> Case
fail o i e = { argv: i, env: [],  expected: Left e, options: o }

infixr 0 Tuple as :>

data TestArgs = TestRequired | TestOptional | TestNone

argParserSpec = \_ -> describe "The parser generator" do
  -- Some options that will be used for these tests
  let
    testCases = ([
      test
        "usage: prog <qux>..."
        [ pass Nothing
            [ "a", "b", "c" ]
            [ "<qux>" :> V.array [ V.str "a", V.str "b", V.str "c" ]
            ]
        , fail
            Nothing
            [ "--foo", "baz" ]
            "unknown option --foo"
        , fail
            Nothing
            [ "a", "--foo", "-f=10" ]
            "unknown option --foo"
        ]

    , test
        "usage: prog <qux>... --"
        [ pass Nothing
            [ "a", "b", "c", "--" ]
            [ "<qux>" :> V.array [ V.str "a", V.str "b", V.str "c" ]
            , "--"    :> V.array []
            ]
        , pass Nothing
            [ "a", "b", "c", "--", "--", "--" ]
            [ "<qux>" :> V.array [ V.str "a", V.str "b", V.str "c" ]
            , "--"    :> V.array [ V.str "--" , V.str "--" ]
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
            [ "-h"     :> V.str "http://localhost:5000"
            , "--host" :> V.str "http://localhost:5000"
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
            [ "-h"     :> V.str "HOME"
            , "--host" :> V.str "HOME"
            ]
        ]

    , test
        """
        usage: prog -iFILE
        options:
          -i, --input FILE
        """
        [ --fail Nothing [] "missing -iFILE"
          pass Nothing
            [ "-i", "bar" ]
            [ "-i"      :> V.str "bar"
            , "--input" :> V.str "bar" ]
        ]

    , test
        """
        usage: prog -i FILE
        options:
          -i, --input FILE
        """
        [ --fail Nothing [] "missing -iFILE"
          pass Nothing
            [ "-i", "bar" ]
            [ "-i"      :> V.str "bar"
            , "--input" :> V.str "bar" ]
        ]

    , test
        """
        usage: prog (-iFILE)
        options:
          -i, --input FILE
        """
        [ --fail Nothing [] "missing -iFILE"
          pass Nothing
            [ "-i", "bar" ]
            [ "-i"      :> V.str "bar"
            , "--input" :> V.str "bar" ]
        ]

    , test
        """
        usage: prog (-iFILE) -oFILE
        options:
          -i, --input FILE
          -o, --output FILE
        """
        [ fail Nothing []
          $ "missing -iFILE"

        , fail Nothing [ "-i", "bar" ]
          $ "missing -oFILE"

        , pass Nothing
            [ "-i", "bar", "-o", "bar" ]
            [ "--input"  :> V.str "bar"
            , "-i"       :> V.str "bar"
            , "--output" :> V.str "bar"
            , "-o"       :> V.str "bar" ]

          -- group should be interchangable if it's only of options:
        , pass Nothing
            [ "-o", "bar", "-i", "bar" ]
            [ "--input"  :> V.str "bar"
            , "-i"       :> V.str "bar"
            , "--output" :> V.str "bar"
            , "-o"       :> V.str "bar" ]
        ]

    , test
        """
        usage: prog ((-iFILE) -rFILE) -oFILE
        options:
          -i, --input FILE
          -o, --output FILE
          -r, --redirect FILE [env: QUX]
        """
        [ fail Nothing []
          $ "missing -iFILE"

        , fail Nothing [ "-i", "bar", "-r", "bar" ]
            "missing -oFILE"

        , pass Nothing
            [ "-i", "bar", "-r", "bar", "-o", "bar" ]
            [ "--input"    :> V.str "bar"
            , "-i"         :> V.str "bar"
            , "--redirect" :> V.str "bar"
            , "-r"         :> V.str "bar"
            , "--output"   :> V.str "bar"
            , "-o"         :> V.str "bar" ]

          -- group should be interchangable if it's only of options:
        , pass Nothing
            [ "-o", "bar", "-r", "bar", "-i", "bar" ]
            [ "--input"    :> V.str "bar"
            , "-i"         :> V.str "bar"
            , "--redirect" :> V.str "bar"
            , "-r"         :> V.str "bar"
            , "--output"   :> V.str "bar"
            , "-o"         :> V.str "bar" ]

        , pass' Nothing
            [ "-o", "bar", "-i", "bar" ]
            [ "QUX" :> "BAR" ]
            [ "--input"    :> V.str "bar"
            , "-i"         :> V.str "bar"
            , "--redirect" :> V.str "BAR"
            , "-r"         :> V.str "BAR"
            , "--output"   :> V.str "bar"
            , "-o"         :> V.str "bar" ]
        ]

    , test
        """
        usage: prog ((-i FILE) <env>) -oFILE
        options:
          -i, --input FILE
          -o, --output FILE
          -r, --redirect FILE
        """
        [ fail Nothing [] "missing -iFILE"
          -- XXX: Would be cool to show the reason the group did not parse!
        , fail Nothing [ "-i", "bar" ] "missing <env>"
        , pass Nothing
            [ "-i", "bar", "x", "-o", "bar" ]
            [ "--input"  :> V.str "bar"
            , "-i"       :> V.str "bar"
            , "<env>"    :> V.str "x"
            , "--output" :> V.str "bar"
            , "-o"       :> V.str "bar" ]
          -- group should NOT be interchangable if it contains non-options:
        , fail Nothing [ "-o", "bar", "x", "-i", "bar" ]
            "expected (-iFILE <env>), but got -o"
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
            [  "--out" :> V.str "BAR"
            ,  "-o"    :> V.str "BAR" ]
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
            [  "--out" :> V.str "BAR"
            ,  "-o"    :> V.str "BAR" ]
        ]

    , test
        """
        usage: prog -a
           or: prog -b
        """
        [ pass Nothing
            [ "-a" ]
            [ "-a" :> V.bool true ]
        , pass Nothing
            [ "-b" ]
            [ "-b" :> V.bool true ]
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
            [ "a" :> V.bool true ]
        , pass Nothing
            [ "b" ]
            [ "b" :> V.bool true ]
        , fail Nothing
            [ "a", "b" ]
            "unexpected command b"
        , fail Nothing
            [ "b", "a" ]
            "unexpected command a"
        , fail Nothing [] ""
        ]

    , test
        """
        usage: prog foo -io [-q]... -b=BAZ (-f FOZ)... baz
        options:
          -i, --input
          -o, --out
          -q, --qux...
          -b, --baz BAZ [default: "ax"]
          -f, --foo FOZ...
        """
        [ pass Nothing
            [ "foo", "--out", "--input", "--qux", "--foo=ox", "baz" ]
            [ "foo"     :> V.bool true
            , "--out"   :> V.bool true
            , "-o"      :> V.bool true
            , "--input" :> V.bool true
            , "-i"      :> V.bool true
            , "--qux"   :> V.int 1
            , "-q"      :> V.int 1
            , "--foo"   :> V.array [ V.str "ox" ]
            , "-f"      :> V.array [ V.str "ox" ]
            , "baz"     :> V.bool true
            , "--baz"   :> V.str "ax"
            , "-b"      :> V.str "ax"
            ]

        , pass Nothing
            [ "foo" , "--out", "-qqq", "--foo=ox", "--baz=ax", "--input", "baz" ]
            [ "foo"     :> V.bool true
            , "--out"   :> V.bool true
            , "-o"      :> V.bool true
            , "--qux"   :> V.int 3
            , "-q"      :> V.int 3
            , "--foo"   :> V.array [ V.str "ox" ]
            , "-f"      :> V.array [ V.str "ox" ]
            , "--baz"   :> V.str "ax"
            , "-b"      :> V.str "ax"
            , "--input" :> V.bool true
            , "-i"      :> V.bool true
            , "baz"     :> V.bool true
            ]

        , pass Nothing
            [ "foo", "-q", "-o", "--qux", "-i", "--baz=ax", "-f=ox", "baz" ]
            [ "foo"     :> V.bool true
            , "--qux"   :> V.int 2
            , "-q"      :> V.int 2
            , "--out"   :> V.bool true
            , "-o"      :> V.bool true
            , "--input" :> V.bool true
            , "-i"      :> V.bool true
            , "--baz"   :> V.str "ax"
            , "-b"      :> V.str "ax"
            , "--foo"   :> V.array [ V.str "ox" ]
            , "-f"      :> V.array [ V.str "ox" ]
            , "baz"     :> V.bool true
            ]

        , pass Nothing
            [ "foo", "--baz=ax", "-o", "-f=ox", "-i", "baz" ]
            [ "foo"     :> V.bool true
            , "--baz"   :> V.str "ax"
            , "-b"      :> V.str "ax"
            , "--out"   :> V.bool true
            , "-o"      :> V.bool true
            , "--foo"   :> V.array [ V.str "ox" ]
            , "-f"      :> V.array [ V.str "ox" ]
            , "--input" :> V.bool true
            , "-i"      :> V.bool true
            , "baz"     :> V.bool true
            ]

        , pass Nothing
            [ "foo", "-o", "-i", "-bax", "-f=ox", "baz" ]
            [ "foo"     :> V.bool true
            , "--out"   :> V.bool true
            , "-o"      :> V.bool true
            , "--input" :> V.bool true
            , "-i"      :> V.bool true
            , "--baz"   :> V.str "ax"
            , "-b"      :> V.str "ax"
            , "baz"     :> V.bool true
            , "--baz"   :> V.str "ax"
            , "-b"      :> V.str "ax"
            , "--foo"   :> V.array [ V.str "ox" ]
            , "-f"      :> V.array [ V.str "ox" ]
            ]

        , fail Nothing
            [ "foo" ]
            "missing -fFOZ"
        , fail Nothing
            [ "foo", "-o", "-i", "-bax" ]
            "missing -fFOZ"
        ]

    , test
        """
        usage: prog [foo]
        """
        [ fail Nothing [ "goo" ] "unknown command goo" ]

    , test
        """
        usage: prog (foo)
        """
        [ fail Nothing [ "goo" ] "unknown command goo" ]

    , test
        """
        Usage: prog [options]
        options:
          -n
        """
        [ pass
            (Just (defaultOptions
                    { stopAt = [ "-n" ]
                    , optionsFirst = true
                    }))
            [ "-n", "-a", "-b", "-c" ]
            [ "-n" :> V.array [ V.str "-a",  V.str "-b",  V.str "-c" ] ]
        , pass
            (Just (defaultOptions
                    { stopAt = [ "-n" ]
                    , optionsFirst = true
                    }))
            [ "-n", "true", "false" ]
            [ "-n" :> V.array [ V.str "true",  V.str "false" ] ]
        ]

    , test
        """
        Usage: prog [options]
        options:
          -n...
        """
        [ pass
            (Just (defaultOptions
                    { stopAt = [ "-n" ]
                    , optionsFirst = true
                    }))
            [ "-n", "-a", "-b", "-c" ]
            [ "-n" :> V.array [ V.str "-a",  V.str "-b",  V.str "-c" ] ]
        , pass
            (Just (defaultOptions
                    { stopAt = [ "-n" ]
                    , optionsFirst = true
                    }))
            [ "-n", "true", "false" ]
            [ "-n" :> V.array [ V.str "true",  V.str "false" ] ]
        ]

    -- , test
    --     """
    --     Usage: prog [options]
    --     options:
    --       -n[=FOO]
    --     """
    --     [ pass
    --         (Just (defaultOptions
    --                 { stopAt = [ "-n" ]
    --                 , optionsFirst = true
    --                 }))
    --         [ "-n", "-a", "-b", "-c" ]
    --         [ "-n" :> V.array [ V.str "-a",  V.str "-b",  V.str "-c" ] ]
    --     , pass
    --         (Just (defaultOptions
    --                 { stopAt = [ "-n" ]
    --                 , optionsFirst = true
    --                 }))
    --         [ "-n", "true", "false" ]
    --         [ "-n" :> V.array [ V.str "true",  V.str "false" ] ]
    --     ]
    --
    -- , test
    --     """
    --     Usage: prog [options]
    --     options:
    --       -n[=FOO]...
    --     """
    --     [ pass
    --         (Just (defaultOptions
    --                 { stopAt = [ "-n" ]
    --                 , optionsFirst = true
    --                 }))
    --         [ "-n", "-a", "-b", "-c" ]
    --         [ "-n" :> V.array [ V.str "-a",  V.str "-b",  V.str "-c" ] ]
    --     , pass
    --         (Just (defaultOptions
    --                 { stopAt = [ "-n" ]
    --                 , optionsFirst = true
    --                 }))
    --         [ "-n", "true", "false" ]
    --         [ "-n" :> V.array [ V.str "true",  V.str "false" ] ]
    --     ]
    --
    -- , test
    --     """
    --     Usage: prog [options]
    --     options:
    --       -n, --noop
    --     """
    --     [ pass
    --         (Just (defaultOptions
    --                 { stopAt = [ "-n" ]
    --                 , optionsFirst = true
    --                 }))
    --         [ "-n", "-a", "-b", "-c" ]
    --         [ "-n"     :> V.array [ V.str "-a",  V.str "-b",  V.str "-c" ]
    --         , "--noop" :> V.array [ V.str "-a",  V.str "-b",  V.str "-c" ] ]
    --     , pass
    --         (Just (defaultOptions
    --                 { stopAt = [ "--noop" ]
    --                 , optionsFirst = true
    --                 }))
    --         [ "-n", "-a", "-b", "-c" ]
    --         [ "-n"     :> V.array [ V.str "-a",  V.str "-b",  V.str "-c" ]
    --         , "--noop" :> V.array [ V.str "-a",  V.str "-b",  V.str "-c" ] ]
    --     , pass
    --         (Just (defaultOptions
    --                 { stopAt = [ "-n", "--noop" ]
    --                 , optionsFirst = true
    --                 }))
    --         [ "-n", "-a", "-b", "-c" ]
    --         [ "-n"     :> V.array [ V.str "-a",  V.str "-b",  V.str "-c" ]
    --         , "--noop" :> V.array [ V.str "-a",  V.str "-b",  V.str "-c" ] ]
    --     ]
    --
    -- , test
    --     """
    --     Usage: prog [options]
    --     options:
    --       -n ARC
    --     """
    --     [ pass
    --         (Just (defaultOptions
    --                 { stopAt = [ "-n" ]
    --                 , optionsFirst = true
    --                 }))
    --         [ "-n", "-a", "-b", "-c" ]
    --         [ "-n" :> V.array [ V.str "-a",  V.str "-b",  V.str "-c" ] ]
    --     ]
    --
    -- , test
    --     """
    --     Usage: prog ((((foo|bar)|qux)|wux)|-n ARC) ARGS...
    --     options:
    --       -n ARC
    --     """
    --     [ pass
    --         (Just (defaultOptions
    --                 { stopAt = [ "-n" ]
    --                 , optionsFirst = true
    --                 }))
    --         [ "-n", "-a", "-b", "-c" ]
    --         [ "-n" :> V.array [ V.str "-a",  V.str "-b",  V.str "-c" ] ]
    --     ]
    --
    -- , test
    --     """
    --     Usage: prog [-n ARG]
    --     """
    --     [ pass
    --         (Just (defaultOptions
    --                 { stopAt       = [ "-n" ]
    --                 , optionsFirst = true
    --                 , smartOptions = false
    --                 }))
    --         [ "-n", "-a", "-b", "-c" ]
    --         [ "-n" :> V.array [ V.str "-a",  V.str "-b",  V.str "-c" ] ]
    --     ]
    --
    -- , test
    --     """
    --     Usage: prog ((((foo|bar)|-n ARC)|wux))
    --     options:
    --       -n ARC
    --     """
    --     [ pass
    --         (Just (defaultOptions
    --                 { stopAt = [ "-n" ]
    --                 , optionsFirst = true
    --                 }))
    --         [ "-n", "-a", "-b", "-c" ]
    --         [ "-n" :> V.array [ V.str "-a",  V.str "-b",  V.str "-c" ] ]
    --     ]
    --
    -- , test
    --     """
    --     Usage: prog [-i] [-q]...
    --     """
    --     [ pass
    --         Nothing
    --         [ "-q", "-i", "-q" ]
    --         [ "-i" :> V.bool true
    --         , "-q" :> V.int 2 ]
    --     , pass
    --         Nothing
    --         [ "-i", "-q", "-q" ]
    --         [ "-i" :> V.bool true
    --         , "-q" :> V.int 2 ]
    --     , pass
    --         Nothing
    --         [ "-q", "-q", "-i" ]
    --         [ "-i" :> V.bool true
    --         , "-q" :> V.int 2 ]
    --     ]
    --
    -- , test
    --     """
    --     usage: prog (-a | -b)... (-d | -e)...
    --     """
    --     [ pass
    --         Nothing
    --         [ "-a", "-d" ]
    --         [ "-a" :> V.int 1
    --         , "-d" :> V.int 1 ]
    --     , pass
    --         Nothing
    --         [ "-a", "-a", "-d" ]
    --         [ "-a" :> V.int 2
    --         , "-d" :> V.int 1 ]
    --     , pass
    --         Nothing
    --         [ "-a", "-a", "-d", "-d" ]
    --         [ "-a" :> V.int 2
    --         , "-d" :> V.int 2 ]
    --     , pass
    --         Nothing
    --         [ "-a", "-d", "-a", "-a", "-d", "-a" ]
    --         [ "-a" :> V.int 4
    --         , "-d" :> V.int 2 ]
    --     , pass
    --         Nothing
    --         [ "-a", "-b" ]
    --         [ "-a" :> V.int 1
    --         , "-b" :> V.int 1 ]
    --     ]
    --
    -- , test
    --     """
    --     usage: prog foo --foo... --bar...
    --        or: prog <env>...
    --     """
    --     [ pass
    --         Nothing
    --         [ "100", "200" ]
    --         [ "<env>" :> V.array [ V.int 100, V.int 200 ] ]
    --     , pass
    --         Nothing
    --         [ "foo", "--foo", "--foo" ]
    --         [ "foo"   :> V.bool true
    --         , "--foo" :> V.int 2 ]
    --     , pass
    --         Nothing
    --         [ "foo", "--foo", "--bar", "--foo" ]
    --         [ "foo"   :> V.bool true
    --         , "--foo" :> V.int 2
    --         , "--bar" :> V.int 1 ]
    --     ]
    --
    -- , test
    --     """
    --     usage: prog [<foo> <bar>]
    --        or: prog foo
    --     """
    --     [ pass
    --       Nothing
    --       [ "foo", "bar" ]
    --       [ "<foo>" :> V.str "foo"
    --       , "<bar>" :> V.str "bar" ]
    --     , pass
    --       Nothing
    --       []
    --       []
    --     , pass
    --       Nothing
    --       [ "foo" ]
    --       [ "foo" :> V.bool true ]
    --     ]
    --
    -- , test
    --     """
    --     usage: prog move --speed=<kn>
    --     """
    --     [ pass
    --       (Just (defaultOptions {
    --         stopAt = [ "--speed" ]
    --       , laxPlacement = true
    --       }))
    --       [ "--speed", "10"  ]
    --       [ "--speed" :> V.array [ V.str "10" ] ]
    --     ]
    --
    -- , test
    --     """
    --     usage: prog move [--speed=<kn>]
    --     """
    --     [ pass
    --       (Just (defaultOptions {
    --         stopAt = [ "--speed" ]
    --       , laxPlacement = true
    --       }))
    --       [ "--speed", "10"  ]
    --       [ "--speed" :> V.array [ V.str "10" ] ]
    --     ]
    --
    -- , test
    --     """
    --     usage: prog move [[[[[[--speed=<kn>]]]]]]
    --     """
    --     [ pass
    --       (Just (defaultOptions {
    --         stopAt = [ "--speed" ]
    --       , laxPlacement = true
    --       }))
    --       [ "--speed", "10"  ]
    --       [ "--speed" :> V.array [ V.str "10" ] ]
    --     ]
    --   ]
    --
    --   -- stop-at tests in various forms
    --   <> (A.concat $ [ TestRequired, TestOptional, TestNone ] <#> \scenario ->
    --     [ test
    --       (case scenario of
    --         TestRequired -> "Usage: prog [-iofx=FILE]"
    --         TestOptional -> "Usage: prog [-iofx[=FILE]]"
    --         TestNone     -> "Usage: prog [-iofx]"
    --       )
    --      ([ pass
    --           (Just (defaultOptions { stopAt = [ "-x" ] }))
    --           [ "-ifx" ,"foo", "-i" ]
    --           [ "-i" :> V.bool true
    --           , "-f" :> V.bool true
    --           , "-x" :> V.array [ V.str "foo",  V.str "-i"  ] ]
    --
    --       , pass
    --           (Just (defaultOptions { stopAt = [ "-x" ] }))
    --           [ "-i", "-f", "-x" ]
    --           [ "-i" :> V.bool true
    --           , "-f" :> V.bool true
    --           , "-x" :> V.array [] ]
    --       ]
    --       <>
    --       (case scenario of
    --         TestNone ->
    --           [ fail
    --               (Just (defaultOptions { stopAt = [ "-x" ] }))
    --               [ "-i", "-f", "-x=foo", "-i" ]
    --               "option takes no argument: -x"
    --
    --           , fail
    --               (Just (defaultOptions { stopAt = [ "-x" ] }))
    --               [ "-ifx=foo", "-i" ]
    --               "option takes no argument: -x"
    --
    --           , fail
    --               (Just (defaultOptions { stopAt = [ "-x" ] }))
    --               [ "-ifxy=foo", "-i" ]
    --               "unknown option -y=foo"
    --
    --           , fail
    --               (Just (defaultOptions { stopAt = [ "-x" ] }))
    --               [ "-i", "-f", "-xoz" ]
    --               "unknown option -z"
    --
    --           , fail
    --               (Just (defaultOptions { stopAt = [ "-x" ] }))
    --               [ "-i", "-f", "-oxzfoo", "-i" ]
    --              "unknown option -zfoo"
    --           ]
    --         otherwise ->
    --           [ pass
    --               (Just (defaultOptions { stopAt = [ "-x" ] }))
    --               [ "-i", "-f", "-x=foo", "-i" ]
    --               [ "-i" :> V.bool true
    --               , "-f" :> V.bool true
    --               , "-x" :> V.array [ V.str "foo",  V.str "-i"  ] ]
    --
    --           , pass
    --               (Just (defaultOptions { stopAt = [ "-x" ] }))
    --               [ "-ifx=foo", "-i" ]
    --               [ "-i" :> V.bool true
    --               , "-f" :> V.bool true
    --               , "-x" :> V.array [ V.str "foo",  V.str "-i"  ] ]
    --
    --           , pass
    --               (Just (defaultOptions { stopAt = [ "-x" ] }))
    --               [ "-ifxy=foo", "-i" ]
    --               [ "-i" :> V.bool true
    --               , "-f" :> V.bool true
    --               , "-x" :> V.array [ V.str "y=foo",  V.str "-i"  ] ]
    --
    --           , pass
    --               (Just (defaultOptions { stopAt = [ "-x" ] }))
    --               [ "-i", "-f", "-xoz" ]
    --               [ "-i" :> V.bool true
    --               , "-f" :> V.bool true
    --               , "-x" :> V.array [ V.str "oz" ] ]
    --
    --           , pass
    --               (Just (defaultOptions { stopAt = [ "-x" ] }))
    --               [ "-i", "-f", "-oxzfoo", "-i" ]
    --               [ "-i" :> V.bool true
    --               , "-f" :> V.bool true
    --               , "-o" :> V.bool true
    --               , "-x" :> V.array [ V.str "zfoo",  V.str "-i"  ] ]
    --
    --           , pass
    --               (Just (defaultOptions { stopAt = [ "-x" ] }))
    --               [ "-i", "-f", "-oxfoo", "-i" ]
    --               [ "-i" :> V.bool true
    --               , "-f" :> V.bool true
    --               , "-o" :> V.bool true
    --               , "-x" :> V.array [ V.str "foo",  V.str "-i"  ] ]
    --           ]
    --       ))
    --
    --     -- Test 'stopAt' on long-options
    --     , test
    --         (case scenario of
    --           TestRequired -> "Usage: prog --foo=BAR"
    --           TestOptional -> "Usage: prog --foo[=BAR]"
    --           TestNone     -> "Usage: prog --foo"
    --         )
    --         ([ pass
    --               (Just (defaultOptions { stopAt = [ "--foo" ] }))
    --               [ "--foo" ]
    --               [ "--foo" :> V.array [] ]
    --           , pass
    --               (Just (defaultOptions { stopAt = [ "--foo" ] }))
    --               [ "--foo", "-f", "-o", "-x" ]
    --               [ "--foo" :> V.array [ V.str "-f", V.str "-o", V.str "-x" ] ]
    --           ] <>
    --           (case scenario of
    --             TestNone ->
    --               [ fail
    --                   (Just (defaultOptions { stopAt = [ "--foo" ] }))
    --                   [ "--foo=BAR", "-f"]
    --                   "option takes no argument: --foo"
    --               , fail
    --                   (Just (defaultOptions { stopAt = [ "--foo" ] }))
    --                   [ "--fooBAR", "-f"]
    --                   "unknown option --fooBAR"
    --               ]
    --             otherwise ->
    --               [ pass
    --                   (Just (defaultOptions { stopAt = [ "--foo" ] }))
    --                   [ "--foo=BAR", "-f"]
    --                   [ "--foo" :> V.array [ V.str "BAR", V.str "-f" ] ]
    --               , pass
    --                   (Just (defaultOptions { stopAt = [ "--foo" ] }))
    --                   [ "--fooBAR", "-f"]
    --                   [ "--foo" :> V.array [ V.str "BAR", V.str "-f" ] ]
    --               ]
    --           ))
    --     ])
    --
    --   <>
    --     -- issue #70 - Ignore ANSI escape codes
    --     [ test ("${h} prog foo" <~> { h: Chalk.blue "Usage:" })
    --         [ pass Nothing ["foo"] ["foo" :> V.bool true ] ]
    --     , test (
    --         """
    --         ${h}
    --           prog foo
    --         """ <~> { h: Chalk.blue "Usage:" })
    --         [ pass Nothing ["foo"] ["foo" :> V.bool true ] ]
    --
    --     -- Ignore ANSI escape codes anywhere:
    --     , test ("${h}: prog foo" <~> { h: Chalk.blue "Usage" })
    --         [ pass Nothing ["foo"] ["foo" :> V.bool true ] ]
    --     , test (
    --         """
    --         ${h}:
    --           prog foo
    --         """ <~> { h: Chalk.blue "Usage" })
    --         [ pass Nothing ["foo"] ["foo" :> V.bool true ] ]
    ])

  for_ testCases \(({ help, cases, skip })) -> do
    when (not skip) $ describe help do
      for_ cases \(({ argv, env, expected, options })) ->
        let msg = either (\e -> "Should fail with \"" <> e <> "\"")
                          pretty
                          expected
            envmsg = intercalate " " $ (\(a /\ b) -> a <> "=" <> b) <$> env
            inpmsg = if not (A.null argv)
                        then intercalate " " argv
                        else "(no input)"
            help' = dedent help
         in it ("\n~$ " <> envmsg <> " " <> inpmsg <> "\n== " <> msg) $ vliftEff do
            spec <- runEitherEff do
              -- scan the input text
              { originalUsage, usage, options } <- Error.capture do
                Scanner.scan help'

              -- lex/parse the usage section
              { program, layouts } <- do
                toks <- Error.capture $ Lexer.lexUsage usage
                Error.capture $ Spec.parseUsage toks

              -- lex/parse the description section(s)
              descriptions <- concat <$> for options \description -> do
                toks <- Error.capture $ Lexer.lexDescs description
                Error.capture $ Spec.parseDescription toks

              Error.capture do
                Solver.solve
                  { smartOptions: false }
                  (Spec { program
                        , layouts
                        , descriptions
                        , helpText: help'
                        , shortHelp: originalUsage
                        })

            validate spec argv (Env.fromFoldable env) options expected

    where

      prettyPrintStrMap :: StrMap Value -> String
      prettyPrintStrMap m = intercalate "\n\t" $
        StrMap.toList m <#> \(Tuple arg val) ->
          arg <> " => " <> prettyPrintValue val

      validate
        :: ∀ eff
         . Neodoc.Spec SolvedLayout
        -> Array String
        -> Env
        -> Maybe Options
        -> Either String (StrMap Value)
        -> Eff (err :: EXCEPTION | eff) Unit
      validate (spec@(Spec { descriptions })) argv env mOptions expected =
        let opts = fromMaybe defaultOptions mOptions
            result = do
              ArgParseResult mBranch vs <- do
                lmap pretty $ ArgParser.run spec {
                    optionsFirst:      opts.optionsFirst
                  , stopAt:            opts.stopAt
                  , requireFlags:      opts.requireFlags
                  , laxPlacement:      opts.laxPlacement
                  , repeatableOptions: opts.repeatableOptions
                  , allowUnknown:      opts.allowUnknown
                  , helpFlags:         Nil
                  , versionFlags:      Nil
                  } env argv
              pure $ Evaluate.reduce env descriptions mBranch vs
         in case result of
            Left msg {- XXX: Check against `ArgParserError`? -} ->
              either
                (\e' ->
                  if (msg /= e')
                    then throwException $ error $
                      "Unexpected error:\n" <> msg
                    else pure unit)
                (const $ throwException $ error $ msg)
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
                      <> pretty r)
                (\r' ->
                  if (r /= r')
                    then throwException $ error $
                      "Unexpected output:\n"
                        <> pretty r
                    else pure unit)
                expected

getSolveErrorMessage :: SolveError -> String
getSolveErrorMessage (SolveError s) = s
