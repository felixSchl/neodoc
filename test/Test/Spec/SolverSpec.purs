module Test.Spec.SolverSpec (solverSpec) where

import Prelude
import Debug.Trace
import Data.Either (Either(..), either)
import Control.Bind ((=<<))
import Control.Apply ((*>))
import Data.List (List(..), toList)
import Control.Plus (empty)
import Data.Foldable (intercalate, for_)
import Data.Traversable (for)
import Control.Monad.Eff.Exception (error, throwException)
import qualified Data.Array as A
import Data.Maybe (Maybe(..), maybe)
import Data.String (fromChar)

import Test.Assert (assert)
import Test.Spec (describe, it)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Assert.Simple

import Test.Support (vliftEff, runEitherEff)
import Test.Support.Usage as U
import Test.Support.Desc as DE
import Test.Support.Docopt as D
import Test.Support.Arguments

import Language.Docopt (preparseDocopt)
import Language.Docopt.Errors
import Language.Docopt.Argument
import Language.Docopt.Value
import Language.Docopt.Usage
import Language.Docopt.Solver (solve)
import Language.Docopt.Parser.Desc (Desc())
import Language.Docopt.Argument as D
import Test.Support.Docopt      as D
import Language.Docopt.Parser.Usage.Argument as U
import Language.Docopt.Parser.Usage          as U
import Language.Docopt.Parser.Desc           as DE
import Language.Docopt.Scanner (scan)
import Language.Docopt.Parser.Lexer (lex)
import Text.Wrap (dedent)

newtype TestSuite = TestSuite { help  :: String
                              , cases :: Array TestCase
                              }
newtype TestCase = TestCase { expected :: Either String (Array (Array Argument))
                            , desctext :: String
                            }

test :: String -> Array TestCase -> TestSuite
test help cs = TestSuite { help: help, cases: cs }

pass :: String -> Array (Array Argument) -> TestCase
pass text as = TestCase { expected: Right as, desctext: text  }

fail :: String -> String -> TestCase
fail text msg = TestCase { expected: Left msg, desctext: text }

solverSpec = \_ ->
  describe "The solver" do
    for_ (toList [

      test "Usage: prog foo"
        [ pass "" [ [ co "foo" ] ] ]

    , test "Usage: prog <prog>..."
        [ pass "" [ [ poR "<prog>" ] ] ]

    , test
        "Usage: prog --foo..."
        [ fail
            "options: -f --foo=bar [default: qux]"
            "Option-Argument specified in options-section missing --foo"
        ]

    , test
        "Usage: prog --foo... BAR"
        [ fail
            "options: -f --foo=BAR [default: qux]"
            "Option-Argument specified in options-section missing --foo"
        ]

    , test
        "Usage: prog -f... BAR"
        [ fail
            "options: -f --foo=BAR [default: qux]"
            "Option-Argument specified in options-section missing -f"
        ]

    , test
        "Usage: prog --foo BAR..."
        [ pass
            "options: -f --foo=BAR [default: qux]"
            [ [ optR 'f' "foo" (oa "BAR" (D.str "qux")) ] ]
        ]

    , test
        "Usage: prog --foo BAR"
        [ pass
            "options: -f --foo=BAR [default: qux]"
            [ [ opt 'f' "foo" (oa "BAR" (D.str "qux")) ] ]
        ]

    , test
        "Usage: prog --foo... BAR..."
        [ fail
            "options: -f --foo=BAR [default: qux]"
            "Option-Argument specified in options-section missing --foo"
        ]

    , test
        "Usage: prog -f... BAR..."
        [ fail
            "options: -f --foo=BAR [default: qux]"
            "Option-Argument specified in options-section missing -f"
        ]

    , test
        "Usage: prog -xvzfFILE..."
        [ pass
            "options: -f --file=FILE  [default: foo]"
            [ [ soptR_ 'x'
              , soptR_ 'v'
              , soptR_ 'z'
              , optR 'f' "file" (oa "FILE" (D.str "foo"))
            ] ]
        ]

    , test
        "Usage: prog -xvzf FILE..."
        [ pass
            "options: -f --file=FILE  [default: foo]"
            [ [ soptR_ 'x'
              , soptR_ 'v'
              , soptR_ 'z'
              , optR 'f' "file" (oa "FILE" (D.str "foo"))
            ] ]
        ]

    , test
        "Usage: prog --file FILE..."
        [ fail
            """
            options:
              -f --file=FILE  [default: foo]
              -f --file=FILE  [default: foo]
            """
            "Multiple option descriptions for option --file"
        ]

    , test
        "Usage: prog -f FILE..."
        [ fail
            """
            options:
              -f --file=FILE  [default: foo]
              -f --file=FILE  [default: foo]
            """
            "Multiple option descriptions for option -f"
        ]

    , test
        "Usage: prog --file FILE..."
        [ fail
            """
            options:
              -f --file=FILE  [default: foo]
              -f --file=FILE  [default: foo]
            """
            "Multiple option descriptions for option --file"
        ]

    , test
        "Usage: prog -fx..."
        [ fail
            "options: -f --file=FILE  [default: foo]"
            "Stacked option -f may not specify arguments"
        ]

      -- Note: `f` should not adopt `file` as it's full name since it's in an
      -- option stack and not in trailing position (therefore cannot inherit the
      -- description's argument, rendering it an unfit candidate)
    , test
        "Usage: prog -fvzx..."
        [ fail
            "options: -f --file=FILE  [default: foo]"
            "Stacked option -f may not specify arguments"
        ]

    , test
        "Usage: prog -xvzf FILE..."
        [ pass
            "options: -f --file=FILE  [default: foo]"
            [ [ soptR_ 'x'
            , soptR_ 'v'
            , soptR_ 'z'
            , optR 'f' "file" (oa "FILE" (StringValue "foo"))
            ] ]
        ]

    , test
        "Usage: prog -xvzf..."
        [ fail
            "options: -f --file=FILE  [default: foo]"
            "Option-Argument specified in options-section missing -f"
        ]

    , test
        "Usage: prog [options]"
        [ pass
            "options: -f --file=FILE [default: foo]"
            [ [ gro false [ [
                  opt 'f' "file" (oa "FILE" $ StringValue "foo")
            ] ] ] ]

        -- newline treatment for option alias in the description:
        -- the aliases may be separated by commas, in which case the alias may
        -- appear on a new line. If the comma is ommitted, the association won't
        -- take place
        , pass
            """
            options:
              -f,
              --foo
            """
            [ [ gro false [ [
                  opt_ 'f' "foo"
            ] ] ] ]

        , pass
            """
            options:
              -f
              --foo
            """
            [ [ gro false [ [ sopt_ 'f'   ] ]
              , gro false [ [ lopt_ "foo" ] ]
            ] ]

        , pass
            """
            options:
              --foo
                  ARG
                  ...
              -q
                  ARG
                  ...
            """
            [ [ gro false [ [ lopt_ "foo" ] ]
              , gro false [ [ sopt_ 'q'   ] ]
            ] ]
        ]

    ]) runtest

  where

    prettyPrintOutput :: List Usage -> String
    prettyPrintOutput as =
      intercalate "\n" (("  " ++ ) <$>
        (prettyPrintUsage <$> as))

    runtest (TestSuite { help, cases }) = do
      for_ cases \(TestCase { expected, desctext }) -> do
        describe ("\n" <> help <> "\n" <> dedent desctext) do
          it (
            either
              (\msg  -> "Should fail with:\n" ++ msg)
              (\args -> "Should resolve to:\n"
                  <> (intercalate "\n" (
                      ("Usage: prog " <> _)
                        <$> (intercalate " " <<< (prettyPrintArg <$> _))
                              <$> args)))
              expected
          ) do
            vliftEff do
              { usages, descriptions } <- runEitherEff do
                (preparseDocopt
                  (help <> "\n" <> dedent desctext)
                  { smartOptions: false })
              evaltest
                (solve usages descriptions)
                (expected)

    evaltest (Right output) (Right expected)
      = if output == (pure $ Usage $ toList $ toList <$> expected)
            then return unit
            else throwException $ error $
              "Unexpected output:\n\n" <> prettyPrintOutput output

    evaltest (Right output) (Left _)
      = throwException $ error $
          "Missing exception! Got:\n\n" <> prettyPrintOutput output

    evaltest (Left (SolveError err)) (Left expected)
      = if err == expected
            then return unit
            else throwException $ error $
              "Unexpected error:\n\n" <> err

    evaltest (Left err) _ = throwException $ error $ show err
