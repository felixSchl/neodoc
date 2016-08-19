module Test.Spec.SolverSpec (solverSpec) where

import Prelude
import Debug.Trace
import Data.Either (Either(..), either, fromRight)
import Control.Bind ((=<<))
import Control.Apply ((*>))
import Data.List (List(..), fromFoldable)
import Data.Bifunctor (lmap, rmap)
import Control.Plus (empty)
import Data.Foldable (intercalate, for_)
import Data.Traversable (for)
import Control.Monad.Eff.Exception (error, throwException)
import Data.Array as A
import Data.Maybe (Maybe(..), maybe)

import Test.Assert (assert)
import Test.Spec (describe, it)
import Test.Spec.Reporter.Console (consoleReporter)

import Test.Support (vliftEff, runEitherEff)
import Test.Support.Usage as U
import Test.Support.Desc as DE
import Test.Support.Docopt as D
import Test.Support.Arguments

import Language.Docopt (preparseDocopt)
import Language.Docopt.Errors
import Language.Docopt.Specification
import Language.Docopt.Argument
import Language.Docopt.Value
import Language.Docopt.Usage
import Language.Docopt.Solver (solve)
import Language.Docopt.SpecParser.Desc (Desc())
import Language.Docopt.Argument as D
import Test.Support.Docopt      as D
import Language.Docopt.SpecParser.Usage.Argument as U
import Language.Docopt.SpecParser.Usage          as U
import Language.Docopt.SpecParser.Desc           as DE
import Language.Docopt.Scanner (scan)
import Language.Docopt.SpecParser.Lexer (lex)
import Text.Wrap (dedent)
import Partial.Unsafe

data Expected = Source String | Parsed (Array (Array Argument))

newtype TestSuite = TestSuite { help  :: String
                              , cases :: Array TestCase
                              }
newtype TestCase = TestCase { expected :: Either String Expected
                            , desctext :: String
                            }

test :: String -> Array TestCase -> TestSuite
test help cs = TestSuite { help: help, cases: cs }

pass :: String -> Array (Array Argument) -> TestCase
pass text as = TestCase { expected: Right (Parsed as), desctext: text  }

pass' :: String -> String -> TestCase
pass' text s = TestCase { expected: Right (Source s), desctext: text  }

fail :: String -> String -> TestCase
fail text msg = TestCase { expected: Left msg, desctext: text }

solverSpec = \_ ->
  describe "The solver" do
    for_ (fromFoldable [

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

    , test
        "Usage: prog [options] [-a]"
        [ pass
            "options: -a, --all"
            [ [ gro false [ [
                  opt_ 'a' "all"
            ] ] ] ]
        ]

      -- Canonicalisation / simplification

    , test
        "Usage: prog [-a]"
        [ pass' "" "Usage: prog [-a]" ]

    , test
        "Usage: prog [[-a]]"
        [ pass' "" "Usage: prog [-a]" ]

    , test
        "Usage: prog [[[-a]]]"
        [ pass' "" "Usage: prog [-a]" ]

    , test
        "Usage: prog [[[-a|-b]]]"
        [ pass' "" "Usage: prog [-a|-b]" ]

    , test
        "Usage: prog [[[-a|[-b]]]]"
        [ pass' "" "Usage: prog [-a|-b]" ]

    , test
        "Usage: prog [[[-a|[-b]]]]"
        [ pass' "" "Usage: prog [-a|-b]" ]

    , test
        "Usage: prog [<name> [<name>]]"
        [ pass' "" "Usage: prog [<name> [<name>]]" ]

    , test
        "Usage: prog [[-a=FOO] | [[-a=FOO] [-b=FOO]]]"
        [ pass' "" "Usage: prog [-a=FOO | [[-a=FOO] [-b=FOO]]]" ]

    , test
        "Usage: prog ((-i=FILE) -o=FILE)"
        [ pass' "" "Usage: prog (-i=FILE -o=FILE)" ]

    , test
        "Usage: prog (-i=FILE) | (-o=FILE)"
        [ pass' "" "Usage: prog -i=FILE | -o=FILE" ]

    ]) runtest

  where

    prettyPrintOutput :: Specification -> String
    prettyPrintOutput as =
      intercalate "\n" (("  " <> _) <$>
        (prettyPrintUsage <$> as))

    runtest (TestSuite { help, cases }) = do
      for_ cases \(TestCase { expected, desctext }) -> do
        describe ("\n" <> help <> "\n" <> dedent desctext) do
          it (
            either
              (\msg      -> "Should fail with:\n" <> msg)
              (\expected ->
                let msg = case expected of
                      Source s  -> s
                      Parsed xs -> intercalate "\n" $
                        ("Usage: prog " <> _)
                          <$> (intercalate " " <<< (prettyPrintArg <$> _))
                                <$> xs
                 in "Should resolve to:\n" <> msg)
              expected
          ) do
            vliftEff do
              { usages, descriptions } <- runEitherEff do
                preparseDocopt
                  (help <> "\n" <> dedent desctext)
                  { smartOptions: false }
              evaltest
                (solve usages descriptions)
                (rmap (case _ of
                  Parsed xs -> Parsed xs
                  Source  s -> Source $ s <> "\n" <> dedent desctext)
                expected)

    evaltest (Right output) (Right expected) = do
      expected' <- case expected of
        Parsed xs   -> pure $ pure $ fromFoldable $ fromFoldable <$> xs
        Source help -> do
          runEitherEff do
            { usages, descriptions } <- preparseDocopt help {
              smartOptions: false
            }
            lmap show (solve usages descriptions)
      if output == expected'
          then pure unit
          else throwException $ error $
            "Unexpected output:\n\n" <> prettyPrintOutput output

    evaltest (Right output) (Left _)
      = throwException $ error $
          "Missing exception! Got:\n\n" <> prettyPrintOutput output

    evaltest (Left (SolveError err)) (Left expected)
      = if err == expected
            then pure unit
            else throwException $ error $
              "Unexpected error:\n\n" <> err

    evaltest (Left err) _ = throwException $ error $ show err
