module Test.Spec.SolveSpec (solveSpec) where

import Prelude
import Debug.Trace
import Data.Either (Either(..), either, fromRight)
import Data.Pretty (pretty)
import Data.Tuple.Nested ((/\))
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
import Data.NonEmpty ((:|))
import Data.String as S
import Text.Parsing.Parser (ParseError(..)) as P

import Test.Assert (assert)
import Test.Spec (describe, it)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Support (vliftEff, runEitherEff)

import Neodoc.Value
import Neodoc.Error.Class (capture) as Error
import Neodoc.Data.UsageLayout
import Neodoc.OptionAlias as OptionAlias
import Neodoc.Data.SolvedLayout
import Neodoc.Data.SolvedLayout as Solved
import Neodoc.Data.UsageLayout as Usage
import Neodoc.Spec (Spec (..))
import Neodoc.Spec.Lexer as Lexer
import Neodoc.Spec.Parser as SpecParser
import Neodoc.Spec.Parser.Usage as U
import Neodoc.Spec.Parser.Description as D
import Neodoc.Solve (solve)
import Neodoc.Solve.ExpandOptions (ExpandedOptionsLayoutArg(..), ExpandedOptionsLayout)
import Neodoc.Solve.Error (SolveError(..))
import Text.Wrap (dedent)
import Partial.Unsafe

-- | Since the solver only expands arguments, it's expanded version is valid
-- | usage input.
-- | For example: `-abc` expands to `-a -b -c`, which is valid input.
usageToSolved :: Partial => UsageLayout -> SolvedLayout
usageToSolved layout = layout <#> case _ of
  Usage.Command     n r        -> Solved.Command    n r
  Usage.Positional  n r        -> Solved.Positional n r
  Usage.Option      n a r      -> Solved.Option (OptionAlias.Long n) a r
  Usage.OptionStack (c:|_) a r -> Solved.Option (OptionAlias.Short c) a r
  Usage.EOA                    -> Solved.EOA
  Usage.Stdin                  -> Solved.Stdin

type Error = String
type Result = String
type UsageText = String
type DescriptionText = String
data TestSuite = TestSuite Boolean {- smart options? -} UsageText (Array TestCase)
data TestCase = TestCase DescriptionText (Either Error Result)

test = TestSuite false
testSmartOpts = TestSuite true

pass description expected = TestCase description (Right expected)
fail description err      = TestCase description (Left  err)

solveSpec = \_ ->
  describe "The solver" do
    for_ (fromFoldable [

      -- should not affect positionals and commands:
      test "prog foo"       [ pass "" "prog foo" ]
    , test "prog <prog>..." [ pass "" "prog <prog>..." ]

      -- options:
    , test "prog -io"
        [ pass "" "prog -i -o" ]

    , test "prog -io [-q]..."
        [ pass "" "prog -i -o [-q]..." ]

    , test "prog --foo..."
        [ fail "-f --foo=bar [default: qux]"
            "Option-Argument specified in options-section missing --foo" ]

    , test "prog --foo... BAR"
        [ fail "-f --foo=BAR [default: qux]"
            "Option-Argument specified in options-section missing --foo" ]

    , test "prog -f... BAR"
        [ fail "-f --foo=BAR [default: qux]"
            "Option-Argument specified in options-section missing -f" ]

    , test "prog --foo BAR..."
        [ pass "-f --foo=BAR [default: qux]" "prog --foo=BAR..."
        , pass "-f --foo" "prog --foo BAR..." ]

    , test "prog --foo BAR"
        [ pass "-f --foo=BAR [default: qux]" "prog --foo=BAR"
        , pass "-f --foo" "prog --foo BAR" ]

    , test "prog --foo... BAR..."
        [ fail "-f --foo=BAR [default: qux]"
            "Option-Argument specified in options-section missing --foo" ]

    , test "prog -f... BAR..."
        [ fail "-f --foo=BAR [default: qux]"
            "Option-Argument specified in options-section missing -f" ]

    , test "prog -xvzfFILE..."
        [ pass "-f --file=FILE [default: foo]"
            "prog -x... -v... -z... -f=FILE..." ]

    , test "prog -xvzf=FILE..."
        [ pass "-f --file=FILE [default: foo]"
            "prog -x... -v... -z... -f=FILE..." ]

    , test "prog -xvzf FILE..."
        [ pass "-f --file=FILE  [default: foo]"
            "prog -x... -v... -z... -f=FILE..." ]

    , test "prog --file FILE..."
        [ fail
            """
            -f --file=FILE  [default: foo]
            -f --file=FILE  [default: foo]
            """
            "Multiple option descriptions for option --file" ]

    , test "prog -f FILE..."
        [ fail
            """
            -f --file=FILE  [default: foo]
            -f --file=FILE  [default: foo]
            """
            "Multiple option descriptions for option -f" ]

    , test "prog --file FILE..."
        [ fail
            """
            -f --file=FILE  [default: foo]
            -f --file=FILE  [default: foo]
            """
            "Multiple option descriptions for option --file" ]

    , test "prog -fx..."
        [ fail
            "-f --file=FILE  [default: foo]"
            "Stacked option -f may not specify arguments" ]

      -- Note: `f` should not adopt `file` as it's full name since it's in an
      -- option stack and not in trailing position (therefore cannot inherit the
      -- description's argument, rendering it an unfit candidate)
    , test "prog -fvzx..."
        [ fail
            "-f --file=FILE  [default: foo]"
            "Stacked option -f may not specify arguments" ]

    , test "prog -xvzf FILE..."
        [ pass
            "-f --file=FILE  [default: foo]"
            "prog -x... -v... -z... -f=FILE..." ]

    , test "prog -xvzf..."
        [ fail
            "-f --file=FILE  [default: foo]"
            "Option-Argument specified in options-section missing -f" ]

      -- same adjacent arg
    , test "prog -0 ARG ARG..."
        [ pass "-0, --foo ARG [default: 100]" "prog -0=ARG ARG..." ]

      -- optional arguments
    , test "prog -f"
        [ pass "-f[=ARG]" "prog -f[=ARG]" ]

    , test "prog --file"
        [ pass "--file[=ARG]" "prog --file[=ARG]" ]

      -- smart options
    , testSmartOpts "prog [-f FILE]"
        [ pass ""   "prog [-f=FILE]"
        , pass "-f" "prog [-f FILE]"
        ]

    , testSmartOpts "prog (-f FILE)"
        [ pass ""   "prog -f=FILE"
        , pass "-f" "prog (-f FILE)"
        ]

    , testSmartOpts     "prog [-a FOO] [-b BAR] [-c QUX]"
        [ pass ""       "prog [-a=FOO] [-b=BAR] [-c=QUX]"
        , fail "-a BAR" "Arguments mismatch for option -a: \"FOO\" and \"BAR\""
        ]

    , testSmartOpts "prog (-x <foo> | -f FILE)"
        [ pass ""                  "prog (-x=<foo> | -f=FILE)"
        , pass "-x"                "prog (-x <foo> | -f=FILE)"
        , pass "-f"                "prog (-x=<foo> | -f FILE)"
        , pass "-f\n-x"            "prog (-x <foo> | -f FILE)"
        , pass "-x=<foo>"          "prog (-x=<foo> | -f=FILE)"
        , pass "-f=FILE"           "prog (-x=<foo> | -f=FILE)"
        , pass "-f=FILE\n-x=<foo>" "prog (-x=<foo> | -f=FILE)"
        ]

      -- [references]:
    , test "prog [options]"
        [ pass
            """
            -a        Add
            -r        Remote
            -m <msg>  Message
            """
            "prog ([-a] [-r] [-m<msg>])"
        ]

    , test "prog [options]"
        [ pass (intercalate "\n" [ "-i", "-o" ]) "prog ([-i] [-o])"
        , pass (intercalate "\n" [ "-i=FOO", "-o" ]) "prog ([-i=FOO] [-o])"
        , pass (intercalate "\n" [ "-i=FOO", "-o=FOO" ]) "prog ([-i=FOO] [-o=FOO])"
        ]

    , test "prog [options] -o"
        [ pass (intercalate "\n" [ "-i", "-o" ]) "prog [-i] -o"
        , pass (intercalate "\n" [ "-i=FOO", "-o" ]) "prog [-i=FOO] -o"
        ]

    , test "prog -i [options] -o"
        [ pass (intercalate "\n" [ "-i", "-o" ]) "prog -i -o"
        , pass (intercalate "\n" [ "-i", "-o" ]) "prog -i -o"
        , pass (intercalate "\n" [ "-i[=FOO]", "-o" ]) "prog -i[=FOO] -o"
        ]
    ]) runTest

  where
  runTest (TestSuite smartOptions usage cases) = do
    describe ("\nusage: " <> usage <> "\n") do
      for_ cases \(TestCase description expected) ->
        let description' = if S.length description > 0
                              then description
                              else "(no description)"
         in describe description' $
          let message = case expected of
                Left err     -> "should fail with: " <> show err
                Right result -> result
           in it message $ vliftEff do
            expected' <- runEitherEff do
              case expected of
                Left err  -> pure (Left err)
                Right res -> Right <$> do
                  { program, layouts } <- do
                    toks <- Error.capture $ Lexer.lexUsage res
                    Error.capture $ SpecParser.parseUsage toks
                  pure $ Spec {
                    program
                  , helpText: ""
                  , shortHelp: ""
                  , descriptions: Nil
                  , layouts: unsafePartial $
                                ((usageToSolved <$> _) <$> _) <$> layouts
                  }
            let output' = do
                  spec <- lmap pretty do
                    { program, layouts } <- do
                      toks <- Error.capture $ Lexer.lexUsage usage
                      Error.capture $ SpecParser.parseUsage toks
                    descriptions <- do
                      toks <- Error.capture $ Lexer.lexDescs description
                      Error.capture $ SpecParser.parseDescription toks
                    pure (Spec {
                        descriptions
                      , program
                      , layouts
                      , helpText: ""
                      , shortHelp: ""
                    })
                  lmap pretty $ solve { smartOptions } spec
            case expected' /\ output' of
              Left expected /\ Left actual | expected /= actual  ->
                throwException $ error $
                  "Wrong exception! Got:\n\n" <> pretty actual
              Left _ /\ Right result ->
                throwException $ error $
                  "Missing exception! Got:\n\n" <> pretty result
              Right (Spec expected) /\ Right (Spec actual)
                | expected.layouts /= actual.layouts ->
                    throwException $ error $
                      "Unexpected output! expected:\n\n" <> pretty (Spec expected)
                      <> "\n\nbut got:\n\n" <> pretty (Spec actual)
              Right _ /\ Left err ->
                throwException $ error $ "Failure!\n" <> err
              _ -> pure unit
