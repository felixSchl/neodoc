module Test.Spec.PreSolveSpec (preSolveSpec) where

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
import Neodoc.Transform.PreSolve (
  preSolve, PreSolvedLayoutArg(SolvedArg), PreSolvedLayout)
import Neodoc.Transform.SolveError (SolveError(..))
import Text.Wrap (dedent)
import Partial.Unsafe

-- | Since the solver only expands arguments, it's expanded version is valid
-- | usage input.
-- | For example: `-abc` expands to `-a -b -c`, which is valid input.
usageToSolved :: Partial => UsageLayout -> PreSolvedLayout
usageToSolved layout = layout <#> SolvedArg <<< case _ of
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
data TestSuite = TestSuite UsageText (Array TestCase)
data TestCase = TestCase DescriptionText (Either Error Result)

pass description expected = TestCase description (Right expected)
fail description err      = TestCase description (Left  err)

preSolveSpec = \_ ->
  describe "The solver" do
    for_ (fromFoldable [

      -- should not affect positionals and commands:
      TestSuite "prog foo"       [ pass "" "prog foo" ]
    , TestSuite "prog <prog>..." [ pass "" "prog <prog>..." ]

      -- options:
    , TestSuite "prog -io"
        [ pass "" "prog -i -o" ]

    , TestSuite "prog -io [-q]..."
        [ pass "" "prog -i -o [-q]..." ]

    , TestSuite "prog --foo..."
        [ fail "-f --foo=bar [default: qux]"
            "Option-Argument specified in options-section missing --foo" ]

    , TestSuite "prog --foo... BAR"
        [ fail "-f --foo=BAR [default: qux]"
            "Option-Argument specified in options-section missing --foo" ]

    , TestSuite "prog -f... BAR"
        [ fail "-f --foo=BAR [default: qux]"
            "Option-Argument specified in options-section missing -f" ]

    , TestSuite "prog --foo BAR..."
        [ pass "-f --foo=BAR [default: qux]" "prog --foo=BAR..."
        , pass "-f --foo" "prog --foo BAR..." ]

    , TestSuite "prog --foo BAR"
        [ pass "-f --foo=BAR [default: qux]" "prog --foo=BAR"
        , pass "-f --foo" "prog --foo BAR" ]

    , TestSuite "prog --foo... BAR..."
        [ fail "-f --foo=BAR [default: qux]"
            "Option-Argument specified in options-section missing --foo" ]

    , TestSuite "prog -f... BAR..."
        [ fail "-f --foo=BAR [default: qux]"
            "Option-Argument specified in options-section missing -f" ]

    , TestSuite "prog -xvzfFILE..."
        [ pass "-f --file=FILE [default: foo]"
            "prog -x... -v... -z... -f=FILE..." ]

    , TestSuite "prog -xvzf=FILE..."
        [ pass "-f --file=FILE [default: foo]"
            "prog -x... -v... -z... -f=FILE..." ]

    , TestSuite "prog -xvzf FILE..."
        [ pass "-f --file=FILE  [default: foo]"
            "prog -x... -v... -z... -f=FILE..." ]

    , TestSuite "prog --file FILE..."
        [ fail
            """
            -f --file=FILE  [default: foo]
            -f --file=FILE  [default: foo]
            """
            "Multiple option descriptions for option --file" ]

    , TestSuite "prog -f FILE..."
        [ fail
            """
            -f --file=FILE  [default: foo]
            -f --file=FILE  [default: foo]
            """
            "Multiple option descriptions for option -f" ]

    , TestSuite "prog --file FILE..."
        [ fail
            """
            -f --file=FILE  [default: foo]
            -f --file=FILE  [default: foo]
            """
            "Multiple option descriptions for option --file" ]

    , TestSuite "prog -fx..."
        [ fail
            "-f --file=FILE  [default: foo]"
            "Stacked option -f may not specify arguments" ]

      -- Note: `f` should not adopt `file` as it's full name since it's in an
      -- option stack and not in trailing position (therefore cannot inherit the
      -- description's argument, rendering it an unfit candidate)
    , TestSuite "prog -fvzx..."
        [ fail
            "-f --file=FILE  [default: foo]"
            "Stacked option -f may not specify arguments" ]

    , TestSuite "prog -xvzf FILE..."
        [ pass
            "-f --file=FILE  [default: foo]"
            "prog -x... -v... -z... -f=FILE..." ]

    , TestSuite "prog -xvzf..."
        [ fail
            "-f --file=FILE  [default: foo]"
            "Option-Argument specified in options-section missing -f" ]

    ]) runTest

  where
  runTest (TestSuite usage cases) = do
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
                    pure (Spec { descriptions, program, layouts })
                  lmap pretty $ preSolve spec
            case expected' /\ output' of
              Left expected /\ Left actual | expected /= actual  ->
                throwException $ error $
                  "Wrong exception! Got:\n\n" <> show actual
              Left _ /\ Right result ->
                throwException $ error $
                  "Missing exception! Got:\n\n" <> pretty result
              Right (Spec expected) /\ Right (Spec actual)
                | expected.layouts /= actual.layouts ->
                    throwException $ error $
                      "Unexpected output! Got:\n" <> pretty (Spec actual)
                      <> "\n\nbut expected:\n" <> pretty (Spec expected)
              Right _ /\ Left err ->
                throwException $ error $ "Failure!\n" <> err
              _ -> pure unit
