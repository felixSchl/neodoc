module Test.Spec.UsageParserSpec (usageParserSpec) where

import Prelude
import Control.Monad.Aff (liftEff')
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION(), error, throwException)
import Control.Monad.Error.Class (throwError)
import Data.List (List(..), length, (!!), take, toList)
import Data.Either (Either(..), isRight, isLeft, either)
import Data.Either.Unsafe (fromLeft, fromRight)
import Data.Maybe.Unsafe (fromJust)
import Data.Maybe (Maybe(..))
import Data.Foldable (foldMap, traverse_, for_)
import Data.Array ((..))

import Docopt
import qualified Docopt.Spec.Parser.Usage as Usage
import qualified Docopt.Spec.Parser.Options as Options
import qualified Docopt.Spec.Parser.Lexer as Lexer
import qualified Docopt.Spec.Parser.Scanner as Scanner
import Docopt.Spec.Parser.Base (debug)
import Text.Wrap (dedent)

import Test.Assert (assert)
import Test.Spec (describe, it)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Assert.Simple
import Test.Support (vliftEff, runMaybeEff, runEitherEff)

data Expected a = F | P a

usageParserSpec =
  describe "usage parser" do

    -- Test commands.
    -- Commands are the least considered type of token.
    -- Only after the token is not considered a positional or
    -- option, it defaults to being a command (given that the
    -- command parer succeeds).
    it "should parse commands" do
      vliftEff do
        usage <- runEitherEff do
          toks <- Lexer.lex "foo bar"
          Usage.parse toks
        assertEqual 1 (length usage)
        (Usage.Usage _ u) <- runMaybeEff $ usage !! 0
        g <- runMaybeEff $ u !! 0
        flip assertEqual g (Cons (Usage.Command "bar") Nil)

    -- Test positionals in various formats.
    -- Each entry is run for both singular and repeated version.
    describe "positionals" do
      runSingleUsageNodeTests
        [ pass "BAR"       $ po "BAR"
        , pass "<foo-qux>" $ po "foo-qux"
        , pass "<QUX>"     $ po "QUX"
        , pass "<QuX>"     $ po "QuX"
        , pass "<quux>"    $ po "quux"
        ]

    -- Test long options in various formats.
    -- Each entry is run for both singular and repeated version.
    describe "long options" do
      runSingleUsageNodeTests
        [ pass "--bar"         $ lo "bar" Nothing
        , pass "--bar = foo"   $ lo "bar" (Just "foo")
        , pass "--bar=<foo>"   $ lo "bar" (Just "foo")
        , pass "--bar=FOO"     $ lo "bar" (Just "FOO")
        , pass "--bar = FOO"   $ lo "bar" (Just "FOO")
        , pass "--bar=fOo"     $ lo "bar" (Just "fOo")
        , pass "--bar = fOo"   $ lo "bar" (Just "fOo")
        , pass "--bar = <foo>" $ lo "bar" (Just "foo")
        , pass "--barFOO"      $ lo "barFOO" Nothing
        , fail "- - bar"
        , fail "--bar="
        , fail "--bar=<>"
        , fail "--bar=--foo"
        , fail "--bar=-foo"
        ]

    -- Test stacked options in various formats.
    -- Each entry is run for both singular and repeated version.
    describe "stacked options" do
      runSingleUsageNodeTests
        [ pass "-b"          $ so 'b' [] Nothing
        , pass "-bFOO"       $ so 'b' ['F', 'O', 'O'] Nothing
        , pass "-bFoo"       $ so 'b' ['F', 'o', 'o'] Nothing
        , pass "-b<foo>"     $ so 'b' [] (Just "foo")
        , pass "-b<foo>"     $ so 'b' [] (Just "foo")
        , pass "-b=foo"      $ so 'b' [] (Just "foo")
        , pass "-b=FOO"      $ so 'b' [] (Just "FOO")
        , pass "-b=<foo>"    $ so 'b' [] (Just "foo")
        , pass "-bar"        $ so 'b' ['a', 'r'] Nothing
        , pass "-barFOO"     $ so 'b' ['a', 'r', 'F', 'O', 'O'] Nothing
        , pass "-bar<foo>"   $ so 'b' ['a', 'r'] (Just "foo")
        , pass "-barFoo"     $ so 'b' ['a', 'r', 'F', 'o', 'o'] Nothing
        , pass "-bar=foo"    $ so 'b' ['a', 'r'] (Just "foo")
        , pass "-bar=FOO"    $ so 'b' ['a', 'r'] (Just "FOO")
        , pass "-bar=<foo>"  $ so 'b' ['a', 'r'] (Just "foo")
        , pass "-bAR"        $ so 'b' ['A', 'R'] Nothing
        , pass "-bARfoo"     $ so 'b' ['A', 'R', 'f', 'o', 'o'] Nothing
        ]

    -- Test required groups in various formats.
    -- Each entry is run for both singular and repeated version.
    describe "required groups" do
      runSingleUsageNodeTests
        [ fail "()"
        , pass "(foo)"          $ gr [[ co "foo" ]]
        , pass "(foo|bar)"      $ gr [[ co "foo" ], [co "bar"]]
        , pass "(foo bar|bar)"  $ gr [[ co "foo", co "bar"], [co "bar"]]
        , pass "((foo)|bar)"    $ gr [[ gr [[ co "foo" ]] false ], [co "bar"]]
        , pass "((foo)...|bar)" $ gr [[ gr [[ co "foo" ]] true ], [co "bar"]]
        , fail "(()|bar)"
        , fail "(bar|())"
        , fail "(...)"
        ]

    -- Test optional groups in various formats.
    -- Each entry is run for both singular and repeated version.
    describe "optional groups" do
      runSingleUsageNodeTests
        [ fail "[]"
        , pass "[foo]"          $ go [[ co "foo" ]]
        , pass "[foo|bar]"      $ go [[ co "foo" ], [co "bar"]]
        , pass "[foo bar|bar]"  $ go [[ co "foo", co "bar"], [co "bar"]]
        , pass "[[foo]|bar]"    $ go [[ go [[ co "foo" ]] false ], [co "bar"]]
        , pass "[[foo]...|bar]" $ go [[ go [[ co "foo" ]] true ], [co "bar"]]
        , fail "[[]|bar]"
        , fail "[bar|[]]"
        , fail "[...]"
        ]

    -- | Test the scanner and lexer in combination with the parser.
    -- | This validates that the program source can successfully extracted
    -- | from the - possibly - unstructured usage description text.
    it "should parse scanned and lexed usage sections" do
      vliftEff do

        -- Scan, lex and parse the usage section
        usage <- runEitherEff do
          docopt <- Scanner.scan $
            dedent
              """
              Usage: foo foo | bar aux
                    foo (bar qux)
              NOT PART OF SECTION
              """
          toks <- Lexer.lex docopt.usage
          Usage.parse toks
        assertEqual 2 (length usage)

        -- Validate the first usage
        (Usage.Usage _ u0) <- runMaybeEff $ usage !! 0
        assertEqual 2 (length u0)

        -- Validate the left half of the mutex group
        u0g0 <- runMaybeEff $ u0 !! 0
        assertEqual 1 (length u0g0)

        -- Validate the right half of the mutex group
        u0g1 <- runMaybeEff $ u0 !! 1
        assertEqual 2 (length u0g1)

        -- Validate the second usage
        (Usage.Usage _ u1) <- runMaybeEff $ usage !! 1
        assertEqual 1 (length u1)

        u1g0 <- runMaybeEff $ u1 !! 0
        assertEqual 1 (length u1g0)

  where

    -- short hand to create a command node
    co :: String -> Usage.UsageNode
    co = Usage.Command

    -- short hand to create a short option node
    so :: Char -> Array Char -> Maybe String -> Boolean -> Usage.UsageNode
    so = Usage.OptionStack

    -- short hand to create a long option node
    lo :: String -> Maybe String -> Boolean -> Usage.UsageNode
    lo = Usage.Option

    -- short hand to create a positional node
    po :: String -> Boolean -> Usage.UsageNode
    po = Usage.Positional

    -- short hand to create a required group node
    gr :: Array (Array Usage.UsageNode) -> Boolean -> Usage.UsageNode
    gr xs r = Usage.Group false ls r
      where ls = toList <$> (toList xs)

    -- short hand to create a optional group node
    go :: Array (Array Usage.UsageNode) -> Boolean -> Usage.UsageNode
    go xs r = Usage.Group true ls r
      where ls = toList <$> (toList xs)

    kase i o = { i: i, o: o }
    pass i o = kase i (P o)
    fail i = kase i F

    runSingleUsageNodeTests xs =
      for_ xs \{ i: i, o: o } -> do
        for_ [ false, true ] \isRepeated -> do -- Append "..." ?
          let ys = if isRepeated then (1 .. 2) else (1 .. 1)
          for_ ys \q -> do -- "..." vs " ..." (note the space)
            let input = "foo " ++ i ++ (if isRepeated
                                      then (if q == 1 then "..." else " ...")
                                      else "")
            case o of
              P v -> do
                let expected = v isRepeated
                it (input ++ " -> " ++ show expected)  do
                  vliftEff do
                    usage <- runEitherEff do
                      Lexer.lex input >>= Usage.parse

                    -- There should only be one top-level mutex group
                    assertEqual 1 (length usage)
                    (Usage.Usage _ u) <- runMaybeEff $ usage !! 0
                    g <- runMaybeEff $ u !! 0

                    -- Assert output matches expected
                    flip assertEqual (take 1 g) (Cons expected Nil)
              _ -> do
                it (input ++ " should fail") do
                vliftEff do
                  assertThrows (const true) do
                    runEitherEff do
                      toks  <- Lexer.lex input
                      usage <- Usage.parse toks
                      debug usage
