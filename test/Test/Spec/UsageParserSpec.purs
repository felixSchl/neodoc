module Test.Spec.UsageParserSpec (usageParserSpec) where

import Prelude
import Debug.Trace
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
import Data.Foldable (intercalate, foldMap, traverse_, for_)
import Data.Array ((..))

import Language.Docopt
import qualified Test.Support.Usage                    as U
import qualified Language.Docopt.Parser.Usage          as U
import qualified Language.Docopt.Parser.Usage.Argument as U
import qualified Language.Docopt.Parser.Lexer          as Lexer
import qualified Language.Docopt.Scanner               as Scanner
import Language.Docopt.Parser.Base (debug)
import Text.Wrap (dedent)

import Test.Assert (assert)
import Test.Spec (describe, it)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Assert.Simple
import Test.Support (vliftEff, runMaybeEff, runEitherEff)

data Expected a = F | P a

sopt_ f fs r = (if r then U.soptR_ else U.sopt_) f fs
sopt  f fs a r = (if r then U.soptR else U.sopt) f fs a
lopt_ f r = (if r then U.loptR_ else U.lopt_) f
lopt  f a r = (if r then U.loptR else U.lopt) f a
po    n r = (if r then U.poR else U.po) n
arg'  n o = { name: n, optional: o }
arg_  n = arg' n true
arg   n = arg' n false

usageParserSpec = \_ ->
  describe "The usage parser" do

    -- Test commands.
    -- Commands are the least considered type of token.
    -- Only after the token is not considered a positional or
    -- option, it defaults to being a command (given that the
    -- command parer succeeds).
    it "should parse commands" do
      vliftEff do
        usage <- runEitherEff do
          toks <- Lexer.lexUsage "foo bar"
          U.parse toks
        assertEqual 1 (length usage)
        (U.Usage _ u) <- runMaybeEff $ usage !! 0
        g <- runMaybeEff $ u !! 0
        flip assertEqual g (Cons (U.Command "bar" false) Nil)

    -- Test positionals in various formats.
    -- Each entry is run for both singular and repeated version.
    describe "positionals" do
      runSingleArgumentTests
        [ pass "BAR"       $ po "BAR"
        , pass "<foo-qux>" $ po "foo-qux"
        , pass "<QUX>"     $ po "QUX"
        , pass "<QuX>"     $ po "QuX"
        , pass "<quux>"    $ po "quux"
        ]

    -- Test long options in various formats.
    -- Each entry is run for both singular and repeated version.
    describe "long options" do
      runSingleArgumentTests
        [ pass "--bar"         $ lopt_ "bar"
        , pass "--bar = foo"   $ lopt  "bar" (arg "foo")
        , pass "--bar=<foo>"   $ lopt  "bar" (arg "foo")
        , pass "--bar=FOO"     $ lopt  "bar" (arg "FOO")
        , pass "--bar = FOO"   $ lopt  "bar" (arg "FOO")
        , pass "--bar=fOo"     $ lopt  "bar" (arg "fOo")
        , pass "--bar = fOo"   $ lopt  "bar" (arg "fOo")
        , pass "--bar = <foo>" $ lopt  "bar" (arg "foo")
        , pass "--barFOO"      $ lopt_ "barFOO"

        , pass "--bar[ = foo]"   $ lopt  "bar" (arg "foo")
        , pass "--bar[=<foo>]"   $ lopt  "bar" (arg "foo")
        , pass "--bar[=FOO]"     $ lopt  "bar" (arg "FOO")
        , pass "--bar[ = FOO]"   $ lopt  "bar" (arg "FOO")
        , pass "--bar[=fOo]"     $ lopt  "bar" (arg "fOo")
        , pass "--bar[ = fOo]"   $ lopt  "bar" (arg "fOo")
        , pass "--bar[ = <foo>]" $ lopt  "bar" (arg "foo")
        , pass "--bar[FOO]"      $ lopt  "bar" (arg "FOO")

        , pass "--bar [ = foo]"   $ lopt  "bar" (arg "foo")
        , pass "--bar [=<foo>]"   $ lopt  "bar" (arg "foo")
        , pass "--bar [=FOO]"     $ lopt  "bar" (arg "FOO")
        , pass "--bar [ = FOO]"   $ lopt  "bar" (arg "FOO")
        , pass "--bar [=fOo]"     $ lopt  "bar" (arg "fOo")
        , pass "--bar [ = fOo]"   $ lopt  "bar" (arg "fOo")
        , pass "--bar [ = <foo>]" $ lopt  "bar" (arg "foo")

        , fail "--bar="
        , fail "--bar=<>"
        , fail "--bar=--foo"
        , fail "--bar=-foo"

        , fail "--bar[=]"
        , fail "--bar[=<>]"
        , fail "--bar[=--foo]"
        , fail "--bar[=-foo]"
        ]

    -- Test stacked options in various formats.
    -- Each entry is run for both singular and repeated version.
    describe "stacked options" do
      runSingleArgumentTests
        [ pass "-b"          $ sopt_ 'b' []
        , pass "-bFOO"       $ sopt_ 'b' ['F', 'O', 'O']
        , pass "-bFoo"       $ sopt_ 'b' ['F', 'o', 'o']
        , pass "-b<foo>"     $ sopt  'b' [] (arg "foo")
        , pass "-b<foo>"     $ sopt  'b' [] (arg "foo")
        , pass "-b=foo"      $ sopt  'b' [] (arg "foo")
        , pass "-b=FOO"      $ sopt  'b' [] (arg "FOO")
        , pass "-b=<foo>"    $ sopt  'b' [] (arg "foo")

        , pass "-b[FOO]"     $ sopt  'b' [] (arg "FOO")
        , pass "-b[Foo]"     $ sopt  'b' [] (arg "Foo")
        , pass "-b[<foo>]"   $ sopt  'b' [] (arg "foo")
        , pass "-b[<foo>]"   $ sopt  'b' [] (arg "foo")
        , pass "-b[=foo]"    $ sopt  'b' [] (arg "foo")
        , pass "-b[=FOO]"    $ sopt  'b' [] (arg "FOO")
        , pass "-b[=<foo>]"  $ sopt  'b' [] (arg "foo")

        , pass "-b [=foo]"    $ sopt  'b' [] (arg "foo")
        , pass "-b [=FOO]"    $ sopt  'b' [] (arg "FOO")
        , pass "-b [=<foo>]"  $ sopt  'b' [] (arg "foo")

        , pass "-bar"        $ sopt_ 'b' ['a', 'r']
        , pass "-barFOO"     $ sopt_ 'b' ['a', 'r', 'F', 'O', 'O']
        , pass "-bar<foo>"   $ sopt  'b' ['a', 'r'] (arg "foo")
        , pass "-barFoo"     $ sopt_ 'b' ['a', 'r', 'F', 'o', 'o']
        , pass "-bar=foo"    $ sopt  'b' ['a', 'r'] (arg "foo")
        , pass "-bar=FOO"    $ sopt  'b' ['a', 'r'] (arg "FOO")
        , pass "-bar=<foo>"  $ sopt  'b' ['a', 'r'] (arg "foo")
        , pass "-bAR"        $ sopt_ 'b' ['A', 'R']
        , pass "-bARfoo"     $ sopt_ 'b' ['A', 'R', 'f', 'o', 'o']

        , pass "-bar[FOO]"    $ sopt 'b' ['a', 'r'] (arg_ "FOO")
        , pass "-bar[<foo>]"  $ sopt 'b' ['a', 'r'] (arg_ "foo")
        , pass "-bar[Foo]"    $ sopt 'b' ['a', 'r'] (arg_ "Foo")
        , pass "-bar[=foo]"   $ sopt 'b' ['a', 'r'] (arg_ "foo")
        , pass "-bar[=FOO]"   $ sopt 'b' ['a', 'r'] (arg_ "FOO")
        , pass "-bar[=<foo>]" $ sopt 'b' ['a', 'r'] (arg_ "foo")
        , pass "-bAR[foo]"    $ sopt 'b' ['A', 'R'] (arg_ "foo")
        ]

    -- Test required groups in various formats.
    -- Each entry is run for both singular and repeated version.
    describe "required groups" do
      runSingleArgumentTests
        [ fail "()"
        , pass "(foo)"          $ U.gr [[ U.co "foo" ]]
        , pass "(foo|bar)"      $ U.gr [[ U.co "foo" ], [ U.co "bar" ]]
        , pass "(foo bar|bar)"  $ U.gr [[ U.co "foo", U.co "bar"], [ U.co "bar" ]]
        , pass "((foo)|bar)"    $ U.gr [[ U.gr [[ U.co "foo" ]] false ], [ U.co "bar" ]]
        , pass "((foo)...|bar)" $ U.gr [[ U.gr [[ U.co "foo" ]] true ], [ U.co "bar" ]]
        , fail "(()|bar)"
        , fail "(bar|())"
        , fail "(...)"
        ]

    -- Test optional groups in various formats.
    -- Each entry is run for both singular and repeated version.
    describe "optional groups" do
      runSingleArgumentTests
        [ fail "[]"
        , pass "[foo]"          $ U.go [[ U.co "foo" ]]
        , pass "[foo|bar]"      $ U.go [[ U.co "foo" ], [U.co "bar"]]
        , pass "[foo bar|bar]"  $ U.go [[ U.co "foo", U.co "bar"], [U.co "bar"]]
        , pass "[[foo]|bar]"    $ U.go [[ U.go [[ U.co "foo" ]] false ], [U.co "bar"]]
        , pass "[[foo]...|bar]" $ U.go [[ U.go [[ U.co "foo" ]] true ], [U.co "bar"]]
        , fail "[[]|bar]"
        , fail "[bar|[]]"
        , fail "[...]"
        ]

    -- Test the EOA marker "--"
    describe "end-of-args" do
      runTests
        [ pass "--" $ [[[ U.eoa ]]]
        , pass "-- FOO..." $ [[[ U.eoa ]]]
        , pass "-- FOO... BAR" $ [[[ U.eoa ]]]
        , pass "foo -- FOO... BAR" $ [[[ U.co "foo", U.eoa ]]]
        ]

    -- Test the stdin marker "-"
    describe "stdin" do
      runTests
        [ pass "-" $ [[[ U.stdin ]]]
        , pass "-|-" $ [[[ U.stdin ], [ U.stdin ]]]
        , pass "--foo - --bar" $ [[[ U.lopt_ "foo"
                                   , U.stdin
                                   , U.lopt_ "bar"
                                   ]]]
        ]

    -- Test the "[options...]", "[options]", etc. syntax
    describe "stdin" do
      runTests
        [ pass "[options...]"     $ [[[ U.ref ""    ]]]
        , pass "[options]"        $ [[[ U.ref ""    ]]]
        , pass "[foo-options]"    $ [[[ U.ref "foo" ]]]
        , pass "[foo-options...]" $ [[[ U.ref "foo" ]]]
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
          toks <- Lexer.lexUsage docopt.usage
          U.parse toks
        assertEqual 2 (length usage)

        -- Validate the first usage
        (U.Usage _ u0) <- runMaybeEff $ usage !! 0
        assertEqual 2 (length u0)

        -- Validate the left half of the mutex group
        u0g0 <- runMaybeEff $ u0 !! 0
        assertEqual 1 (length u0g0)

        -- Validate the right half of the mutex group
        u0g1 <- runMaybeEff $ u0 !! 1
        assertEqual 2 (length u0g1)

        -- Validate the second usage
        (U.Usage _ u1) <- runMaybeEff $ usage !! 1
        assertEqual 1 (length u1)

        u1g0 <- runMaybeEff $ u1 !! 0
        assertEqual 1 (length u1g0)

  where

    kase :: forall a. String -> Expected a -> { i :: String, o :: Expected a }
    kase i o = { i: i, o: o }

    pass :: forall a. String -> a -> { i :: String, o :: Expected a }
    pass i o = kase i (P o)

    fail :: forall a. String -> { i :: String, o :: Expected a }
    fail i = kase i F

    runTests xs =
      for_ xs \{ i: i, o: o } -> do
        let input = "foo " ++ i
        case o of
          P expected -> do
            -- deeply convert array to list
            -- (array is used for readability above)
            let expected' = (U.Usage "foo" <$> do
                              (((toList <$>) <$>) toList
                                              <$> toList
                                              <$> toList expected))
            it (input
                ++ " -> "
                ++ intercalate "\n" (U.prettyPrintUsage <$> expected'))  do
              vliftEff do
                usages <- runEitherEff do
                  Lexer.lexUsage input >>= U.parse
                flip assertEqual
                  (U.prettyPrintUsage <$> usages)
                  (U.prettyPrintUsage <$> expected')
          _ -> do
            it (input ++ " should fail") do
            vliftEff do
              assertThrows (const true) do
                runEitherEff do
                  toks  <- Lexer.lexUsage input
                  usage <- U.parse toks
                  debug usage

    runSingleArgumentTests xs =
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
                it (input ++ " -> " ++ U.prettyPrintArg expected)  do
                  vliftEff do
                    usage <- runEitherEff do
                      Lexer.lexUsage input >>= U.parse

                    -- There should only be one top-level mutex group
                    assertEqual 1 (length usage)
                    (U.Usage _ u) <- runMaybeEff $ usage !! 0
                    g <- runMaybeEff $ u !! 0

                    -- Assert output matches expected
                    flip assertEqual (take 1 g) (Cons expected Nil)
              _ -> do
                it (input ++ " should fail") do
                vliftEff do
                  assertThrows (const true) do
                    runEitherEff do
                      toks  <- Lexer.lexUsage input
                      usage <- U.parse toks
                      debug usage
