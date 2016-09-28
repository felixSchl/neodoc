module Test.Spec.UsageParserSpec (usageParserSpec) where

import Prelude
import Debug.Trace
import Control.Monad (when)
import Data.NonEmpty (NonEmpty, (:|))
import Data.Pretty (class Pretty)
import Data.Bifunctor (lmap)
import Control.Monad.Aff (liftEff')
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION(), error, throwException)
import Control.Monad.Error.Class (throwError)
import Data.List (List(..), length, (!!), take, fromFoldable, singleton, (:))
import Data.Either (Either(..), isRight, isLeft, either)
import Data.Maybe (Maybe(..), fromJust)
import Data.Foldable (intercalate, foldMap, traverse_, for_)
import Data.Array ((..))
import Data.Pretty (pretty)
import Partial.Unsafe (unsafePartial)

import Test.Support.Usage as U

import Neodoc.Error.Class (capture) as Error
import Neodoc.Spec.Lexer as Lexer
import Neodoc.Spec.Parser as SpecParser
import Neodoc.Spec.Parser.Base (debug)
import Neodoc.Spec.Parser.Usage as U
import Neodoc.Data.UsageLayout as U
import Neodoc.Data.OptionArgument
import Neodoc.Data.Layout

import Text.Wrap (dedent)
import Test.Assert (assert)
import Test.Spec (describe, it)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Support (vliftEff, runMaybeEff, runEitherEff, assertEqual, shouldEqual,
                    assertThrows)

data Expected a = F | P a

sopt_ f fs   r = (if r then U.soptR_ else U.sopt_) f fs
sopt  f fs a r = (if r then U.soptR else U.sopt) f fs a
lopt_ f      r = (if r then U.loptR_ else U.lopt_) f
lopt  f    a r = (if r then U.loptR else U.lopt) f a
po    n      r = (if r then U.poR else U.po) n
co    n      r = (if r then U.coR else U.co) n
arg_  n        = OptionArgument n true
arg   n        = OptionArgument n false

usageParserSpec = \_ -> do

    -- Test positionals in various formats.
    -- Each entry is run for both singular and repeated version.
    describe "positionals" do
      runSingleArgumentTests
        [ pass "BAR"       $ po "BAR"
        , pass "<foo-qux>" $ po "<foo-qux>"
        , pass "<QUX>"     $ po "<QUX>"
        , pass "<QuX>"     $ po "<QuX>"
        , pass "<quux>"    $ po "<quux>"
        ]

    -- Commands are basically the same as positionals with some exception.
    -- Commands are those positionals that are not enclosed in <...>  and do
    -- do not only have uppercase letters.
    describe "commands" do
      runSingleArgumentTests
        [ pass "0" $ co "0" ]

    -- Test long options in various formats.
    -- Each entry is run for both singular and repeated version.
    describe "long options" do
      runSingleArgumentTests
        [ pass "--bar"         $ lopt_ "bar"
        , fail "--bar = foo"
        , pass "--bar=<foo>"   $ lopt  "bar" (arg "<foo>")
        , pass "--bar=FOO"     $ lopt  "bar" (arg "FOO")
        , fail "--bar = FOO"
        , pass "--bar=fOo"     $ lopt  "bar" (arg "fOo")
        , fail "--bar = fOo"
        , fail "--bar = <foo>"
        , pass "--barFOO"      $ lopt_ "barFOO"

        , fail "--bar[ = foo]"
        , pass "--bar[=<foo>]"  $ lopt  "bar" (arg_ "<foo>")
        , pass "--bar[=FOO]"    $ lopt  "bar" (arg_ "FOO")
        , fail "--bar[ = FOO]"
        , pass "--bar[=fOo]"    $ lopt  "bar" (arg_ "fOo")
        , fail "--bar[ = fOo]"
        , fail "--bar[ = <foo>]"
        , pass "--bar[FOO]"     $ lopt  "bar" (arg_ "FOO")

        -- disallow space
        , fail "--bar [ = foo]"
        , fail "--bar [=<foo>]"
        , fail "--bar [=FOO]"
        , fail "--bar [ = FOO]"
        , fail "--bar [=fOo]"
        , fail "--bar [ = fOo]"
        , fail "--bar [ = <foo>]"

        , fail "--bar="
        , fail "--bar=<>"
        , fail "--bar=--foo"
        , fail "--bar=-foo"

        , fail "--bar[=]"
        , fail "--bar[=<>]"
        , fail "--bar[=--foo]"
        , fail "--bar[=-foo]"

          -- disallow immediate following by '-'
        , fail "--bar[=FOO]-"
        , fail "--BAR-"
         ]

    -- Test stacked options in various formats.
    -- Each entry is run for both singular and repeated version.
    describe "stacked options" do
      runSingleArgumentTests
        [ pass "-b"         $ sopt_ 'b' []
        , pass "-io"        $ sopt_ 'i' ['o']
        , pass "-?"         $ sopt_ '?' [] -- special chars
        , pass "-bFOO"      $ sopt_ 'b' ['F', 'O', 'O']
        , pass "-bFoo"      $ sopt_ 'b' ['F', 'o', 'o']
        , pass "-b<foo>"    $ sopt  'b' [] (arg "<foo>")
        , pass "-b<foo>"    $ sopt  'b' [] (arg "<foo>")
        , pass "-b=foo"     $ sopt  'b' [] (arg "foo")
        , pass "-b=FOO"     $ sopt  'b' [] (arg "FOO")
        , pass "-b=<foo>"   $ sopt  'b' [] (arg "<foo>")

        , pass "-b[FOO]"    $ sopt  'b' [] (arg_ "FOO")
        , pass "-b[Foo]"    $ sopt  'b' [] (arg_ "Foo")
        , pass "-b[<foo>]"  $ sopt  'b' [] (arg_ "<foo>")
        , pass "-b[<foo>]"  $ sopt  'b' [] (arg_ "<foo>")
        , pass "-b[=foo]"   $ sopt  'b' [] (arg_ "foo")
        , pass "-b[=FOO]"   $ sopt  'b' [] (arg_ "FOO")
        , pass "-b[=<foo>]" $ sopt  'b' [] (arg_ "<foo>")

        -- disallow space
        , fail "-b [=foo]"
        , fail "-b [=FOO]"
        , fail "-b [=<foo>]"

        , pass "-bar"       $ sopt_ 'b' ['a', 'r']
        , pass "-barFOO"    $ sopt_ 'b' ['a', 'r', 'F', 'O', 'O']
        , pass "-bar<foo>"  $ sopt  'b' ['a', 'r'] (arg "<foo>")
        , pass "-barFoo"    $ sopt_ 'b' ['a', 'r', 'F', 'o', 'o']
        , pass "-bar=foo"   $ sopt  'b' ['a', 'r'] (arg "foo")
        , pass "-bar=FOO"   $ sopt  'b' ['a', 'r'] (arg "FOO")
        , pass "-bar=<foo>" $ sopt  'b' ['a', 'r'] (arg "<foo>")
        , pass "-bAR"       $ sopt_ 'b' ['A', 'R']
        , pass "-bARfoo"    $ sopt_ 'b' ['A', 'R', 'f', 'o', 'o']

        , pass "-bar[FOO]"    $ sopt 'b' ['a', 'r'] (arg_ "FOO")
        , pass "-bar[<foo>]"  $ sopt 'b' ['a', 'r'] (arg_ "<foo>")
        , pass "-bar[Foo]"    $ sopt 'b' ['a', 'r'] (arg_ "Foo")
        , pass "-bar[=foo]"   $ sopt 'b' ['a', 'r'] (arg_ "foo")
        , pass "-bar[=FOO]"   $ sopt 'b' ['a', 'r'] (arg_ "FOO")
        , pass "-bar[=<foo>]" $ sopt 'b' ['a', 'r'] (arg_ "<foo>")
        , pass "-bAR[foo]"    $ sopt 'b' ['A', 'R'] (arg_ "foo")

          -- disallow immediate following by '-'
        , fail "-bAR[=FOO]-"
        , fail "-bAR-"
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
        [ pass "--"                [[[ U.eoa ]]]
        , pass "-- FOO..."         [[[ U.eoa ]]]
        , pass "-- FOO... BAR"     [[[ U.eoa ]]]
        , pass "foo -- FOO... BAR" [[[ U.co "foo", U.eoa ]]]
        ]

    -- Test the stdin marker "-"
    describe "stdin" do
      runTests
        [ pass "-"             [[[ U.stdin ]]]
        , pass "-|-"           [[[ U.stdin ], [ U.stdin ]]]
        , pass "--foo - --bar" [[[ U.lopt_ "foo"
                                 , U.stdin
                                 , U.lopt_ "bar"
                                 ]]]
        ]

    -- Test the "[options...]", "[options]", etc. syntax
    describe "references" do
      runTests
        [ pass "[options...]"     [[[ U.ref ""    ]]]
        , pass "[options]"        [[[ U.ref ""    ]]]
        , pass "[foo-options]"    [[[ U.ref "foo" ]]]
        , pass "[foo-options...]" [[[ U.ref "foo" ]]]
        ]

  where
  kase :: ∀ a. String -> Expected a -> { i :: String, o :: Expected a }
  kase i o = { i, o }

  pass :: ∀ a. String -> a -> { i :: String, o :: Expected a }
  pass i o = kase i (P o)

  fail :: ∀ a. String -> { i :: String, o :: Expected a }
  fail i = kase i F

  prettyTopLevel
    :: forall a
      . (Pretty a)
    => NonEmpty List (NonEmpty List a)
    -> String
  prettyTopLevel xs = intercalate "|" $ xs <#> \x ->
                        intercalate " " $ pretty <$> x

  runTests :: _
  runTests xs =
    for_ xs \{ i, o } -> do
      let input = "prog " <> i
      case o of
        P expectedLayouts -> do
          -- deeply convert array to list
          -- (array is used for readability above)
          let
            expected'
              = ((fromFoldable <$> _)  <$> _) fromFoldable
                                        <$>    fromFoldable
                                        <$>    fromFoldable expectedLayouts
            expected = expected' <#> \ex ->
              unsafePartial $ U.listToNonEmpty $ U.listToNonEmpty <$> ex

          it (input
              <> " -> "
              <> intercalate "\n" (prettyTopLevel <$> expected))  do
            vliftEff do
              pure unit

              -- { program, layouts } <- runEitherEff do
              --   Lexer.lexUsage input >>= U.parse
              -- assertEqual "prog" program
              -- assertEqual (U.prettyPrintUsage <$> expected')
              --             (U.prettyPrintUsage <$> usages)
        otherwise -> do
          pure unit
          -- it (input <> " should fail") do
          -- vliftEff do
          --   assertThrows (const true) do
          --     runEitherEff do
          --       toks       <- Lexer.lexUsage input
          --       { usages } <- U.parse false toks
          --       debug usages

  runSingleArgumentTests :: _
  runSingleArgumentTests xs =
    for_ xs \{ i, o } -> do
      for_ [ false, true ] \isRepeated -> do -- Append "..." ?
        let ys = if isRepeated then (1 .. 2) else (1 .. 1)
        for_ ys \q -> -- "..." vs " ..." (note the space)
          let input = "prog " <> i
                              <> (if isRepeated
                                    then (if q == 1 then "..." else " ...")
                                    else "")
           in case o of
            P v ->
              let expected = v isRepeated
               in it (input <> " -> " <> pretty expected) $ vliftEff do
                  { program, layouts } <- runEitherEff do
                    toks <- Error.capture $ Lexer.lexUsage input
                    Error.capture $ SpecParser.parseUsage toks
                  assertEqual "prog" program
                  case layouts of
                    ((x :| Nil) : Nil) :| Nil -> do
                      when (x /= expected) do
                        throwException $ error $
                          "Unexpected output:\n" <> pretty x
                    _ -> do
                      throwException $ error $
                        "Unexpected output:\n" <> show layouts
            _ -> do
              it (input <> " should fail") $ vliftEff do
                assertThrows (const true) do
                  runEitherEff do
                    toks <- Error.capture $ Lexer.lexUsage input
                    { layouts } <- Error.capture $ SpecParser.parseUsage toks
                    debug layouts
