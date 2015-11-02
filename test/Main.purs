module Test.Main where

import Prelude
import Debug.Trace
import Control.MonadPlus (guard)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Exception (EXCEPTION(), error, throwException)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Trans (lift)
import qualified Text.Parsing.Parser as P
import Data.Either (Either(..), isRight, isLeft, either)
import Data.Either.Unsafe (fromLeft, fromRight)
import Data.List (List(..), length, (!!))
import Data.Maybe.Unsafe (fromJust)
import Data.Maybe (Maybe(..))
import Test.Assert.Simple
import Data.Foldable (foldMap, traverse_, for_)
import Data.Array ((..))

import Docopt
import qualified Docopt.Parser.Usage as Usage
import qualified Docopt.Parser.Options as Options
import qualified Docopt.Textwrap as Textwrap
import qualified Docopt.Parser.Lexer as Lexer
import qualified Docopt.Parser.Scanner as Scanner
import Docopt.Parser.Base (debug)

import Test.Assert (assert)
import Test.Spec (describe, it)
import Test.Spec.Runner (run)
import Test.Spec.Reporter.Console (consoleReporter)

data Expected a = F | P a

main = run [consoleReporter] do
  describe "scanner" do
    it "should scan sections" do
      let docopt = fromRight $ Scanner.scan $
          Textwrap.dedent
            """
            Usage: foo
            Options: bar
            Advanced Options: qux
            """
      liftEff do
        assert $ docopt.usage == "foo\n"
        assert $ length docopt.options == 2
        assert $ fromJust (docopt.options !! 0) == " bar\n"
        assert $ fromJust (docopt.options !! 1) == " qux\n"
      pure unit

    it "should scan sections with new line after colon" do
      let docopt = fromRight $ Scanner.scan $
          Textwrap.dedent
            """
            Usage:
              foo
            Options:
              bar
            Advanced Options:
              qux
            """
      liftEff do
        assert $ docopt.usage == "foo\n"
        assert $ length docopt.options == 2
        assert $ fromJust (docopt.options !! 0) == "\n  bar\n"
        assert $ fromJust (docopt.options !! 1) == "\n  qux\n"
      pure unit

    it "should fail w/o a usage section" do
      let result = Scanner.scan $
          Textwrap.dedent
            """
            Options: bar
            """
      liftEff $ do
        assert $ isLeft result
      pure unit

    it "should fail multiple usage sections" do
      let result = Scanner.scan $
          Textwrap.dedent
            """
            Usage: bar
            Options: foo
            Usage: qux
            """
      liftEff $ do
        assert $ isLeft result
      pure unit

    it "should fail if usage section is not the first section" do
      let result = Scanner.scan $
          Textwrap.dedent
            """
            Options: foo
            Usage: qux
            """
      liftEff $ do
        assert $ isLeft result
      pure unit

    it "should fail w/o any sections" do
      let result = Scanner.scan $
          Textwrap.dedent
            """
            """
      liftEff $ do
        assert $ isLeft result
      pure unit

  describe "parser" do

    it "should parse commands" do
      liftEff do
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
        [ { i: "BAR"       , o: P $ po "BAR"     }
        , { i: "<foo-qux>" , o: P $ po "foo-qux" }
        , { i: "<QUX>"     , o: P $ po "QUX"     }
        , { i: "<QuX>"     , o: P $ po "QuX"     }
        , { i: "<quux>"    , o: P $ po "quux"    }
        ]

    -- Test long options in various formats.
    -- Each entry is run for both singular and repeated version.
    describe "long options" do
      runSingleUsageNodeTests
        [ { i: "--bar"         , o: P $ lo "bar" Nothing      }
        , { i: "--bar = foo"   , o: P $ lo "bar" (Just "foo") }
        , { i: "--bar=<foo>"   , o: P $ lo "bar" (Just "foo") }
        , { i: "--bar=FOO"     , o: P $ lo "bar" (Just "FOO") }
        , { i: "--bar = FOO"   , o: P $ lo "bar" (Just "FOO") }
        , { i: "--bar=fOo"     , o: P $ lo "bar" (Just "fOo") }
        , { i: "--bar = fOo"   , o: P $ lo "bar" (Just "fOo") }
        , { i: "--bar = <foo>" , o: P $ lo "bar" (Just "foo") }
        , { i: "-- bar"        , o: F                         }
        , { i: "- - bar"       , o: F                         }
        , { i: "--bar="        , o: F                         }
        , { i: "--bar=<>"      , o: F                         }
        , { i: "--bar=--foo"   , o: F                         }
        , { i: "--bar=-foo"    , o: F                         }
        ]

    -- Test stacked options in various formats.
    -- Each entry is run for both singular and repeated version.
    describe "stacked options" do
      runSingleUsageNodeTests
        [ { i: "-b"          , o: P $ so 'b' [] Nothing      }
        , { i: "-bFOO"       , o: P $ so 'b' [] (Just "FOO") }
        , { i: "-bFoo"       , o: P $ so 'b' [] (Just "Foo") }
        , { i: "-b<foo>"     , o: P $ so 'b' [] (Just "foo") }
        , { i: "-b<foo>"     , o: P $ so 'b' [] (Just "foo") }
        , { i: "-b=foo"      , o: P $ so 'b' [] (Just "foo") }
        , { i: "-b=FOO"      , o: P $ so 'b' [] (Just "FOO") }
        , { i: "-b=<foo>"    , o: P $ so 'b' [] (Just "foo") }
        , { i: "-bar"        , o: P $ so 'b' ['a', 'r'] Nothing }
        , { i: "-barFOO"     , o: P $ so 'b' ['a', 'r'] (Just "FOO") }
        , { i: "-bar<foo>"   , o: P $ so 'b' ['a', 'r'] (Just "foo") }
        , { i: "-barFoo"     , o: P $ so 'b' ['a', 'r'] (Just "Foo") }
        , { i: "-bar=foo"    , o: P $ so 'b' ['a', 'r'] (Just "foo") }
        , { i: "-bar=FOO"    , o: P $ so 'b' ['a', 'r'] (Just "FOO") }
        , { i: "-bar=<foo>"  , o: P $ so 'b' ['a', 'r'] (Just "foo") }
        , { i: "-bAR"        , o: P $ so 'b' [] (Just "AR") }
        , { i: "-bARfoo"     , o: P $ so 'b' [] (Just "ARfoo") }
        , { i: "-bAR=foo"    , o: F }
        , { i: "-bAR=<foo>"  , o: F }
        , { i: "-bAR<foo>"   , o: F }
        ]

                -- -b foo     -b FOO...
                -- -b FOO     -b FOO...
        --         -bar foo   -bar foo...
        --         -bar FOO   -bar FOO...

    it "should parse scanned and lexed usage sections" do
      liftEff do

        -- Scan, lex and parse the usage section
        usage <- runEitherEff do
          docopt <- Scanner.scan $
            Textwrap.dedent
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

      pure unit

  where

    runMaybeEff :: forall a eff.
      Maybe a ->
      Eff (err :: EXCEPTION | eff) a
    runMaybeEff m =
      case m of
        Just v  -> pure v
        Nothing -> throwException (error "Nothing")

    runEitherEff :: forall err a eff. (Show err, Show a) =>
      Either err a ->
      Eff (err :: EXCEPTION | eff) a
    runEitherEff m =
      case m of
        Right v  -> pure v
        Left err -> throwException (error $ show err)

    -- short hand to create a short option node
    so :: Char -> Array Char -> Maybe String -> Boolean -> Usage.UsageNode
    so = Usage.OptionStack

    -- short hand to create a long option node
    lo :: String -> Maybe String -> Boolean -> Usage.UsageNode
    lo = Usage.Option

    -- short hand to create a positional node
    po :: String -> Boolean -> Usage.UsageNode
    po = Usage.Positional

    runSingleUsageNodeTests xs =
      for_ xs \{ i: i, o: o } -> do
        for_ [ false, true ] \isRepeated -> do -- Append "..." ?
          for_ (1 .. 2) \q -> do -- "..." vs " ..." (note the space)
            let input = "foo " ++ i ++ (if isRepeated
                                       then (if q == 1 then "..." else " ...")
                                       else "")
            case o of
              P v -> do
                let expected = v isRepeated
                it (input ++ " -> " ++ show expected)  do
                  liftEff do
                    usage <- runEitherEff do
                      Lexer.lex input >>= Usage.parse

                    -- There should only be one top-level mutex group
                    assertEqual 1 (length usage)
                    (Usage.Usage _ u) <- runMaybeEff $ usage !! 0
                    g <- runMaybeEff $ u !! 0

                    -- Assert output matches expected
                    flip assertEqual g (Cons expected Nil)
              _ -> do
                it (input ++ " should fail") do
                liftEff do
                  assertThrows (const true) do
                    runEitherEff do
                      toks <- Lexer.lex input 
                      traceShowA input
                      Usage.parse toks >>= debug
                      pure unit
