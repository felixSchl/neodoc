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

    it "should parse positionals" do
      liftEff do
        usage <- runEitherEff do
          toks <- Lexer.lex "foo BAR <foo-qux> <QUX> <QuX> <quux>... <quux> ..."
          Usage.parse toks

        -- There should only be one top-level mutex group
        assertEqual 1 (length usage)
        (Usage.Usage _ u) <- runMaybeEff $ usage !! 0
        g <- runMaybeEff $ u !! 0

        -- Make assertions about the group
        flip assertEqual g
          (Cons (Usage.Positional "BAR" false)
          (Cons (Usage.Positional "foo-qux" false)
          (Cons (Usage.Positional "QUX" false)
          (Cons (Usage.Positional "QuX" false)
          (Cons (Usage.Positional "quux" true)
          (Cons (Usage.Positional "quux" true) Nil))))))

    it "should parse long options" do
      liftEff do
        usage <- runEitherEff do
          toks <- Lexer.lex $ Textwrap.dedent
            """
            foo --bar
                --bar=<foo-bar>
                --bar=FOO
                --bar=fOo
                --bar = foo
                --bar...
                --bar = baz...
                --bar=<baz>...
            """
          Usage.parse toks

        -- There should only be one top-level mutex group
        assertEqual 1 (length usage)
        (Usage.Usage _ u) <- runMaybeEff $ usage !! 0
        g <- runMaybeEff $ u !! 0

        -- Make assertions about the group
        flip assertEqual g
          (Cons (Usage.Option "bar" Nothing false)
          (Cons (Usage.Option "bar" (Just "foo-bar") false)
          (Cons (Usage.Option "bar" (Just "FOO") false)
          (Cons (Usage.Option "bar" (Just "fOo") false)
          (Cons (Usage.Option "bar" (Just "foo") false)
          (Cons (Usage.Option "bar" Nothing true)
          (Cons (Usage.Option "bar" (Just "baz") true)
          (Cons (Usage.Option "bar" (Just "baz") true) Nil))))))))

    it "should parse option stacks" do
      liftEff do
        usage <- runEitherEff do
          toks <- Lexer.lex $ Textwrap.dedent
            """
            foo -b         -b...
                -bFOO      -bFOO...
                -bFoo      -bFoo...
                -b<foo>    -b<foo>...
                -b foo     -b foo
                -b FOO     -b FOO...
                -b=foo     -b=foo...
                -b=FOO     -b=FOO...
                -b=<foo>   -b=<foo>...
                -bar       -bar...
                -barFOO    -barFOO...
                -bar<foo>  -bar<foo>...
                -barFoo    -barFoo...
                -bar foo   -bar foo...
                -bar FOO   -bar FOO...
                -bar=foo   -bar=foo...
                -bar=FOO   -bar=FOO...
                -bar=<foo> -bar=<foo>...
            """
                -- -bAR       -bAR...
                -- -bARfoo    -bARfoo...
                -- -bAR=foo   -bAR=foo...
                -- -bAR=<foo> -bAR=<foo>...
                -- -bAR<foo>  -bAR<foo>...
          Usage.parse toks

        -- There should only be one top-level mutex group
        assertEqual 1 (length usage)
        (Usage.Usage _ u) <- runMaybeEff $ usage !! 0
        g <- runMaybeEff $ u !! 0

        -- Make assertions about the group
        flip assertEqual g
          (Cons (so 'b' [] Nothing false)
          (Cons (so 'b' [] Nothing true)
          (Cons (so 'b' [] (Just "FOO") false)
          (Cons (so 'b' [] (Just "FOO") true)
          (Cons (so 'b' [] (Just "Foo") false)
          (Cons (so 'b' [] (Just "Foo") true)
          (Cons (so 'b' [] (Just "foo") false)
          (Cons (so 'b' [] (Just "foo") true)
          (Cons (so 'b' [] Nothing false) (Cons (Usage.Command "foo")
          (Cons (so 'b' [] Nothing false) (Cons (Usage.Command "foo")
          (Cons (so 'b' [] Nothing false) (Cons (Usage.Positional "FOO" false)
          (Cons (so 'b' [] Nothing false) (Cons (Usage.Positional "FOO" true)
          (Cons (so 'b' [] (Just "foo") false)
          (Cons (so 'b' [] (Just "foo") true)
          (Cons (so 'b' [] (Just "FOO") false)
          (Cons (so 'b' [] (Just "FOO") true)
          (Cons (so 'b' [] (Just "foo") false)
          (Cons (so 'b' [] (Just "foo") true)
          (Cons (so 'b' ['a', 'r'] Nothing false)
          (Cons (so 'b' ['a', 'r'] Nothing true)
          (Cons (so 'b' ['a', 'r'] (Just "FOO") false)
          (Cons (so 'b' ['a', 'r'] (Just "FOO") true)
          (Cons (so 'b' ['a', 'r'] (Just "Foo") false)
          (Cons (so 'b' ['a', 'r'] (Just "Foo") true)
          (Cons (so 'b' ['a', 'r'] (Just "foo") false)
          (Cons (so 'b' ['a', 'r'] (Just "foo") true)
          (Cons (so 'b' ['a', 'r'] Nothing false) (Cons (Usage.Command "foo")
          (Cons (so 'b' ['a', 'r'] Nothing false) (Cons (Usage.Command "foo")
          (Cons (so 'b' ['a', 'r'] Nothing false) (Cons (Usage.Positional "FOO" false)
          (Cons (so 'b' ['a', 'r'] Nothing false) (Cons (Usage.Positional "FOO" true)
          (Cons (so 'b' ['a', 'r'] (Just "foo") false)
          (Cons (so 'b' ['a', 'r'] (Just "foo") true)
          (Cons (so 'b' ['a', 'r'] (Just "FOO") false)
          (Cons (so 'b' ['a', 'r'] (Just "FOO") true)
          (Cons (so 'b' ['a', 'r'] (Just "foo") false)
          (Cons (so 'b' ['a', 'r'] (Just "foo") true)
          Nil))))))))))))))))))))))))))))))))))))))))))))

        pure unit


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

    so :: Char -> Array Char -> Maybe String -> Boolean -> Usage.UsageNode
    so c cs a r = Usage.OptionStack c cs a r
