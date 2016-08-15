module Test.Spec.ScannerSpec (scannerSpec) where

import Prelude
import Debug.Trace
import Control.Monad.Aff
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (error, throwException, EXCEPTION())
import Data.Bifunctor (lmap)
import Data.List (List(..), length, (!!), take)
import Data.TemplateString.Unsafe ((<~>))
import Data.Either (Either(..), isRight, isLeft, either, fromRight)
import Data.Either (fromRight)
import Data.Maybe (Maybe(..), fromJust)
import Data.String.Chalk as Chalk
import Data.String.Ext as String
import Text.Parsing.Parser (ParseError(..))
import Data.String.Regex as Regex
import Data.String.Regex (regex, Regex())
import Data.String as String
import Partial.Unsafe (unsafePartial)

import Language.Docopt
import Language.Docopt.Scanner as Scanner
import Text.Wrap (dedent)

import Test.Assert (assert)
import Test.Spec (describe, it)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Support (vliftEff, runEitherEff)

scannerSpec = \_ ->
  describe "The scanner" do
    it "should scan sections" do
      vliftEff do
        { usage, options } <- runEitherEff do
          scan
            """
            Usage: foo
            Options: bar
            Advanced Options: qux
            """
        length options `shouldEq` 2
        usage                    =?= "______ foo"
        fromJust' (options !! 0) =?= "________ bar"
        fromJust' (options !! 1) =?= "_________________ qux"

    it "should scan sections with new line after colon" do
      vliftEff do
        { usage, options } <- runEitherEff do
          scan
            """
            Usage:
              foo
            Options:
              bar
            Advanced Options:
              qux
            """

        length options `shouldEq` 2
        usage =?=
          """
          ______
          __foo
          """
        fromJust' (options !! 0) =?=
          """
          ________
          __bar
          """
        fromJust' (options !! 1) =?=
          """
          _________________
          __qux
          """

    it "should ignore ascii escape codes" do
      vliftEff do
        { usage, options } <- runEitherEff do
          scan do
            (Chalk.blue "Usage:")
              <> " foo"
              <> "\n"
        length options `shouldEq` 0
        usage =?= "_________________foo\n"

    it "should ignore ascii escape codes" do
      vliftEff do
        { usage, options } <- runEitherEff do
          scan $ dedent do
            """
            ${usageHeader}
              foo
            """ <~> {
              usageHeader: Chalk.blue "Usage:"
            }
        length options `shouldEq` 0
        usage =?=
          """
          ________________
          __foo
          """

    it "should fail w/o a usage section" do
      vliftEff do
        scan "Options: bar" `shouldFailWith` "No usage section found!"

    it "should fail w/o any sections" do
      vliftEff do
        scan "" `shouldFailWith` "No usage section found!"

scan = lmap getMessage <<< Scanner.scan <<< dedent

getMessage :: ParseError -> String
getMessage (ParseError m _ _) = m

shouldFailWith :: ∀ a. Either String a -> String -> Eff _ Unit
shouldFailWith ea msg = case ea of
                      Left m | m == msg -> pure unit
                      Right _           -> throwException $ error $
                                            "Expected failure:\n" <> msg
                      Left m            -> throwException $ error $
                                            "Unexpected error message:\n"
                                              <> m <> "\n"
                                              <> "Expected :\n"
                                              <> msg

shouldEq :: ∀ a. (Show a, Eq a) => a -> a -> Eff _ Unit
shouldEq a b = if a /= b
                  then
                    throwException $ error $
                      "Expected " <> show a <> " to equal " <> show b
                  else pure unit


shouldEqS :: String -> String -> Eff _ Unit
shouldEqS a b = shouldEq a
  $ addTrailingNewline  -- ensure trailing '\n'
  $ stripInitialNewline -- remove leading '\n'
  $ Regex.replace       -- replace all '_' with ' '
      (unsafePartial $ fromRight $ regex "_" $ Regex.parseFlags "g")
      " "
      (dedent b)
  where
    stripInitialNewline s =
          if String.startsWith "\n" s
              then String.drop 1 s
              else s
    addTrailingNewline s =
          if String.endsWith "\n" s
              then s
              else s <> "\n"

fromJust' :: ∀ a. Maybe a -> a
fromJust' ma = unsafePartial $ fromJust ma

infixl 0 shouldEqS as =?=
