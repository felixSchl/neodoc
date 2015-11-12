module Test.Spec.GeneratorSpec (generatorSpec) where

import Prelude
import Control.Monad.Aff (liftEff')
import Debug.Trace

import Docopt
import Docopt.Parser.Usage (Usage(..))
import qualified Docopt.Parser.Usage as Usage
import qualified Docopt.Parser.Options as Options
import qualified Docopt.Textwrap as Textwrap
import qualified Docopt.Parser.Lexer as Lexer
import qualified Docopt.Parser.Scanner as Scanner
import Docopt.Parser.Base (debug)

import Test.Assert (assert)
import Test.Spec (describe, it)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Assert.Simple
import Test.Support (vliftEff, runMaybeEff, runEitherEff)

import Control.Monad.State (State(), evalState)

import qualified Text.Parsing.Parser as P
import qualified Text.Parsing.Parser.Combinators as P
import qualified Text.Parsing.Parser.Pos as P
import qualified Text.Parsing.Parser.String as P
import qualified Data.List as L

import Data.List (List(..), many, some, (:), toList, concat)
import Data.Maybe
import Data.Either (Either(..))

type CliParserState = {}
type CliParser a = P.ParserT (List String) (State CliParserState) a
runCliParser :: forall a.
                  (List String)
                -> CliParser a
                -> Either P.ParseError a
runCliParser s =
  flip evalState ({})
  <<< P.runParserT
  (P.PState { input: s, position: P.initialPos })

generateParser :: Usage -> CliParser (List Unit)
generateParser (Usage _ xs) = parseMutex xs
    where
      parseMutex :: List (List Usage.UsageNode) -> CliParser (List Unit)
      parseMutex xs = P.choice (parseGroup <$> xs)

      parseGroup :: List Usage.UsageNode -> CliParser (List Unit)
      parseGroup (Cons x xs) = Cons <$> (parseNode x) <*> (parseGroup xs)
      parseGroup Nil         = pure Nil

      parseNode :: Usage.UsageNode -> CliParser Unit
      parseNode (Usage.Command n) = pure unit
      parseNode _                 = pure unit

generatorSpec =
  describe "options parser" do
    it "should have some tests..." do
      let g = (lo "Foo" Nothing true)
          p = generateParser $ Usage "" (Cons (Cons g Nil) Nil)
      vliftEff do
        result <- runEitherEff do
          flip runCliParser p $ toList [ "foo" ]
        traceShowA result
      pure unit

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
