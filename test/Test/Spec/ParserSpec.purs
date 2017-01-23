module Test.Spec.ParserSpec (parserSpec) where

import Prelude
import Debug.Trace
import Data.List
import Data.List.Lazy (replicateM)
import Data.Either
import Control.Alt
import Control.Monad.Aff
import Control.Monad.Eff.Class
import Control.Monad.Trans.Class (lift)
import Control.Monad.State as State
import Neodoc.Parsing.Parser
import Neodoc.Parsing.Parser as Parser

import Test.Assert (assert)
import Test.Spec (describe, it)
import Test.Support

type TestError = String
type TestParser a = Parser TestError {} Int Int (List Unit) a

runTestParser :: _ -> _ -> _ -> _ -> TestParser _ -> _
runTestParser = runParser

parserSpec = \_ ->
  describe "The parser" do
    it "should be able to mutate local state" do
      liftEff do
        result <- runEitherEff do
          runTestParser {} 0 0 Nil do
            fromFoldable <$> do
              replicateM 10 do
                Parser.modifyState (_ + 1)
                Parser.getState
        assertEqual result (1:2:3:4:5:6:7:8:9:10:Nil)

    it "should be able to reset local state on error" do
      liftEff do
        result <- runEitherEff do
          runTestParser {} 0 0 Nil do
            (Parser.setState 10
              *> fail "boom") <|> (Parser.return unit)
            Parser.getState
        assertEqual result 0

    it "should be able to keep global state on error" do
      liftEff do
        result <- runEitherEff do
          runTestParser {} 0 0 Nil do
            (Parser.setGlobalState 10
              *> fail "boom") <|> (Parser.return unit)
            Parser.getGlobalState
        assertEqual result 10
