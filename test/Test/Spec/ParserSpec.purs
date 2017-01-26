module Test.Spec.ParserSpec (parserSpec) where

import Prelude
import Debug.Trace
import Debug.Profile
import Data.Function
import Data.Foldable (for_)
import Data.List hiding (many)
import Data.Tuple (Tuple, fst)
import Data.Bifunctor (rmap, lmap)
import Data.Tuple.Nested ((/\), Tuple3)
import Data.Newtype
import Data.Generic
import Data.String as String
import Data.Pretty
import Data.Maybe
import Data.Array as A
import Data.List.Lazy (replicateM)
import Data.List.Lazy as LL
import Data.Either
import Control.Alt
import Control.Monad.Aff
import Control.Monad.Eff
import Control.Monad.Eff.Exception
import Control.Monad.Eff.Class
import Control.Monad.Trans.Class (lift)
import Control.Monad.State as State
import Neodoc.Data.Indexed
import Neodoc.Parsing.Parser hiding (error)
import Neodoc.Parsing.Parser.Combinators
import Neodoc.Parsing.Parser.Combinators as Parser
import Neodoc.Parsing.Parser as Parser
import Neodoc.ArgParser.Pattern

import Test.Assert (assert')
import Test.Spec (describe, describeOnly, it, itOnly)
import Test.Support

type TestError = String
type TestParser i a = Parser TestError {} Int Int i a

runTestParser :: _ -> _ -> _ -> _ -> TestParser _ _ -> Either String _
runTestParser a b c d x = lmap pretty $ runParser a b c d x

parseString x = Parser \a ->
  let _return = \i r -> Step true (setI i a) r
      _fail m = Step false a (Left $ ParseError false (Left m))
   in case getI a of
        s : ss | s == x -> _return ss (Right x)
        s : _ -> _fail $ "expected " <> x <> ", but got: " <> s
        _ -> _fail $ "expected " <> x

expectA
  :: ∀ r
   . (Eq r, Show r)
  => Number
  -> r
  -> Eff _ (Tuple Number r)
  -> Eff _ Unit
expectA eT eR m = do
  t /\ r <- m
  assertEqual eR r
  assert' ("Should run in under " <> show eT <> "ms, but took " <> show t <> "ms")
          (t <= eT)

expectFailureA
  :: String
  -> Either String _
  -> Eff _ Unit
expectFailureA eMsg (Left msg) | eMsg == msg = pure unit
expectFailureA _ (Left msg) = throwException $ error $ "Bad exception message: " <> show msg
expectFailureA _ (Right _) = throwException $ error $ "Missing failure"

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

    it "should honor alt instance" do
      liftEff do
        result <- runEitherEff do
          runTestParser {} 0 0 Nil do
            (Just <$> Parser.fail "boom") <|> (pure Nothing)
        assertEqual result (Nothing :: Maybe Unit)

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
