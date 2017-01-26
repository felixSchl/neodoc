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

    -- TODO: should be it's own spec
    describe "parse patterns" do

      it "Expect" do
        liftEff do
          expectFailureA "Expected L!(c)!" do
            runTestParser {} 0 0 (fromFoldable ["a", "b", "c"]) do
              parse parseString $ fromFoldable do
                [ leafF "c", leafF "b", leafF "a" ]

      it "should exhaust the patterns" do
        liftEff do
          expectA 5.0 (fromFoldable [ "b", "b", "b", "a", "b"]) do
            measureA \_-> runEitherEff do
              runTestParser {} 0 0 (fromFoldable [ "b", "b", "b", "a", "b" ]) do
                parse parseString $ fromFoldable do
                  [ leafR "b", leaf "a" ]

      it "unexpected b" do
        liftEff do
          expectFailureA "Unexpected b" do
            runTestParser {} 0 0 (fromFoldable [ "b", "a", "b" ]) do
              parse parseString $ fromFoldable do
                [ choizORF [[ leaf "a" ]], leaf "b" ]

      it "should respect fixed, repeatable, optional choices" do
        liftEff do
          expectA 5.0
                  (fromFoldable [
                    "b", "a", "c", "b", "a",
                    "b", "a", "c", "b", "a"
                    ])
                  do
            measureA \_-> runEitherEff do
              runTestParser {} 0 0 (fromFoldable [
                "b", "a", "c", "b", "a",
                "b", "a", "c", "b", "a"
                ]) do
                parse parseString $ fromFoldable do
                  [ choizORF [[ leaf "a", leaf "b" ]], leafR "c" ]

      it "should respect fixed optionals" do
        liftEff do
          -- We expect "Unexpected b" here because each item fails and gets
          -- omitted except for "c", which then parses the first "c", but has
          -- trailing input "b ..."
          expectFailureA "Unexpected b" do
            runTestParser {} 0 0 (fromFoldable [ "c", "b", "a" ]) do
              parse parseString $ fromFoldable do
                [ leafOF "a", leafOF "b", leafOF "c" ]

      it "should respect fixed, repeatable, optional choices" do
        liftEff do
          expectA 5.0 (fromFoldable [ "c" ]) do
            measureA \_-> runEitherEff do
              runTestParser {} 0 0 (fromFoldable [ "c" ]) do
                parse parseString $ fromFoldable do
                  [ choizORF [[ leaf "a", leaf "b" ]], leafR "c" ]

      describe "the abcs" do

        let abc = fromFoldable [
                    "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l"
                  , "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x"
                  , "y", "z"
                  ]
            y = Right abc
            n = Left
            s f l = f <<< A.singleton <<< A.singleton <<< l

        for_ [

          leaf    /\ id /\ y /\ "all leafs"
        , leafO   /\ id /\ y /\ "all optional leafs"
        , leafR   /\ id /\ y /\ "all repeatable leafs"
        , leafF   /\ id /\ y /\ "all fixed leafs"
        , leafOR  /\ id /\ y /\ "all optional & repeatable leafs"
        , leafOF  /\ id /\ y /\ "all optional & fixed leafs"
        , leafRF  /\ id /\ y /\ "all fixed leafs & repeatable leafs"
        , leafORF /\ id /\ y /\ "all optional & repeatable & fixed leafs"

        , leaf    /\ reverse /\ y /\ "all (reversed) leafs"
        , leafO   /\ reverse /\ y /\ "all (reversed) optional leafs"
        , leafR   /\ reverse /\ y /\ "all (reversed) repeatable leafs"
        , leafOR  /\ reverse /\ y /\ "all (reversed) optional & repeatable leafs"

        , leafF   /\ reverse /\ n "Expected L!(z)!"    /\ "all (reversed) fixed leafs"
        , leafRF  /\ reverse /\ n "Expected L!(z)...!" /\ "all (reversed) fixed leafs & repeatable leafs"
        , leafOF  /\ reverse /\ n "Unexpected b"       /\ "all (reversed) optional & fixed leafs"
        , leafORF /\ reverse /\ n "Unexpected b"       /\ "all (reversed) optional & repeatable & fixed leafs"

        , s choiz    leaf /\ id /\ y /\ "all choices"
        , s choizO   leaf /\ id /\ y /\ "all optional choices"
        , s choizR   leaf /\ id /\ y /\ "all repeatable choices"
        , s choizF   leaf /\ id /\ y /\ "all fixed choices"
        , s choizOR  leaf /\ id /\ y /\ "all optional & repeatable choices"
        , s choizOF  leaf /\ id /\ y /\ "all optional & fixed choices"
        , s choizRF  leaf /\ id /\ y /\ "all fixed choices & repeatable choices"
        , s choizORF leaf /\ id /\ y /\ "all optional & repeatable & fixed choices"

        , s choiz    leaf /\ reverse /\ y /\ "all choices"
        , s choizO   leaf /\ reverse /\ y /\ "all optional choices"
        , s choizR   leaf /\ reverse /\ y /\ "all repeatable choices"
        , s choizOR  leaf /\ reverse /\ y /\ "all optional & repeatable choices"

          -- TODO: these can cause stack 
        -- , s choiz    leafORF /\ reverse /\ y /\ "all choices (with fixed & optional & repeateable single leaf)"
        -- , s choizO   leafORF /\ reverse /\ y /\ "all optional choices (with fixed & optional & repeateable single leaf)"
        -- , s choizR   leafORF /\ reverse /\ y /\ "all repeatable choices (with fixed & optional & repeateable single leaf)"
        , s choizOR  leafORF /\ reverse /\ y /\ "all optional & repeatable choices (with fixed & optional & repeateable single leaf)"

        , s choizF   leaf /\ reverse /\ n "Expected C!(L*(z)*)!"    /\ "all (reversed) fixed choices"
        , s choizRF  leaf /\ reverse /\ n "Expected C!(L*(z)*)...!" /\ "all (reversed) fixed choices & repeatable choices"
        , s choizOF  leaf /\ reverse /\ n "Unexpected b"            /\ "all (reversed) optional & fixed choices"
        , s choizORF leaf /\ reverse /\ n "Unexpected b"            /\ "all (reversed) optional & repeatable & fixed choices"

        ] \(f /\ mod /\ ex /\ title) -> do
          let title' = title <> case ex of
                Left msg -> ": fail with " <> show msg
                Right _ -> ""
          it title' $ liftEff do
            case ex of
              Left exErrmsg ->
                expectFailureA exErrmsg do
                  runTestParser {} 0 0 abc do
                    parse parseString $ fromFoldable do
                      f <$> mod abc
              Right exResult ->
                expectA 75.0 exResult do
                  measureA \_-> runEitherEff do
                    runTestParser {} 0 0 abc do
                      parse parseString $ fromFoldable do
                        f <$> mod abc
