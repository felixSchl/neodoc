module Test.Spec.PatternParserSpec (patternParserSpec) where

import Prelude
import Debug.Trace
import Debug.Profile
import Data.Function
import Data.NonEmpty (NonEmpty, (:|))
import Data.Foldable (for_)
import Data.List hiding (many)
import Data.Tuple (Tuple, fst)
import Data.Bifunctor (rmap, lmap)
import Data.Tuple.Nested ((/\), Tuple3)
import Data.Newtype
import Data.Generic
import Data.String as String
import Data.Maybe
import Data.Pretty
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
import Neodoc.ArgParser.Pattern as Pattern

import Test.Assert (assert')
import Test.Spec (describe, describeOnly, it, itOnly)
import Test.Spec as Test
import Test.Support

type TestError = String
type TestParser i a = Parser TestError {} Int Int i a

runTestParser :: _ -> _ -> _ -> List _ -> TestParser _ _ -> Either String (List String)
runTestParser a b c d x = lmap pretty $ runParser a b c d x

match
  :: String
  -> List String
  -> AllowOmissions
  -> PatternMatch String String String
match s (i:is) _ | i == s = Right $ s /\ is
match s is true = Right $ ("<sub: " <> show s <> ">") /\ is
match s _ _ = Left $ false /\ ("Expected " <> show s)

parse :: _ -> _
parse = Pattern.parse match pretty

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
  :: ∀ a
   . (Pretty a)
  => String
  -> Either String a
  -> Eff _ Unit
expectFailureA eMsg (Left msg) | eMsg == msg = pure unit
expectFailureA eMsg (Left msg) = throwException $ error do
  "Bad exception message: " <> show msg <> " (as opposed to: " <> show eMsg <> ")"
expectFailureA _ (Right r) = throwException $ error do
  "Missing failure, got: " <> pretty r

patternParserSpec :: Unit -> Test.Spec _ _
patternParserSpec = \_ ->
  describe "parse patterns" do

    it "should handle substitutions" do
      liftEff do
        expectFailureA "Expected L!(b)!" do
          runTestParser {} 0 0 (fromFoldable [ ]) do
            parse $ fromFoldable do
              [ choiz [[ leafOF "c", leafF "b", leafOF "a" ]] ]

        expectA 10.0 (fromFoldable [ "<sub: \"c\">", "b", "<sub: \"a\">" ]) do
          measureA \_ -> runEitherEff do
            runTestParser {} 0 0 (fromFoldable [ "b" ]) do
              parse $ fromFoldable do
                [ choiz [[ leafOF "c", leafF "b", leafOF "a" ]] ]

    it "omits gracefully" do
      liftEff do
        expectA 10.0 (fromFoldable [ "a", "b", "c" ]) do
          measureA \_ -> runEitherEff do
            runTestParser {} 0 0 (fromFoldable ["a", "b", "c"]) do
              parse $ fromFoldable do
                [ choizO [[ leafO "c" ]], leaf "b", leaf "a" ]

    it "omits gracefully in subgroups" do
      liftEff do
        expectA 15.0 (fromFoldable [ "a", "b", "c", "<sub: \"d\">" ]) do
          measureA \_ -> runEitherEff do
            runTestParser {} 0 0 (fromFoldable ["a", "b", "c"]) do
              parse $ fromFoldable do
                [ choizO [[ leafO "c", leafO "d" ]], leaf "b", leaf "a" ]

    it "omits gracefully in subgroups" do
      liftEff do
        expectA 15.0 (fromFoldable [ "a", "b", "c", "<sub: \"d\">" ]) do
          measureA \_ -> runEitherEff do
            runTestParser {} 0 0 (fromFoldable ["a", "b", "c"]) do
              parse $ fromFoldable do
                [ choizO [[ leafO "c", leafO "d" ]], leaf "b", leaf "a" ]

    it "should exhaust the patterns" do
      liftEff do
        expectA 5.0 (fromFoldable [ "b", "b", "b", "a", "b"]) do
          measureA \_-> runEitherEff do
            runTestParser {} 0 0 (fromFoldable [ "b", "b", "b", "a", "b" ]) do
              parse $ fromFoldable do
                [ leafR "b", leaf "a" ]

    it "should choose the best match" do
      liftEff do
        expectA 5.0 (fromFoldable [ "a", "b", "c" ]) do
          measureA \_-> runEitherEff do
            runTestParser {} 0 0 (fromFoldable [ "a", "b", "c" ]) do
              parse $ fromFoldable do
                [ choiz [
                  [ leafO "a", leafO "b", leaf "c" ]
                , [ leaf "a", leaf "b", leaf "c" ]
                ] ]

    it "unexpected b" do
      liftEff do
        expectFailureA "Unexpected b" do
          runTestParser {} 0 0 (fromFoldable [ "b", "a", "b" ]) do
            parse $ fromFoldable do
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
              parse $ fromFoldable do
                [ choizORF [[ leaf "a", leaf "b" ]], leafR "c" ]

    it "should respect fixed optionals" do
      liftEff do
        -- We expect "Unexpected b" here because each item fails and gets
        -- omitted except for "c", which then parses the first "c", but has
        -- trailing input "b ..."
        expectFailureA "Unexpected b" do
          runTestParser {} 0 0 (fromFoldable [ "c", "b", "a" ]) do
            parse $ fromFoldable do
              [ leafOF "a", leafOF "b", leafOF "c" ]

    it "should respect fixed, repeatable, optional choices" do
      liftEff do
        expectA 5.0 (fromFoldable [ "c" ]) do
          measureA \_-> runEitherEff do
            runTestParser {} 0 0 (fromFoldable [ "c" ]) do
              parse $ fromFoldable do
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
      , s choizRF  leaf /\ id /\ y /\ "all repeatable & fixed choices"
      , s choizORF leaf /\ id /\ y /\ "all optional & repeatable & fixed choices"

      , s choiz    leaf /\ reverse /\ y /\ "all (reversed) choices"
      , s choizO   leaf /\ reverse /\ y /\ "all (reversed) optional choices"
      , s choizR   leaf /\ reverse /\ y /\ "all (reversed) repeatable choices"
      , s choizOR  leaf /\ reverse /\ y /\ "all (reversed) optional & repeatable choices"

      , s choiz    leafORF /\ reverse /\ y /\ "all (reversed) choices (with fixed & optional & repeateable single leaf)"
      , s choizO   leafORF /\ reverse /\ y /\ "all (reversed) optional choices (with fixed & optional & repeateable single leaf)"
      , s choizR   leafORF /\ reverse /\ y /\ "all (reversed) repeatable choices (with fixed & optional & repeateable single leaf)"
      , s choizOR  leafORF /\ reverse /\ y /\ "all (reversed) optional & repeatable choices (with fixed & optional & repeateable single leaf)"

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
                  parse $ fromFoldable do
                    f <$> mod abc
            Right exResult ->
              expectA 75.0 exResult do
                measureA \_-> runEitherEff do
                  runTestParser {} 0 0 abc do
                    parse $ fromFoldable do
                      f <$> mod abc
