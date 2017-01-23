module Test.Spec.ParserSpec (parserSpec) where

import Prelude
import Debug.Trace
import Data.List
import Data.Newtype
import Data.Pretty
import Data.Maybe
import Data.List.Lazy (replicateM)
import Data.Either
import Control.Alt
import Control.Monad.Aff
import Control.Monad.Eff.Class
import Control.Monad.Trans.Class (lift)
import Control.Monad.State as State
import Neodoc.Data.Indexed
import Neodoc.Parsing.Parser
import Neodoc.Parsing.Parser.Combinators
import Neodoc.Parsing.Parser as Parser

import Test.Assert (assert)
import Test.Spec (describe, it)
import Test.Support

type TestError = String
type TestParser i a = Parser TestError {} Int Int i a

runTestParser :: _ -> _ -> _ -> _ -> TestParser _ _ -> _
runTestParser = runParser

type IsRepeatable = Boolean
type IsFixed = Boolean

data Pattern a = LeafPattern IsRepeatable IsFixed a
               | ChoicePattern IsRepeatable IsFixed (List (List (Pattern a)))

isRepeatable :: ∀ a. Pattern a -> Boolean
isRepeatable (LeafPattern r _ _) = r
isRepeatable (ChoicePattern r _ _) = r

instance prettyPattern :: (Pretty a) => Pretty (Pattern a) where
  pretty (LeafPattern r _ a) = pretty a <> (if r then "..." else "")
  pretty (ChoicePattern r _ as) = intercalate " | " do
    ((intercalate " " <<< (pretty <$> _)) <$> _) as

leaf :: ∀ a. a -> Pattern a
leaf = LeafPattern false false

leafR :: ∀ a. a -> Pattern a
leafR = LeafPattern true false

leafF :: ∀ a. a -> Pattern a
leafF = LeafPattern false true

leafRF :: ∀ a. a -> Pattern a
leafRF = LeafPattern true true

parsePatterns
  :: ∀ i e c s g u a
   . (Pretty u)
  => List (Pattern u) -- input patterns
  -> List (Pattern u) -- repeatables
  -> (Pattern u -> Parser e c s g i a)
  -> Parser e c s g i (List a)
parsePatterns pats repPats f =
  let xs = indexed pats
   in go xs xs Nil repPats Nil
  where
  go orig Nil Nil _ out = Parser.return out
  go orig Nil ((Indexed _ pat):_) _ _ = Parser.fail do
    "Expected " <> pretty pat
  go orig (x@(Indexed ix pat):xs) carry reps out = do
    mR <- (Just <$> f pat) <|> (pure Nothing)
    case mR of
      Just result ->
        let orig' = filter (not <<< (_ == ix) <<< getIndex) orig
            reps' = if isRepeatable pat then pat : reps else reps
         in go orig' orig' Nil reps' out
      Nothing -> do
        mR' <- (Just <$> choice (f <$> reps)) <|> (pure Nothing)
        case mR' of
          Just result' ->
            let orig' = filter (not <<< (_ == ix) <<< getIndex) orig
             in go orig' orig' Nil reps out
          Nothing -> go orig xs (x:carry) reps out

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

    -- TODO: should be it's own spec
    describe "parse patterns" do
      it "should exhaust the patterns" do
        liftEff do
          result <- runEitherEff do
            runTestParser {} 0 0 (fromFoldable [ "b", "a", "b" ]) do
              result <- parsePatterns
                (fromFoldable [ leafR "b", leaf "a" ])
                Nil
                (case _ of
                  ChoicePattern _ _ _ -> Parser.fail "not implemented"
                  LeafPattern r f x ->
                    Parser \a ->
                      let _return = \i r -> Step true (setI i a) r
                          _fail m = Step false a (Left $ ParseError false (Left m))
                      in case getI a of
                        s : ss | s == x -> _return ss (Right r)
                        s : _ -> _fail $ "expected " <> x <> ", but got: " <> s
                        _ -> _fail $ "expected " <> x
                )
              is <- getInput
              case is of
                Nil   -> Parser.return result
                i : _ -> Parser.fail $ "Unexpected " <> pretty i
          traceShowA result
