module Test.Spec.ParserSpec (parserSpec) where

import Prelude
import Debug.Trace
import Data.List hiding (many)
import Data.Tuple (Tuple, fst)
import Data.Tuple.Nested ((/\))
import Data.Newtype
import Data.Generic
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
import Neodoc.ArgParser.Evaluate (evalParsers')

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

derive instance genericPattern :: (Generic a) => Generic (Pattern a)
instance showPattern :: (Generic a, Show a) => Show (Pattern a) where
  show = gShow

instance prettyPattern :: (Pretty a) => Pretty (Pattern a) where
  pretty (LeafPattern r _ a) = pretty a <> (if r then "..." else "")
  pretty (ChoicePattern r _ as) = intercalate " | " do
    ((intercalate " " <<< (pretty <$> _)) <$> _) as

isRepeatable :: ∀ a. Pattern a -> Boolean
isRepeatable (LeafPattern r _ _) = r
isRepeatable (ChoicePattern r _ _) = r

leaf :: ∀ a. a -> Pattern a
leaf = LeafPattern false false

leafR :: ∀ a. a -> Pattern a
leafR = LeafPattern true false

leafF :: ∀ a. a -> Pattern a
leafF = LeafPattern false true

leafRF :: ∀ a. a -> Pattern a
leafRF = LeafPattern true true

choiz :: ∀ a. Array (Array (Pattern a)) -> Pattern a
choiz xs = ChoicePattern false false $ fromFoldable (fromFoldable <$> xs)

choizR :: ∀ a. Array (Array (Pattern a)) -> Pattern a
choizR xs = ChoicePattern true false $ fromFoldable (fromFoldable <$> xs)

choizF :: ∀ a. Array (Array (Pattern a)) -> Pattern a
choizF xs = ChoicePattern false true $ fromFoldable (fromFoldable <$> xs)

choizRF :: ∀ a. Array (Array (Pattern a)) -> Pattern a
choizRF xs = ChoicePattern true true $ fromFoldable (fromFoldable <$> xs)

parsePatterns
  :: ∀ i e c s g u a
   . (Generic u, Show u, Pretty u, Show a)
  => (u -> Parser e c s g (List i) a)
  -> List (Pattern u) -- repeatables
  -> List (Pattern u) -- input patterns
  -> Parser e c s g (List i) (Tuple (List a) (List (Pattern u)))
parsePatterns f repPats pats =
  let xs = indexed pats
      f' reps = case _ of
                  p@(LeafPattern r _ x) -> do
                    v <- singleton <$> f x
                    pure (v /\ if r then p : reps else reps)
                  ChoicePattern _ _ xs -> do
                    evalParsers'
                      (const 1 {- TODO -})
                      (const 0 {- TODO -})
                      (const Nothing {- TODO -})
                      (\_ _ -> pure unit {- TODO -})
                      (parsePatterns f reps <$> xs)
   in do
        (vs /\ rep) <- go f' xs xs Nil repPats Nil
        vs' <- parseRemainder rep ((fst <$> _) <<< f' rep)
        pure ((vs <> vs') /\ rep)
  where
  go
    :: _ -> _ -> _ -> _
    -> (List (Pattern u))
    -> (List a)
    -> Parser e c s g (List i) (Tuple (List a) (List (Pattern u)))
  go _ _ Nil Nil rep out = Parser.return $ out /\ rep
  go _ _ Nil ((Indexed _ pat):_) _ _ = Parser.fail do
    "Expected " <> pretty pat
  go f' orig (x@(Indexed ix pat):xs) carry reps out = do
    mR <- (Just <$> f' reps pat) <|> (pure Nothing)
    case mR of
      Just (result /\ reps') ->
        let orig' = filter (not <<< (_ == ix) <<< getIndex) orig
         in go f' orig' orig' Nil reps' (result <> out)
      Nothing -> do
        -- TODO: is `f' reps <$> reps` correct?
        mR' <- (Just <$> choice (f' reps <$> reps)) <|> (pure Nothing)
        case mR' of
          Just (result' /\ _) ->
            let orig' = filter (not <<< (_ == ix) <<< getIndex) orig
             in go f' orig' orig' Nil reps (result' <> out)
          Nothing -> go f' orig xs (x:carry) reps out

parseRemainder
  :: ∀ i e c s g u a
   . List u -- repeatables
  -> (u -> Parser e c s g (List i) (List a))
  -> Parser e c s g (List i) (List a)
parseRemainder repPats f = concat <$> many do
                            choice do
                              f <$> repPats

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
                (\x -> Parser \a ->
                  let _return = \i r -> Step true (setI i a) r
                      _fail m = Step false a (Left $ ParseError false (Left m))
                   in case getI a of
                        s : ss | s == x -> _return ss (Right x)
                        s : _ -> _fail $ "expected " <> x <> ", but got: " <> s
                        _ -> _fail $ "expected " <> x
                )
                Nil
                (fromFoldable [ leafR "b", leaf "a" ])
              is <- getInput
              case is of
                Nil   -> Parser.return result
                i : _ -> Parser.fail $ "Unexpected " <> pretty i
          traceShowA result
