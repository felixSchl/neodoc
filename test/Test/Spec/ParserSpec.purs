module Test.Spec.ParserSpec (parserSpec) where

import Prelude
import Debug.Trace
import Data.Function
import Data.List hiding (many)
import Data.Tuple (Tuple, fst)
import Data.Tuple.Nested ((/\), Tuple3)
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

type IsOptional = Boolean
type IsRepeatable = Boolean
type IsFixed = Boolean
type HasMoved = Boolean

data Pattern a = LeafPattern   IsOptional IsRepeatable IsFixed a
               | ChoicePattern IsOptional IsRepeatable IsFixed (List (List (Pattern a)))

derive instance genericPattern :: (Generic a) => Generic (Pattern a)
instance showPattern :: (Generic a, Show a) => Show (Pattern a) where
  show = gShow

instance prettyPattern :: (Pretty a) => Pretty (Pattern a) where
  pretty (LeafPattern r o f a) = "L"
    <>  (if f then "!" else "*")
    <>  (if o then "[" else "(")
    <>  (pretty a <> (if r then "..." else ""))
    <>  (if o then "]" else ")")
    <>  (if r then "..." else "")
    <>  (if f then "!" else "*")
  pretty (ChoicePattern r o f as) = "C"
    <>  (if f then "!" else "*")
    <>  (if o then "[" else "(")
    <>  (intercalate " | " $ ((intercalate " " <<< (pretty <$> _)) <$> _) as)
    <>  (if o then "]" else ")")
    <>  (if r then "..." else "")
    <>  (if f then "!" else "*")

isOptional :: ∀ a. Pattern a -> Boolean
isOptional (LeafPattern o _ _ _) = o
isOptional (ChoicePattern o _ _ _) = o

isRepeatable :: ∀ a. Pattern a -> Boolean
isRepeatable (LeafPattern _ r _ _) = r
isRepeatable (ChoicePattern _ r _ _) = r

isFixed :: ∀ a. Pattern a -> Boolean
isFixed (LeafPattern _ _ f _) = f
isFixed (ChoicePattern _ _ f _) = f

leaf :: ∀ a. a -> Pattern a
leaf = LeafPattern false false false

leafR :: ∀ a. a -> Pattern a
leafR = LeafPattern false true false

leafF :: ∀ a. a -> Pattern a
leafF = LeafPattern false false true

leafRF :: ∀ a. a -> Pattern a
leafRF = LeafPattern false true true

leafO :: ∀ a. a -> Pattern a
leafO = LeafPattern true false false

leafOR :: ∀ a. a -> Pattern a
leafOR = LeafPattern true true false

leafOF :: ∀ a. a -> Pattern a
leafOF = LeafPattern true false true

leafORF :: ∀ a. a -> Pattern a
leafORF = LeafPattern true true true

choiz :: ∀ a. Array (Array (Pattern a)) -> Pattern a
choiz xs = ChoicePattern false false false $ fromFoldable (fromFoldable <$> xs)

choizR :: ∀ a. Array (Array (Pattern a)) -> Pattern a
choizR xs = ChoicePattern false true false $ fromFoldable (fromFoldable <$> xs)

choizF :: ∀ a. Array (Array (Pattern a)) -> Pattern a
choizF xs = ChoicePattern false false true $ fromFoldable (fromFoldable <$> xs)

choizRF :: ∀ a. Array (Array (Pattern a)) -> Pattern a
choizRF xs = ChoicePattern false true true $ fromFoldable (fromFoldable <$> xs)

choizO :: ∀ a. Array (Array (Pattern a)) -> Pattern a
choizO xs = ChoicePattern true false false $ fromFoldable (fromFoldable <$> xs)

choizOR :: ∀ a. Array (Array (Pattern a)) -> Pattern a
choizOR xs = ChoicePattern true true false $ fromFoldable (fromFoldable <$> xs)

choizOF :: ∀ a. Array (Array (Pattern a)) -> Pattern a
choizOF xs = ChoicePattern true false true $ fromFoldable (fromFoldable <$> xs)

choizORF :: ∀ a. Array (Array (Pattern a)) -> Pattern a
choizORF xs = ChoicePattern true true true $ fromFoldable (fromFoldable <$> xs)

type Result a u = Tuple (List a)
                        (Tuple (List (Pattern u))
                                HasMoved)

parse
  :: ∀ i e c s g u a
   . (Generic u, Show u, Pretty u, Show a, Pretty i)
  => (u -> Parser e c s g (List i) a)
  -> List (Pattern u) -- input patterns
  -> Parser e c s g (List i) (List a)
parse f pats = do
  vs <- fst <$> parsePatterns f Nil pats
  is <- getInput
  case is of
    Nil   -> Parser.return vs
    i : _ -> Parser.fail $ "Unexpected " <> pretty i

parsePatterns
  :: ∀ i e c s g u a
   . (Generic u, Show u, Pretty u, Show a)
  => (u -> Parser e c s g (List i) a)
  -> List (Pattern u) -- repeatables
  -> List (Pattern u) -- input patterns
  -> Parser e c s g (List i) (Result a u)
parsePatterns f repPats pats =
  let xs = indexed pats
      f' reps pat = do
        (vs /\ reps' /\ hasMoved) <- case pat of
                LeafPattern _ r _ x -> do
                  vs <- singleton <$> f x
                  pure (vs /\ reps /\ true)
                ChoicePattern _ _ _ xs -> do
                  evalParsers'
                    (const 1 {- TODO -})
                    (const 0 {- TODO -})
                    (const Nothing {- TODO -})
                    (\_ _ -> pure unit {- TODO -})
                    (parsePatterns f reps <$> xs)
        let reps'' = if isRepeatable pat then pat : reps' else reps'
        pure (vs /\ reps'' /\ hasMoved)
   in do
        -- 1. parse the pattern wholly
        (vs /\ rep /\ hasMoved) <- go f' xs xs Nil false repPats Nil

        -- 2. consume any trailing arguments using the repeating patterns we
        --    learned about so far.
        --    note: parsing the remainder may have consumed input, since we
        --          never substitute values during rep parsing.
        vs' <- parseRemainder rep ((fst <$> _) <<< f' rep)

        pure $ (vs <> vs') /\ rep /\ (hasMoved || (not $ null vs'))
  where
  go
    :: _                  -- the parser function on elements of `u`
    -> _                  -- the original input for given iteration
    -> _                  -- the patterns to match
    -> _                  -- the carry
    -> HasMoved           -- have we consumed any input this far?
    -> (List (Pattern u)) -- repeatable patterns found so far
    -> (List a)           -- output values
    -> Parser e c s g (List i) (Result a u)
  go _ _ Nil Nil hasMoved reps out = Parser.return $ out /\ reps /\ hasMoved

  go _ _ Nil ((Indexed _ pat):_) _ _ _ = do
    -- at this point we rotated the entire input and tried to consume via
    -- repetitions, but w/o any luck. it's time for drastic measures (TODO)

    Parser.fail $ "Expected " <> pretty pat
  go f' orig (x@(Indexed ix pat):xs) carry hasMoved reps out = do
    -- 1. try parsing the pattern
    mR <- (Just <$> f' reps pat) <|> (pure Nothing)
    case mR of
      -- if we have a result, and the result consumed input, we're looking
      -- good. we reset the input pattern bar the matched element, clear
      -- the carry and add this pattern to the list of repeated patterns,
      -- (if this pattern allows for repetition)
      Just (result /\ reps' /\ true) ->
        let orig' = sortBy (compare `on` getIndex) do
                      filter (not <<< (_ == ix) <<< getIndex) orig
         in go f' orig' orig' Nil true reps' (result <> out)

      -- 2. if we did not manage to make a match, we will try to make a match
      --    using all previously matched, repeatable patterns.
      _ -> do
        mR' <- (Just <$> choice (f' Nil <$> reps)) <|> (pure Nothing)
        case mR' of
          -- if we have a result, and the result consumed input, we're looking
          -- ok. we reset the input pattern completely, clear the carry and
          -- re-iterate using the input.
          Just (result' /\ _ /\ true) ->
            let orig' = sortBy (compare `on` getIndex) do
                          filter (not <<< (_ == ix) <<< getIndex) orig
             in go f' orig' orig' Nil true reps (result' <> out)

          -- 3. if, at this point, we still did not manage to make a match, we
          --    rotate this pattern into the carry and give the next pattern a
          --    chance.
          _ -> do
            if not $ isFixed pat
              then go f' orig xs (x:carry) hasMoved reps out
              else
                let mIx = findIndex (isFixed <<< getIndexedElem) carry
                    orig' = case mIx of
                              Nothing -> orig
                              Just i -> case insertAt i x carry of
                                          Just xs -> xs
                                          Nothing -> snoc orig x
                 in go f' orig' xs (x:carry) hasMoved reps out

parseRemainder
  :: ∀ i e c s g u a
   . List u -- repeatables
  -> (u -> Parser e c s g (List i) (List a))
  -> Parser e c s g (List i) (List a)
parseRemainder repPats f = do
  go (choice $ f <$> repPats) Nil
  where go p xs = do
          vs <- p <|> pure Nil
          if null vs then pure xs else go p (vs <> xs)

parseString x = Parser \a ->
  let _return = \i r -> Step true (setI i a) r
      _fail m = Step false a (Left $ ParseError false (Left m))
   in case getI a of
        s : ss | s == x -> _return ss (Right x)
        s : _ -> _fail $ "expected " <> x <> ", but got: " <> s
        _ -> _fail $ "expected " <> x

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
          traceShowA =<< runEitherEff do
            runTestParser {} 0 0 (fromFoldable [ "b", "a", "b" ]) do
              parse parseString $ fromFoldable do
                [ choizOR [[ leaf "b" ]], leaf "a" ]

      it "unexpected b" do
        liftEff do
          traceShowA =<< runEitherEff do
            runTestParser {} 0 0 (fromFoldable [ "b", "a", "b" ]) do
              parse parseString $ fromFoldable do
                [ choizORF [[ leaf "a" ]], leaf "b" ]
