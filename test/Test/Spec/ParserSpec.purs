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
import Neodoc.ArgParser.Evaluate (evalParsers')

import Test.Assert (assert')
import Test.Spec (describe, describeOnly, it, itOnly)
import Test.Support

type TestError = String
type TestParser i a = Parser TestError {} Int Int i a

runTestParser :: _ -> _ -> _ -> _ -> TestParser _ _ -> Either String _
runTestParser a b c d x = lmap pretty $ runParser a b c d x

type IsOptional = Boolean
type IsRepeatable = Boolean
type IsFixed = Boolean
type IsLocked = Boolean
type HasMoved = Boolean
type AllowRepetitions = Boolean

data Pattern a = LeafPattern   IsOptional IsRepeatable IsFixed a
               | ChoicePattern IsOptional IsRepeatable IsFixed (List (List (Pattern a)))

derive instance genericPattern :: (Generic a) => Generic (Pattern a)
instance showPattern :: (Generic a, Show a) => Show (Pattern a) where
  show = gShow

instance prettyPattern :: (Pretty a) => Pretty (Pattern a) where
  pretty (LeafPattern o r f a) = "L"
    <>  (if f then "!" else "*")
    <>  (if o then "[" else "(")
    <>  (pretty a)
    <>  (if o then "]" else ")")
    <>  (if r then "..." else "")
    <>  (if f then "!" else "*")
  pretty (ChoicePattern o r f as) = "C"
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
   . (Generic u, Show u, Show a, Pretty u, Pretty a, Pretty i)
  => (u -> Parser e c s g (List i) a)
  -> List (Pattern u) -- input patterns
  -> Parser e c s g (List i) (List a)
parse f pats = do
  vs <- fst <$> parsePatterns 0 f Nil pats
  is <- getInput
  case is of
    Nil   -> Parser.return vs
    i : _ -> Parser.fail $ "Unexpected " <> pretty i

indent :: Int -> String
indent l = String.fromCharArray $ LL.toUnfoldable $ LL.take (l * 4) $ LL.repeat ' '

requireMovement p = do
  a /\ b /\ hasMoved <- p
  if not hasMoved
     then Parser.fail "no movement ..."
     else pure $ a /\ b

parsePatterns
  :: ∀ i e c s g u a
   . (Generic u, Show u, Show a, Pretty u, Pretty a, Pretty i)
  => Int
  -> (u -> Parser e c s g (List i) a)
  -> List (Pattern u) -- repeatables
  -> List (Pattern u) -- input patterns
  -> Parser e c s g (List i) (Result a u)
parsePatterns l f repPats pats =
  let xs = indexed pats
      f' reps pat = do
        vs /\ reps' /\ hasMoved <- case pat of
                LeafPattern _ r _ x -> do
                  vs <- singleton <$> f x
                  pure (vs /\ reps /\ true)
                ChoicePattern _ _ _ xs ->
                  let resetReps = rmap (lmap (const reps))
                   in resetReps <$> do
                        evalParsers'
                          (const 1 {- TODO -})
                          (const 0 {- TODO -})
                          (const Nothing {- TODO -})
                          (\_ _ -> pure unit {- TODO -})
                          -- TODO: do we need to pass `reps` down?
                          (parsePatterns (l + 1) f Nil <$> xs)
        let reps'' = if isRepeatable pat then pat : reps' else reps'
        pure (vs /\ reps'' /\ hasMoved)
   in do
        -- 1. parse the pattern wholly
        vs /\ rep /\ hasMoved <- go f' xs xs Nil false false false repPats Nil

        -- 2. consume any trailing arguments using the repeating patterns we
        --    learned about so far.
        --    note: parsing the remainder may have consumed input, since we
        --          never substitute values during rep parsing.
        --    note: we ignore any newly learned reps at this point (TODO: is
        --          this correct?)
        vs' <- parseRemainder rep ((fst <$> _) <<< f' rep)

        pure $ (vs <> vs') /\ rep /\ (hasMoved || (not $ null vs'))
  where
  go
    :: _                  -- the parser function on elements of `u`
    -> _                  -- the original input for given iteration
    -> _                  -- the patterns to match
    -> _                  -- the carry
    -> AllowRepetitions   -- allow repetitions?
    -> HasMoved           -- have we consumed any input this far?
    -> IsLocked           -- are we locked from processing any more fixed?
    -> List (Pattern u)   -- repeatable patterns found so far
    -> List a             -- output values
    -> Parser e c s g (List i) (Result a u)
  -- Success!
  go _ _ Nil Nil _ hasMoved _ reps out = do
    Parser.return $ out /\ reps /\ hasMoved

  -- Failure
  go f' _ Nil ((Indexed _ pat):_) true true _ _ _ = do
    Parser.fail $ "Expected " <> pretty pat

  -- Failure: try again, this time allow repetitions
  go f' orig Nil (_:_) false _ _ reps out = do
    go f' orig orig Nil true false false reps out

  -- Failure: try again, this time start dropping
  go f' _ Nil (carry@(((Indexed _ pat):_))) true _ _ reps out = do
    -- at this point we rotated the entire input and tried to consume via
    -- repetitions, but w/o any luck. it's time for drastic measures by starting
    -- to remove optional elements, ony be one.

    -- TODO: this might need more logic as to which element we throw out first
    --       the current approach is simply removes from left to right.
    let sortedCarry = sortBy (compare `on` getIndex) carry

    case dropFirst (isOptional <<< getIndexedElem) sortedCarry of
      Just carry' -> go f' carry' carry' Nil false false false reps out <|> do
                        Parser.fail $ "Expected " <> pretty pat
      Nothing -> Parser.fail $ "Expected " <> pretty pat

  -- Step: ignore fixed patterns when locked
  go f' orig (x@(Indexed ix pat):xs) carry allowReps hasMoved true reps out | isFixed pat = do
    go f' orig xs (snoc carry x) allowReps hasMoved true reps out

  -- Step: process next element
  go f' orig (x@(Indexed ix pat):xs) carry allowReps hasMoved isLocked reps out = do
    -- 1. try parsing the pattern
    mR <- (Just <$> (Parser.try $ requireMovement (f' reps pat))) <|> (pure Nothing)
    case mR of
      -- if we have a result, and the result consumed input, we're looking
      -- good. we reset the input pattern bar the matched element, clear
      -- the carry and add this pattern to the list of repeated patterns,
      -- (if this pattern allows for repetition). We also reset `allowReps` to
      -- to treat the rest of the pattern as fresh input.
      Just (result /\ reps') -> do
        -- before attempting to parse the next element, we try to eagerly parse
        -- adjacent matches of the same pattern.
        -- note: we do not check `allowReps` because we want adjacent
        --       repetitions to be done eagerly.
        vs' <- if isRepeatable pat
                then parseRemainder (singleton pat) ((fst <$> _) <<< f' reps)
                else pure Nil
        let orig' = sortBy (compare `on` getIndex) do
                      filter (not <<< (_ == ix) <<< getIndex) orig
        go f' orig' orig' Nil false true isLocked reps' (out <> result <> vs')

      -- 2. if we did not manage to make a match, we will try to make a match
      --    using all previously matched, repeatable patterns, provided we are
      --    allowed to do so (`allowReps`).
      _ -> do
        mR' <- if allowReps
                  then (Just <$> choice (Parser.try <<< requireMovement
                                                    <<< f' Nil <$> reps))
                          <|> (pure Nothing)
                  else pure Nothing
        case mR' of
          -- if we have a result, and the result consumed input, we're looking
          -- ok. we reset the input pattern completely, clear the carry and
          -- re-iterate using the new input. Again, we reset `allowReps` to
          -- `false` in order to treat the resulting input as a fresh pattern.
          Just (result' /\ _) ->
            let orig' = sortBy (compare `on` getIndex) do
                          filter (not <<< (_ == ix) <<< getIndex) orig
             in go f' orig' orig' Nil false true isLocked reps (out <> result')

          -- 3. if, at this point, we still did not manage to make a match, we
          --    rotate this pattern into the carry and give the next pattern a
          --    chance.
          _ -> do
            -- if this pattern is fixed in place we have to make sure not to
            -- place it in front of a previously failed fixed pattern, since
            -- fixed patterns must be parsed in consecutive order.
            go f' orig xs (x:carry) allowReps hasMoved (isFixed pat) reps out

{-
  drop the first matching element from the list and return the
  resulting list or nothing if nothign changed
-}
dropFirst f xs = go xs Nil
  where go Nil _ = Nothing
        go (x:xs) out | f x = Just $ xs <> out
        go (x:xs) out = go xs (x:out)

{-
  Parse remaining elements, applying the given parser to each pattern.
-}
parseRemainder
  :: ∀ i e c s g u a
   . List u -- repeatables
  -> (u -> Parser e c s g (List i) (List a))
  -> Parser e c s g (List i) (List a)
parseRemainder repPats f = do
  go (choice $ f <$> repPats) Nil
  where go p xs = do
          vs <- p <|> pure Nil
          if null vs then pure xs else go p (xs <> vs)

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
