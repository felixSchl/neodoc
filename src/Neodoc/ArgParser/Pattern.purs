module Neodoc.ArgParser.Pattern where

import Prelude
import Debug.Trace
import Data.Function (on)
import Data.Tuple
import Data.Tuple.Nested
import Data.Foldable (for_)
import Data.NonEmpty ((:|), NonEmpty)
import Data.Optimize.Uncurried
import Data.List hiding (many)
import Data.Bifunctor (rmap, lmap)
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
import Control.Monad.Trans.Class (lift)
import Control.Monad.State as State
import Neodoc.Data.Indexed
import Neodoc.Parsing.Parser hiding (error)
import Neodoc.Parsing.Parser.Combinators
import Neodoc.Parsing.Parser.Combinators as Parser
import Neodoc.Parsing.Parser as Parser
import Neodoc.ArgParser.Evaluate (chooseBest)

data Pattern a = LeafPattern   IsOptional IsRepeatable IsFixed a
               | ChoicePattern IsOptional IsRepeatable IsFixed (List (List (Pattern a)))

type IsOptional = Boolean
type IsRepeatable = Boolean
type IsFixed = Boolean
type IsLocked = Boolean
type HasMoved = Boolean
type AllowRepetitions = Boolean
type AllowOmissions = Boolean

derive instance genericPattern :: (Generic a) => Generic (Pattern a)
instance showPattern :: (Generic a, Show a) => Show (Pattern a) where
  show = gShow

instance functorPattern :: Functor Pattern where
  map f (ChoicePattern o r fix xs) = ChoicePattern o r fix $ ((f <$> _) <$> _) <$> xs
  map f (LeafPattern o r fix x) = LeafPattern o r fix (f x)

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

type Depth = Int
type Reps u = List (Pattern u)
data Error e = Error (Either String e) Depth
data Result a u = Result (List a) (Reps u) HasMoved Depth

instance showError :: (Show e) => Show (Error e) where
  show (Error x d) = "(Error " <> show x <> " " <> show d <> ")"

type IsFatal = Boolean
type PatternMatch i e a = Either (Tuple IsFatal e) (Tuple a (List i))
type Matcher u i a e = u -> List i -> AllowOmissions -> PatternMatch i e a

setReps :: ∀ a u. Reps u -> Result a u -> Result a u
setReps r (Result a _ c d) = Result a r c d

getValue :: ∀ a u. Result a u -> List a
getValue (Result a _ _ _) = a

getSuccessDepth :: ∀ a u. Result a u -> Int
getSuccessDepth (Result _ _ _ d) = d

getErrorDepth :: ∀ e. Error e -> Int
getErrorDepth (Error _ d) = d

{-
  XXX explain me
-}
match
  :: ∀ i e c s g u a
   . (Pretty u)
  => Matcher u i a e
  -> AllowOmissions
  -> Depth
  -> u
  -> Parser (Error e) c s g (List i) a
match f allowOmissions depth p = Parser \a ->
  let result = f p (getI a) allowOmissions
   in case result of
        Right (r /\ i) -> Step true (Parser.setI i a) (Right r)
        Left (isFatal /\ e) ->
          let error = Error (Right e) depth
           in Step false a $ Left $ ParseError isFatal (Right error)

{-
  Lower a parser from the depth tracking error type `Error e` to it's contained
  error type e.
-}
lowerError
  :: ∀ e c s g i a
   . Parser (Error e) c s g i a
  -> Parser e c s g i a
lowerError p = Parser \a ->
  let step = unParser p a
   in case step of
        Step c i (Left (ParseError fatal (Right (Error (Left s) _)))) ->
          Step c i (Left (ParseError fatal (Left s)))
        Step c i (Left (ParseError fatal (Right (Error (Right e) _)))) ->
          Step c i (Left (ParseError fatal (Right e)))
        Step c i (Left (ParseError fatal (Left e))) ->
          Step c i (Left (ParseError fatal (Left e)))
        Step c i (Right r) -> Step c i (Right r)

errorAt :: ∀ e. Int -> String -> Error e
errorAt d s = Error (Left s) d

errorAt' :: ∀ e. Int -> e -> Error e
errorAt' d e = Error (Right e) d

{-
  XXX explain me
-}
parse
  :: ∀ i e c s g u a
   . (Pretty i, Pretty u, Pretty a, Show e)
  => Matcher u i a e
  -> List (Pattern u) -- input patterns
  -> Parser e c s g (List i) (List a)
parse f pats = do
  vs <- getValue <$> do
          lowerError do
            parsePatterns 0 (match f) true Nil pats
  is <- getInput
  case is of
    Nil   -> Parser.return vs
    i : _ -> Parser.fail $ "Unexpected " <> pretty i

indent :: Int -> String
indent l = String.fromCharArray $ LL.toUnfoldable $ LL.take (l * 4) $ LL.repeat ' '

{-
  Fail with an "expected" message ...
-}
expected
  :: ∀ e c s g i a u
   . (Pretty u)
  => Int
  -> Pattern u
  -> Parser (Error e) c s g i a
expected depth pat = Parser.fail' $ errorAt depth ("Expected " <> pretty pat)

{-
  XXX explain me
-}
requireMovement p = do
  Result a b hasMoved d <- p
  if not hasMoved
     then Parser.fail "no movement ..."
     else Parser.return $ Result a b true d

{-
  XXX explain me
-}
parsePatterns
  :: ∀ i e c s g u a
   . (Pretty i, Pretty u, Pretty a, Show e)
  => Int
  -> (AllowOmissions -> Depth -> u -> Parser (Error e) c s g (List i) a)
  -> AllowOmissions   -- allow omissions?
  -> List (Pattern u) -- repeatables
  -> List (Pattern u) -- input patterns
  -> Parser (Error e) c s g (List i) (Result a u)
parsePatterns l f allowOmit repPats pats =
  let xs = indexed pats
      f' allowOmit' depth reps pat = do
        Result vs reps' hasMoved depth <- case pat of
          LeafPattern o r _ x -> do
            vs <- singleton <$> f (o && allowOmit') depth x
            pure $ Result vs reps true depth
          ChoicePattern o _ _ xs ->
            let resetReps = setReps reps
              in resetReps <$> do
                  chooseBest
                    getErrorDepth
                    getSuccessDepth
                    (parsePatterns (l + 1) f (o && allowOmit') Nil <$> xs)
        let reps'' = if isRepeatable pat then pat : reps' else reps'

        pure $ Result vs reps'' hasMoved depth
   in do
        -- 1. parse the pattern wholly
        Result vs rep hasMoved depth <- go do
          Args12 f' xs xs Nil false false false false repPats Nil 0 Nothing

        -- 2. consume any trailing arguments using the repeating patterns we
        --    learned about so far.
        --    note: parsing the remainder may have consumed input, since we
        --          never substitute values during rep parsing.
        --    note: we ignore any newly learned reps at this point (TODO: is
        --          this correct?)
        vs' <- parseRemainder rep ((getValue <$> _) <<< f' false 0 rep)

        Parser.return $ Result  (vs <> vs')
                                rep
                                -- XXX: is `not (null vs')` fair here? we might
                                --      be retrieving values via fallbacks.
                                -- TODO: `match` should indicate if moved!
                                (hasMoved || (not $ null vs'))
                                (depth + length vs') -- XXX: same here (^)

  where
  go
    :: Args12
        _                  -- the parser function on elements of `u`
        _                  -- the original input for given iteration
        _                  -- the patterns to match
        _                  -- the carry
        AllowOmissions     -- allow omissions?
        AllowRepetitions   -- allow repetitions?
        HasMoved           -- have we consumed any input this far?
        IsLocked           -- are we locked from processing any more fixed?
        (List (Pattern u)) -- repeatable patterns found so far
        (List a)           -- output values
        Int                -- the depth of this parse
        (Maybe (Error e))  -- the deepest error met
    ->  Parser (Error e) c s g (List i) (Result a u)
  -- Success!
  go (Args12 _ _ Nil Nil _ _ hasMoved _ reps out depth _) = do
    Parser.return $ Result out reps hasMoved depth

  -- Failure: try again, this time allow omissions (keep the accumulator)
  go (Args12 f' orig Nil (_:_) false true _ _ reps out depth mE) | allowOmit = do
    go (Args12 f' orig orig Nil true true false false reps out depth mE)

  -- Failure: try again, this time allow repetitions (keep the accumulator)
  go (Args12 f' orig Nil (_:_) allowOmissions false _ _ reps out depth mE) = do
    go (Args12 f' orig orig Nil allowOmissions true false false reps out depth mE)

  -- Failure: ...
  go (Args12 f' _ Nil (carry@(((Indexed _ pat):_))) false true _ _ _ _ depth mE) = do
    case mE of
      Just e -> Parser.fail' e
      _      -> expected depth pat

  go (Args12 f' _ Nil (carry@(((Indexed _ pat):_))) true true _ _ reps out depth mE) = do

    -- at this point we rotated the entire input and tried to consume via
    -- repetitions, but w/o any luck. it's time for drastic measures by starting
    -- to remove optional elements, ony be one.

    -- TODO: this might need more logic as to which element we throw out first
    --       the current approach is simply removes from left to right.
    let throwExistingError = case mE of
          Just e -> Parser.fail' e
          _      -> expected depth pat
    if allowOmit
       then
          let sortedCarry = sortBy (compare `on` getIndex) carry
           in case dropFirst (isOptional <<< getIndexedElem) sortedCarry of
            Just carry' -> (
              go $ Args12 f' carry' carry' Nil false false false false reps out depth mE
              ) <|> throwExistingError
            Nothing -> throwExistingError
       else throwExistingError

  -- Step: ignore fixed patterns when locked
  go (Args12 f' orig (x@(Indexed ix pat):xs) carry allowOmissions allowReps hasMoved true reps out depth mE) | isFixed pat = do
    go (Args12 f' orig xs (snoc carry x) allowOmissions allowReps hasMoved true reps out depth mE)

  -- Step: process next element
  go (Args12 f' orig (x@(Indexed ix pat):xs) carry allowOmissions allowReps hasMoved isLocked reps out depth mE) = do

    -- 1. try parsing the pattern
    -- we try parsing this pattern under varying circumstances but only capture
    -- new errors for non-ommissable parses.
    eR <- (if allowOmissions
            then Right <$> do
                  Parser.try do
                    f' true depth reps pat
            else
              let p = Right <$> do
                        Parser.try do
                          requireMovement do
                            f' false depth reps pat
               in p `catch` \_ e -> pure $ Left $ Just e
          ) <|> (pure $ Left Nothing)

    case eR of
      -- if we have a result, and the result consumed input, we're looking
      -- good. we reset the input pattern bar the matched element, clear
      -- the carry and add this pattern to the list of repeated patterns,
      -- (if this pattern allows for repetition). We also reset `allowReps` to
      -- to treat the rest of the pattern as fresh input.
      Right (Result result reps' hasMoved depth') -> do
        -- before attempting to parse the next element, we try to eagerly parse
        -- adjacent matches of the same pattern.
        -- note: we do not check `allowReps` because we want adjacent
        --       repetitions to be done eagerly.
        vs' <- if isRepeatable pat
                then parseRemainder (singleton pat) do
                      (getValue <$> _) <<< f' false depth reps
                else pure Nil

        let orig' = sortBy (compare `on` getIndex) do
                      filter (not <<< (_ == ix) <<< getIndex) orig
            depth'' = depth + depth' + length vs'

        go (Args12 f' orig' orig' Nil false false true isLocked reps' (out <> result <> vs') depth'' mE)

      -- 2. if we did not manage to make a match, we will try to make a match
      --    using all previously matched, repeatable patterns, provided we are
      --    allowed to do so (`allowReps`).
      Left mE' -> do
        mR' <- if allowReps
                  then (Just <$> choice (Parser.try <<< requireMovement
                                                    <<< f' false depth Nil <$> reps))
                          <|> (pure Nothing)
                  else pure Nothing
        case mR' of
          -- if we have a result, and the result consumed input, we're looking
          -- ok. we reset the input pattern completely, clear the carry and
          -- re-iterate using the new input. Again, we reset `allowReps` to
          -- `false` in order to treat the resulting input as a fresh pattern.
          Just (Result result' _ _ depth') ->
            let orig' = sortBy (compare `on` getIndex) do
                          filter (not <<< (_ == ix) <<< getIndex) orig
                depth'' = depth + depth'
             in go (Args12 f' orig' orig' Nil false false true isLocked reps (out <> result') depth'' mE)

          -- 3. if, at this point, we still did not manage to make a match, we
          --    rotate this pattern into the carry and give the next pattern a
          --    chance.
          _ -> do
            -- check to see if the new error occurred at a greater depth than
            -- the currently tracked error.
            let mE'' = case mE /\ mE' of
                  Nothing /\ Just (ParseError _ (Right e)) -> Just e
                  Just (Error _ d) /\ Just (ParseError _ (Right (Error e' d')))
                    | (d' + depth) >= d
                    -> Just (Error e' (d' + depth))
                  _ /\ _ -> mE

            go (Args12 f' orig xs (x:carry) allowOmissions allowReps hasMoved (isFixed pat) reps out depth mE'')

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
          vs <- (Parser.try p) <|> pure Nil
          if null vs then pure xs else go p (xs <> vs)