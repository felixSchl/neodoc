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
import Data.List.Partial as LU
import Data.List.Lazy as LL
import Data.Either
import Partial.Unsafe
import Control.Alt
import Control.Monad.Trans.Class (lift)
import Control.Monad.State as State
import Neodoc.Data.Indexed
import Neodoc.Data.Indexed as Indexed
import Neodoc.Parsing.Parser hiding (error)
import Neodoc.Parsing.Parser.Combinators
import Neodoc.Parsing.Parser.Combinators as Parser
import Neodoc.Parsing.Parser as Parser
import Neodoc.ArgParser.Evaluate (chooseBest, chooseBestTag)

data Pattern a = LeafPattern   IsOptional IsRepeatable IsFixed a
               | ChoicePattern IsOptional IsRepeatable IsFixed (List (List (Pattern a)))

setIsOptional :: ∀ a. Boolean -> Pattern a -> Pattern a
setIsOptional o (LeafPattern _ r f a) = LeafPattern o r f a
setIsOptional o (ChoicePattern _ r f a) = ChoicePattern o r f a

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

instance eqPattern :: (Generic a, Eq a) => Eq (Pattern a) where
  eq = gEq

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
type RepPats u = List (Pattern u)
type InputPats u = List (Pattern u)

-- depth-tracking error type
data Error e i u = Error (Either (PatternError i u) e)
                          Depth -- the depth of this error

data PatternError i u
  = GenericError String
  | UnexpectedInputError (NonEmpty List i)
  | MissingPatternError (Pattern u)
  | MissingElementError u

type IsFatal = Boolean
type HasTerminated = Boolean
type KeepPattern = Boolean

-- the function to yield a match
-- it is provided with the currently to-be-matched pattern item, the input
-- and a flag indicating whether omissions / substitutions are allowed to occur.
type Matcher u i a e = u -> List i -> AllowOmissions -> Match i e a

-- the internal type of a match, as used by the pattern parse algorithm.
-- it is essentially a reduction of `Match`.
type Match' a = Tuple a (Tuple HasTerminated KeepPattern)

-- the type of a single result, used by the pattern parse algorithm.
data Result a u = Result  (List a)       -- the output
                          (RepPats u)    -- resulting repeating patterns
                          HasMoved       -- indication if input was changed
                          Depth          -- relative depth
                          HasTerminated  -- forcefully terminate?
                          KeepPattern    -- keep the pattern, do not discard (useful for unknown patterns)

data Match i e a
  = NoMatch
  | Failed IsFatal e
  | Success KeepPattern (List i) a
  | Substituted (List i) a
  | Terminated a

instance functorMatch :: Functor (Match i e) where
  map f (Terminated a) = Terminated (f a)
  map f (Success k is a) = Success k is (f a)
  map f (Substituted is a) = Substituted is (f a)
  map _ NoMatch = NoMatch
  map _ (Failed f e) = Failed f e

instance altMatch :: Alt (Match i e) where
  alt (m1@(Failed true _)) _   = m1
  alt _ (m2@(Failed true _))   = m2
  alt (Failed _ _) m2          = m2
  alt NoMatch m2               = m2
  alt (m1@(Terminated _)) _    = m1
  alt (m1@(Success _ _ _)) _   = m1
  alt (m1@(Substituted _ _)) _ = m1

instance showResult :: (Generic u, Show a, Show u) => Show (Result a u) where
  show (Result a u m d t k) = "(Result " <> show a
                                  <> " " <> show u
                                  <> " " <> show m
                                  <> " " <> show d
                                  <> " " <> show t
                                  <> " " <> show k <> ")"

instance showError :: (Generic u, Show e, Show i, Show u) => Show (Error e i u) where
  show (Error x d) = "(Error " <> show x <> " " <> show d <> ")"

instance prettyError :: (Generic u, Pretty e, Pretty i, Pretty u) => Pretty (Error e i u) where
  pretty (Error x d) = pretty x <> " at " <> pretty d

instance showPatternError :: (Generic u, Show i, Show u) => Show (PatternError i u) where
  show (GenericError x) = "(GenericError " <> show x <> ")"
  show (UnexpectedInputError x) = "(UnexpectedInputError " <> show x <> ")"
  show (MissingPatternError x) = "(MissingPatternError " <> show x <> ")"
  show (MissingElementError x) = "(MissingElementError " <> show x <> ")"

instance prettyPatternError :: (Pretty i, Pretty u) => Pretty (PatternError i u) where
  pretty (GenericError s) = s
  pretty (UnexpectedInputError (s:|_)) = "Unexpected " <> pretty s
  pretty (MissingPatternError p) = "Missing " <> pretty p
  pretty (MissingElementError p) = "Missing " <> pretty p

setReps :: ∀ a u. RepPats u -> Result a u -> Result a u
setReps r (Result a _ c d t k) = Result a r c d t k

getValue :: ∀ a u. Result a u -> List a
getValue (Result a _ _ _ _ _) = a

getSuccessDepth :: ∀ a u. Result a u -> Int
getSuccessDepth (Result _ _ _ d _ _) = d

getErrorDepth :: ∀ e i u. Error e i u -> Int
getErrorDepth (Error _ d) = d

{-
  Apply the `Matcher` function to current input and update the parser state
  based on the results.
-}
match
  :: ∀ i e c s g u a
   . (Pretty u)
  => Matcher u i a e
  -> AllowOmissions
  -> Depth
  -> u
  -> Parser (Error e i u) c s g (List i) (Match' a)
match f allowOmissions depth p = Parser \a ->
  let result = f p (getI a) allowOmissions
   in case result of
        NoMatch ->
          let error = Error (Left $ GenericError "TODO") depth
           in Step false a $ Left $ ParseError false $ Right error
        Success k i v ->
          Step  true
                (Parser.setI i a)
                (Right $ v /\ false /\ k)
        Substituted i v ->
          Step  true
                (Parser.setI i a)
                (Right $ v /\ false /\ false)
        Terminated v ->
          Step  true
                (Parser.setI Nil a)
                (Right $ v /\ true /\ false)
        Failed isFatal e ->
          let error = Error (Right e) depth
           in Step false a $ Left $ ParseError isFatal $ Right error

{-
  Lower a parser from the depth tracking error type `Error e` to it's contained
  error type e.
-}
lowerError
  :: ∀ e c s g i a u
   . (PatternError i u -> e)
  -> Parser (Error e i u) c s g (List i) a
  -> Parser e c s g (List i) a
lowerError fE p = Parser \a ->
  let step = unParser p a
   in case step of
        Step c i (Left (ParseError fatal (Right (Error (Left pE) _)))) ->
          Step c i (Left (ParseError fatal (Right (fE pE))))
        Step c i (Left (ParseError fatal (Right (Error (Right e) _)))) ->
          Step c i (Left (ParseError fatal (Right e)))
        Step c i (Left (ParseError fatal (Left e))) ->
          Step c i (Left (ParseError fatal (Left e)))
        Step c i (Right r) -> Step c i (Right r)

errorAt :: ∀ e i u. Int -> PatternError i u -> Error e i u
errorAt d e = Error (Left e) d

errorAt' :: ∀ e i u. Int -> e -> Error e i u
errorAt' d e = Error (Right e) d

{-
  Parse the given patterns using the custom matcher function.
-}
parse
  :: ∀ i e c s g u a
   . (Eq u, Generic u, Show a, Show i, Show u, Show e, Pretty e, Pretty i, Pretty u, Pretty a)
  => Matcher u i a e
  -> (PatternError i u -> e)
  -> InputPats u
  -> Parser e c s g (List i) (List a)
parse f fE pats = lowerError fE $ fst <$> parse' f pats

{-
  Parse the given patterns using the custom matcher function and expect the EOF
  after the parse.
  TODO: this should probably be removed
-}
parseToEnd
  :: ∀ i e c s g u a
   . (Eq u, Generic u, Show a, Show i, Show u, Show e, Pretty e, Pretty i, Pretty u, Pretty a)
  => Matcher u i a e
  -> (PatternError i u -> e)
  -> InputPats u
  -> Parser e c s g (List i) (List a)
parseToEnd f fE pats = lowerError fE $ fst <$> parseToEnd' f pats

{-
  Parse the given tagged patterns using the custom matcher function.
-}
parseBestTag
  :: ∀ i e c s g u a tag
   . (Eq u, Generic u, Show a, Show i, Show u, Show e, Show tag, Pretty e, Pretty i, Pretty u, Pretty a)
  => Matcher u i a e
  -> (PatternError i u -> e)
  -> List (Tuple tag (List (Pattern u))) -- tagged input patterns
  -> Parser e c s g (List i) (Tuple tag (List a))
parseBestTag f fE taggedPats = lowerError fE do
  rmap fst <$> do
    chooseBestTag
      getErrorDepth
      snd
      (rmap (Parser.try <<< parse' f) <$> taggedPats)

{-
  Parse the given tagged patterns using the custom matcher function and expect
  the EOF after the parse.
  TODO: this should probably be removed
-}
parseBestTagToEnd
  :: ∀ i e c s g u a tag
   . (Eq u, Generic u, Show a, Show i, Show u, Show e, Show tag, Pretty e, Pretty i, Pretty u, Pretty a)
  => Matcher u i a e
  -> (PatternError i u -> e)
  -> List (Tuple tag (List (Pattern u))) -- tagged input patterns
  -> Parser e c s g (List i) (Tuple tag (List a))
parseBestTagToEnd f fE taggedPats = lowerError fE do
  rmap fst <$> do
    chooseBestTag
      getErrorDepth
      snd
      (rmap (Parser.try <<< parseToEnd' f) <$> taggedPats)

{-
  XXX explain me
-}
parseToEnd'
  :: ∀ i e c s g u a
   . (Eq u, Generic u, Show a, Show i, Show u, Show e, Pretty e, Pretty i, Pretty u, Pretty a)
  => Matcher u i a e
  -> List (Pattern u) -- input patterns
  -> Parser (Error e i u) c s g (List i) (Tuple (List a) Depth)
parseToEnd' f pats = do
  vs /\ depth <- parse' f pats
  is <- getInput
  case is of
    Nil  -> Parser.return $ vs /\ depth
    i:is -> Parser.fail' $ errorAt depth $ UnexpectedInputError (i :| is)

{-
  XXX explain me
-}
parse'
  :: ∀ i e c s g u a
   . (Eq u, Generic u, Show a, Show i, Show u, Show e, Pretty e, Pretty i, Pretty u, Pretty a)
  => Matcher u i a e
  -> List (Pattern u) -- input patterns
  -> Parser (Error e i u) c s g (List i) (Tuple (List a) Depth)
parse' f pats = do
  Result vs _ _ depth _ _ <- parsePatterns' 0 (match f) true Nil pats
  Parser.return $ vs /\ depth

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
  -> Parser (Error e i u) c s g (List i) a
expected depth pat = Parser.fail' $ errorAt depth (MissingPatternError pat)

{-
  XXX explain me
-}
requireMovement p = do
  Result a b hasMoved d hasTerminated keepPat <- p
  if hasMoved || hasTerminated
     then Parser.return $ Result a b true d hasTerminated keepPat
     else Parser.fail "no movement ..."

{-
  XXX explain me
-}
parsePatterns'
  :: ∀ i e c s g u a
   . (Eq u, Generic u, Show a, Show i, Show u, Show e, Pretty e, Pretty i, Pretty u, Pretty a)
  => Int
  -> (AllowOmissions
      -> Depth
      -> u
      -> Parser (Error e i u) c s g (List i) (Match' a))
  -> AllowOmissions   -- allow omissions?
  -> List (Pattern u) -- repeatables
  -> List (Pattern u) -- input patterns
  -> Parser (Error e i u) c s g (List i) (Result a u)
parsePatterns' l f allowOmit repPats pats =
  let xs = indexed pats
      f' allowOmit' depth reps pat = do
        Result vs reps' hasMoved depth hasTerminated keepPat <- case pat of
          LeafPattern _ r _ x -> do
            v /\ hasTerminated /\ keepPat <- f allowOmit' depth x
            pure $ Result (singleton v) reps true (depth + 1) hasTerminated keepPat
          ChoicePattern _ _ _ xs ->
            let resetReps = id -- setReps reps
              in resetReps <$> do
                  chooseBest
                    getErrorDepth
                    getSuccessDepth
                    (parsePatterns' (l + 1) f allowOmit' reps <$> xs)
        let reps'' = if isRepeatable pat then nub (pat : reps') else reps'
        pure $ Result vs reps'' hasMoved depth hasTerminated keepPat
   in do
        -- 1. parse the pattern wholly
        Result vs rep hasMoved depth hasTerminated keepPat <- go do
          Args12 f' xs xs Nil false false false false repPats Nil 0 Nothing

        -- 2. consume any trailing arguments using the repeating patterns we
        --    learned about so far.
        --    note: parsing the remainder may have consumed input, since we
        --          never substitute values during rep parsing.
        --    note: we ignore any newly learned reps at this point (TODO: is
        --          this correct?)
        vs' <- if hasTerminated || keepPat
                  then pure Nil
                  else parseRemainder rep ((getValue <$> _) <<< f' true depth rep)

        Parser.return $ Result  (vs <> vs')
                                rep
                                -- XXX: is `not (null vs')` fair here? we might
                                --      be retrieving values via fallbacks.
                                -- TODO: `match` should indicate if moved!
                                (hasMoved || (not $ null vs'))
                                (depth + length vs') -- XXX: same here (^)
                                hasTerminated
                                keepPat

  where
  go
    :: Args12
        _                     -- the parser function on elements of `u`
        _                     -- the original input for given iteration
        _                     -- the patterns to match
        _                     -- the carry
        AllowOmissions        -- allow omissions?
        AllowRepetitions      -- allow repetitions?
        HasMoved              -- have we consumed any input this far?
        IsLocked              -- are we locked from processing any more fixed?
        (List (Pattern u))    -- repeatable patterns found so far
        (List a)              -- output values
        Int                   -- the depth of this parse
        (Maybe (Error e i u)) -- the deepest error met
    ->  Parser (Error e i u) c s g (List i) (Result a u)

  -- Success!
  go (Args12 _ orig Nil Nil _ _ hasMoved _ reps out depth _) = do
    getInput >>= \i-> traceShowA $ indent l <> (pretty $
                                "success"
                             /\ ("input=" <> pretty i)
                             /\ ("reps=" <> pretty reps)
                             /\ ("out=" <> pretty out)
                             /\ ("orig=" <> pretty orig)
                             )

    Parser.return $ Result out reps hasMoved depth false false

  -- Step: ignore fixed patterns when locked
  go (Args12 f' orig (x@(Indexed ix pat):xs) carry allowOmissions allowReps hasMoved true reps out depth mE) | isFixed pat = do
    getInput >>= \i-> traceShowA $ indent l <> (pretty $
                                "ignore (locked)"
                             /\ ("pat=" <> pretty pat)
                             /\ ("input=" <> pretty i)
                             /\ ("out=" <> pretty out)
                             /\ ("carry=" <> pretty carry)
                             /\ ("allowOmissions=" <> pretty allowOmissions)
                             /\ ("allowReps=" <> pretty allowReps)
                             )

    go (Args12 f' orig xs (snoc carry x) allowOmissions allowReps hasMoved true reps out depth mE)

  -- Failure: try again, this time allow omissions (keep the accumulator)
  go (Args12 f' orig Nil (carry@(_:_)) false true _ _ reps out depth mE) | allowOmit = do
    getInput >>= \i-> traceShowA $ indent l <> (pretty $
                                "failure (next: retry w/ omissions)"
                            /\ ("input=" <> pretty i)
                            /\ ("out=" <> pretty out)
                            /\ ("carry=" <> pretty carry)
                            )

    go (Args12 f' orig orig Nil true true false false reps out depth mE)

  -- Failure: try again, this time allow repetitions (keep the accumulator)
  go (Args12 f' orig Nil (carry@(_:_)) allowOmissions false _ isLocked reps out depth mE) = do
    getInput >>= \i-> traceShowA $ indent l <> (pretty $
                                "failure (next: retry w/ reps)"
                            /\ ("input=" <> pretty i)
                            /\ ("out=" <> pretty out)
                            /\ ("carry=" <> pretty carry)
                            )

    go (Args12 f' orig Nil carry allowOmissions true false false reps out depth mE)

  -- Failure: ...
  go (Args12 f' orig Nil (carry@(((Indexed _ pat):_))) false true _ _ _ out depth mE) = do
    getInput >>= \i-> traceShowA $ indent l <> (pretty $
                                "failure"
                            /\ ("input=" <> pretty i)
                            /\ ("out=" <> pretty out)
                            /\ ("carry=" <> pretty carry)
                            )

    let throwExistingError = case mE of
          Just (e@(Error _ d)) | d > depth -> Parser.fail' e
          _ ->
            let sortedCarry = sortBy (compare `on` getIndex) carry
                pat' = getIndexedElem $ unsafePartial $ LU.head sortedCarry
             in expected depth pat'
    throwExistingError

  -- Failure: now try to omit one element and reset
  go (Args12 f' _ Nil (carry@(((Indexed _ pat):_))) true true _ _ reps out depth mE) = do
    getInput >>= \i-> traceShowA $ indent l <> (pretty $
                                "omit"
                            /\ ("input=" <> pretty i)
                            /\ ("out=" <> pretty out)
                            /\ ("carry=" <> pretty carry)
                            )

    -- at this point we rotated the entire input and tried to consume via
    -- repetitions, but w/o any luck. it's time for drastic measures by starting
    -- to remove optional elements, ony be one.

    -- TODO: this might need more logic as to which element we throw out first
    --       the current approach is simply removes from left to right.
    let throwExistingError = case mE of
          Just (e@(Error _ d)) | d > depth -> Parser.fail' e
          _ ->
            let sortedCarry = sortBy (compare `on` getIndex) carry
                pat' = getIndexedElem $ unsafePartial $ LU.head sortedCarry
             in expected depth pat'
    if allowOmit
       then
          let sortedCarry = sortBy (compare `on` getIndex) carry
           in case dropFirst (isOptional <<< getIndexedElem) sortedCarry of
            Just carry' -> (
              go $ Args12 f' carry' carry' Nil false false false false reps out depth mE
              ) <|> throwExistingError -- XXX: is this right, or `catch` here?
            Nothing -> throwExistingError
       else throwExistingError

  -- Step: process next element
  go (Args12 f' orig (x@(Indexed ix pat):xs) carry allowOmissions allowReps hasMoved isLocked reps out depth mE) = do
    getInput >>= \i-> traceShowA $ indent l <> (pretty $
                                "step"
                              /\ ("input=" <> pretty i)
                              /\ ("allowOmissions=" <> show allowOmissions)
                              /\ ("allowReps=" <> show allowReps)
                              /\ ("reps=" <> pretty reps)
                              /\ ("isLocked=" <> pretty isLocked)
                              /\ ("pat=" <> pretty pat)
                              /\ ("carry=" <> pretty carry)
                              /\ ("orig=" <> pretty orig)
                              /\ ("hasMoved=" <> pretty hasMoved)
                              /\ ("out=" <> pretty out)
                              )

    -- 1. try parsing the pattern
    -- we try parsing this pattern under varying circumstances but only capture
    -- new errors for non-ommissable parses.
    -- when allowing omissions, ensure not to allow omissing non-fixed patterns
    -- before having culled fixed ones.
    eR <- (if allowOmissions && (not isLocked || isFixed pat)
            then
              let p = Right <$> do
                        Parser.try do
                          f' true depth reps pat
               in p -- XXX: should we `catch` here?
            else
              let p = Right <$> do
                        Parser.try do
                          requireMovement do
                            f' false depth reps pat
               in p `catch` \_ e ->
                    let e' = Parser.mapError (\(Error e d) -> Error e (d + depth)) e
                      in pure $ Left $ Just e'
          ) <|> (pure $ Left Nothing)

    case eR of
      -- if we have a result, and the result consumed input, we're looking
      -- good. we reset the input pattern bar the matched element, clear
      -- the carry and add this pattern to the list of repeated patterns,
      -- (if this pattern allows for repetition). We also reset `allowReps` to
      -- to treat the rest of the pattern as fresh input.
      Right (Result result reps' hasMoved depth' hasTerminated keepPat) -> do

        -- before attempting to parse the next element, we try to eagerly parse
        -- adjacent matches of the same pattern.
        -- note: we do not check `allowReps` because we want adjacent
        --       repetitions to be done eagerly.
        vs' <- if keepPat || hasTerminated || (not $ isRepeatable pat)
                then pure Nil
                else parseRemainder (singleton pat) do
                      (getValue <$> _) <<< f' true depth reps

        let orig' = sortBy (compare `on` getIndex) do
                      if keepPat
                          then orig
                          else filter (not <<< (_ == ix) <<< getIndex) orig
            depth'' = depth + depth' + length vs'
            out' = out <> result <> vs'

        if hasTerminated
          then Parser.return $ Result out' reps' hasMoved depth'' true false
          else go (Args12 f' orig' orig' Nil false false true ((not $ isFixed pat) && isLocked) reps' out' depth'' mE)

      -- 2. if we did not manage to make a match, we will try to make a match
      --    using all previously matched, repeatable patterns, provided we are
      --    allowed to do so (`allowReps`).
      Left mE' -> do
        mR' <- if allowReps && (not $ null reps)
                  then (Just <$> choice (Parser.try <<< requireMovement
                                                    <<< f' false depth Nil <$> reps))
                          <|> (pure Nothing)
                  else pure Nothing
        case mR' of
          -- if we have a result, and the result consumed input, we're looking
          -- ok. we reset the input pattern completely, clear the carry and
          -- re-iterate using the new input. Again, we reset `allowReps` to
          -- `false` in order to treat the resulting input as a fresh pattern.
          Just (Result result' _ _ depth' hasTerminated _) ->
            let orig' = sortBy (compare `on` getIndex) orig
                depth'' = depth + depth'
                out' = out <> result'
             in if hasTerminated
                   then pure $ Result out' reps true depth'' true false
                   else go (Args12 f' orig' orig' Nil false false true false reps out' depth'' mE)

          -- 3. if, at this point, we still did not manage to make a match, we
          --    rotate this pattern into the carry and give the next pattern a
          --    chance.
          _ -> do
            -- check to see if the new error occurred at a greater depth than
            -- the currently tracked error.
            let mE'' = case mE /\ mE' of
                  Nothing /\ Just (ParseError _ (Right e)) -> Just e
                  Just (Error _ d) /\ Just (ParseError _ (Right (e'@(Error _ d'))))
                    | d' > d -> Just e'
                  _ /\ _ -> mE

            go (Args12 f' orig xs (x:carry) allowOmissions allowReps hasMoved (isLocked || isFixed pat) reps out depth mE'')

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
  :: ∀ i e c s g u a. (Show a, Show i) =>
    List u -- repeatables
  -> (u -> Parser e c s g (List i) (List a))
  -> Parser e c s g (List i) (List a)
parseRemainder Nil _ = pure Nil
parseRemainder repPats f =  go (choice $ f <$> repPats) Nil
  where go p xs = do
          i <- Parser.getInput
          case i of
            Nil -> pure xs
            _   -> do
              vs <- (Parser.try p) <|> pure Nil
              traceShowA vs
              if null vs then pure xs else go p (xs <> vs)
