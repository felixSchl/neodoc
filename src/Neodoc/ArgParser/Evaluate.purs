module Neodoc.ArgParser.Evaluate where

import Prelude
import Debug.Trace
import Debug.Profile
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Data.Optimize.Uncurried
import Data.Pretty
import Data.Newtype (unwrap)
import Data.List (
  List(..), some, singleton, filter, fromFoldable, last, groupBy, sortBy, (:)
, null, length, reverse)
import Data.Function (on)
import Data.Either (Either(..), fromRight)
import Data.NonEmpty (NonEmpty(..), (:|))
import Data.Maybe (Maybe(..))
import Data.Traversable (for)
import Data.Foldable (class Foldable, maximumBy)
import Data.Optimize.Uncurried
import Control.Alt ((<|>))
import Control.Lazy (defer)
import Control.MonadPlus.Partial (mrights, mlefts, mpartition)
import Partial.Unsafe

import Neodoc.Value (Value(..))
import Neodoc.Spec (Spec(..))
import Neodoc.Env
import Neodoc.Data.Layout
import Neodoc.Data.SolvedLayout
import Neodoc.Data.SolvedLayout as Solved
import Neodoc.Data.Chunk
import Neodoc.Parsing.Parser
import Neodoc.Parsing.Parser as Parser
import Neodoc.ArgParser.Type
import Neodoc.ArgParser.Options
import Neodoc.ArgParser.Token
import Neodoc.ArgParser.Argument
import Neodoc.ArgParser.Lexer as L
import Neodoc.ArgParser.Profile

type ChunkedLayout a = Layout (Chunk a)

data ParserCont c s g i = ParserCont c s g i

data Evaluation c s g i e a
  = ErrorEvaluation   (ParserCont c s g i) (ParseError e)
  | SuccessEvaluation (ParserCont c s g i) a

instance showParserCont :: (Show c, Show s, Show g, Show i) => Show (ParserCont c s g i) where
  show (ParserCont c s g i) = "ParserCont " <> show c <> " " <> show s <> " " <> show g <> " " <> show i

instance showEvaluation :: (Show c, Show s, Show g, Show i, Show e, Show a) => Show (Evaluation c s g i e a) where
  show (ErrorEvaluation   c e) = "ErrorEvaluation " <> show c <> " " <> show e
  show (SuccessEvaluation c a) = "SuccessEvaluation " <> show c <> " " <> show a

isError :: ∀ c s g i e a. Evaluation c s g i e a -> Boolean
isError (ErrorEvaluation _ _) = true
isError _ = false

isSuccess :: ∀ c s g i e a. Evaluation c s g i e a -> Boolean
isSuccess (SuccessEvaluation _ _) = true
isSuccess _ = false

getCont :: ∀ e c s g i a. Evaluation c s g i e a -> ParserCont c s g i
getCont (ErrorEvaluation c _) = c
getCont (SuccessEvaluation c _) = c

getC :: ∀ c s g i. ParserCont c s g i -> c
getC (ParserCont c _ _ _) = c

getS :: ∀ c s g i. ParserCont c s g i -> s
getS (ParserCont _ s _ _) = s

getG :: ∀ c s g i. ParserCont c s g i -> g
getG (ParserCont _ _ g _) = g

snapshot :: ∀ e c s g i. Parser e c s g i (ParserCont c s g i)
snapshot = Parser \(a@(ParseArgs c s g i)) -> do
            Step false a (Right $ ParserCont c s g i)

-- Evaluate multiple parsers, producing a new parser that chooses the best
-- succeeding match or fails otherwise. If any of the parsers yields a fatal
-- error, it is propagated immediately.
evalParsers
  :: ∀ b r a
   . (Ord b)
  => Args2 (a -> b) (List (ArgParser r a))
  -> ArgParser r a
evalParsers (Args2 f ps) = evalParsers' f
                                        (_.depth <<< getS)
                                        (_.deepestError <<< getG)
                                        setErrorAtDepth
                                        ps

evalParsers'
  :: ∀ b e c s g i a
   . (Ord b)
  => (a -> b)
  -> (ParserCont c s g (List i) -> Int {- query depth -})
  -> (ParserCont c s g (List i) -> Maybe (Tuple Int e) {- query deepest errors -})
  -> (Int -> e -> Parser e c s g _ Unit)
  -> List (Parser e c s g (List i) a)
  -> Parser e c s g (List i) a
evalParsers' p getDepth getDeepestError setDeepestErrors parsers = do
  a@(ParseArgs c s g i) <- getParseState

  -- Run all parsers and collect their results for further evaluation
  let collected = parsers <#> \parser ->
        runParser' $ Args5 c s g i $ Parser \a' ->
          case unParser parser a' of
            Step b' a''@(ParseArgs c' s' g' i') result ->
              let cont = ParserCont c' s' g' i'
               in Step b' a'' case result of
                  Left  (err@(ParseError true _)) -> Left err
                  Left  err -> Right $ ErrorEvaluation   cont err
                  Right val -> Right $ SuccessEvaluation cont val

  -- Now, check to see if we have any fatal errors, winners or soft errors.
  -- We know we must have either, but this is not encoded at the type-level,
  -- we have to fail with an internal error message in the impossible case this
  -- is not true.
  case mlefts collected of
    error:_ -> Parser \a -> Step false a (Left error)
    _       -> pure unit

  let
    results = mrights collected
    successes = reverse $ filter isSuccess results
    eqByDepth = eq `on` (getCont >>> getDepth)
    cmpByDepth = compare `on` (getCont >>> getDepth)
    errors = filter isError results
    deepestErrors = last $ groupBy eqByDepth $ sortBy cmpByDepth errors
    bestSuccess = do
      deepest <- last $ groupBy eqByDepth $ sortBy cmpByDepth successes
      unsafePartial $ flip maximumBy deepest $ compare `on` case _ of
        SuccessEvaluation _ v -> p v

  -- Ok, now that we have a potentially "best error" and a potentially "best
  -- match", take a pick.
  case bestSuccess of
    Just (SuccessEvaluation (ParserCont c' s' g' i') val) -> do
      applyResults results $ Parser \_ -> do
        Step true (ParseArgs c' s' g' i') (Right val)
    _ -> case deepestErrors of
          -- TODO: FIX UP
          -- Just errors -> case unwrap errors of
          --   (ErrorEvaluation (ParserCont c' s' g' i') e) :| es | null es || not (null i) -> do
          --     applyResults results $ Parser \_ ->
          --       Step false (ParseArgs c' s' g' i') (Right unit)
          --
          --     depth        <- getDepth        <$> snapshot
          --     deepestError <- getDeepestError <$> snapshot
          --
          --     case deepestError of
          --       Just (d /\ e) | d > depth -> Parser.fail' e
          --       _                         -> Parser.throw e
            -- _ -> Parser.fail "" -- XXX: explain this
          _ -> Parser.fail "The impossible happened. Failure without error"

  where
  applyResults results p = do
    out <- p
    -- transport adjacent error messages up, even on success. should we fail
    -- at a later stage, we have access to this valuable information and
    -- can present the best error message possible.
    unsafePartial $ for results \result ->
      case getDeepestError $ getCont result of
        -- TODO: FIX UP:
        -- Just (d /\ e) -> setErrorAtDepth d e
        _             -> pure unit
    pure out

{-
  Yield a successful continuation or fail.
  The parse does not influence the outer parser state with
  the exception of "global state".

  The reason for modifying the global state in face of an
  error is that we can propagate the "deepestError" value.
 -}
fork
  :: ∀ e c s g i a
   . Parser e c s g i a
  -> Parser e c s g i (Tuple (ParserCont c s g i) a)
fork parser = do
  c <- getConfig
  s <- getState
  g <- getGlobalState
  i <- getInput
  let r = runParser' $ Args5 c s g i $ Parser \a ->
        case unParser parser a of
          Step b' a' r' ->
          --  TODO: use tuple in `ParserCont`
            let cont = ParserCont (Parser.getC a')
                                  (Parser.getS a')
                                  (Parser.getG a')
                                  (Parser.getI a')
              in Step b' a' case r' of
                Left  err -> Right (Left  (Parser.getG a' /\ err))
                Right val -> Right (Right (cont           /\ val))
  unsafePartial case r of
    Right (Left (g' /\ error)) ->
      Parser \a ->
        Step false (setG g' a) (Left error)
    Right (Right vc) -> pure vc

{-
  Resume a yielded, successful continuation.
 -}
resume
  :: ∀ e c s g i a
   . (Tuple (ParserCont c s g i) a)
  -> Parser e c s g i a
resume ((ParserCont c s g i) /\ v) = Parser \_ -> do
  Step true (ParseArgs c s g i) (Right v)
