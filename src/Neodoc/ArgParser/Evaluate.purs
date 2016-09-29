module Neodoc.ArgParser.Evaluate where

import Prelude
import Debug.Trace
import Data.List (
  List(..), some, singleton, filter, fromFoldable, last, groupBy, sortBy, (:)
, null, length)
import Data.Function (on)
import Data.Either (Either(..))
import Data.NonEmpty (NonEmpty(..), (:|))
import Data.Maybe (Maybe(..))
import Data.Traversable (for)
import Data.Foldable (class Foldable, maximumBy)
import Control.Alt ((<|>))
import Control.MonadPlus.Partial (mrights, mlefts, mpartition)
import Partial.Unsafe

import Neodoc.Value (Value(..))
import Neodoc.Spec (Spec(..))
import Neodoc.Env
import Neodoc.Data.Layout
import Neodoc.Data.SolvedLayout
import Neodoc.Data.SolvedLayout as Solved
import Neodoc.Data.Chunk
import Neodoc.ArgParser.Type
import Neodoc.ArgParser.Options
import Neodoc.ArgParser.Token
import Neodoc.ArgParser.Argument
import Neodoc.ArgParser.Lexer as L

type ChunkedLayout a = Layout (Chunk a)

data ParserCont c s i = ParserCont c s i
data Evaluation c s i e a
  = ErrorEvaluation   (ParserCont c s i) Int (ParseError e)
  | SuccessEvaluation (ParserCont c s i) Int a

instance showParserCont :: (Show c, Show s, Show i) => Show (ParserCont c s i) where
  show (ParserCont c s i) = "ParserCont " <> show c <> " " <> show s <> " " <> show i

instance showEvaluation :: (Show c, Show s, Show i, Show e, Show a) => Show (Evaluation c s i e a) where
  show (ErrorEvaluation   c i e) = "ErrorEvaluation " <> show c <> " " <> show i <> " " <> show e
  show (SuccessEvaluation c i a) = "SuccessEvaluation " <> show c <> " " <> show i <> " " <> show a

isErrorEvaluation :: ∀ c s i e a. Evaluation c s i e a -> Boolean
isErrorEvaluation (ErrorEvaluation _ _ _) = true
isErrorEvaluation _ = false

isSuccessEvaluation :: ∀ c s i e a. Evaluation c s i e a -> Boolean
isSuccessEvaluation (SuccessEvaluation _ _ _) = true
isSuccessEvaluation _ = false

getEvaluationDepth :: ∀ c s i e a. Evaluation c s i e a -> Int
getEvaluationDepth (ErrorEvaluation _ i _) = i
getEvaluationDepth (SuccessEvaluation _ i _) = i

-- Evaluate multiple parsers, producing a new parser that chooses the best
-- succeeding match or fails otherwise. If any of the parsers yields a fatal
-- error, it is propagated immediately.
evalParsers
  :: ∀ b s i a e c r
   . (Ord b)
  => (a -> b)
  -> List (ArgParser r a)
  -> ArgParser r a
evalParsers _ parsers | length parsers == 0
  = fail' $ InternalError "no parsers to evaluate"

evalParsers p parsers = do

  config <- getConfig
  state  <- getState
  input  <- getInput

  -- Run all parsers and collect their results for further evaluation
  let collected = fromFoldable $ parsers <#> \parser ->
        runParser config (state { depth = 0 }) input $ Parser \c s i ->
          case unParser parser c s i of
            Step b' c' s' i' result ->
              let cont = ParserCont c' s' i'
               in Step b' c' s' i' case result of
                  Left  (err@(ParseError true _)) -> Left err
                  Left  err -> Right $ ErrorEvaluation   cont s.depth err
                  Right val -> Right $ SuccessEvaluation cont s.depth val

  -- Now, check to see if we have any fatal errors, winnders or soft errors.
  -- We know we must have either, but this is not encoded at the type-level,
  -- we have to fail with an internal error message in the impossible case this
  -- is not true.
  case mlefts collected of
    error:_ -> Parser \c s i -> Step false c s i (Left error)
    _       -> pure unit

  let
    results = mrights collected
    errors = filter isErrorEvaluation results
    successes = filter isSuccessEvaluation results
    eqByDepth = eq `on` getEvaluationDepth
    cmpByDepth = compare `on` getEvaluationDepth
    deepestErrors = last $ groupBy eqByDepth $ sortBy cmpByDepth errors
    bestSuccess = do
      deepest <- last $ groupBy eqByDepth $ sortBy cmpByDepth successes
      unsafePartial $ flip maximumBy deepest $ compare `on` case _ of
        SuccessEvaluation _ _ v -> p v

  -- Ok, now that we have a potentially "best error" and a potentially "best
  -- match", take a pick.
  case bestSuccess of
    Just (SuccessEvaluation (ParserCont c s i) _ val) ->
      Parser \_ _ _ -> Step true c (state { done = s.done }) i (Right val)
    _ -> case deepestErrors of
      Just errors -> case errors of
        (ErrorEvaluation (ParserCont c s i) _ e):es | null es || not (null input) ->
          Parser \_ _ _ -> Step false c s i (Left e)
        _ -> fail "" -- XXX: explain this
      _ -> do
        fail "The impossible happened. Failure without error"
