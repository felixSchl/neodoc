module Neodoc.ArgParser.Evaluate where

import Prelude
import Debug.Trace
import Data.Tuple.Nested ((/\))
import Data.Pretty
import Data.List (
  List(..), some, singleton, filter, fromFoldable, last, groupBy, sortBy, (:)
, null, length, reverse)
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

data ParserCont c s g i = ParserCont c s g i
data Evaluation c s g i e a
  = ErrorEvaluation   (ParserCont c s g i) (ParseError e)
  | SuccessEvaluation (ParserCont c s g i) a

instance showParserCont :: (Show c, Show s, Show g, Show i) => Show (ParserCont c s g i) where
  show (ParserCont c s g i) = "ParserCont " <> show c <> " " <> show s <> " " <> show g <> " " <> show i

instance showEvaluation :: (Show c, Show s, Show g, Show i, Show e, Show a) => Show (Evaluation c s g i e a) where
  show (ErrorEvaluation   c e) = "ErrorEvaluation " <> show c <> " " <> show e
  show (SuccessEvaluation c a) = "SuccessEvaluation " <> show c <> " " <> show a

isErrorEvaluation :: ∀ c s g i e a. Evaluation c s g i e a -> Boolean
isErrorEvaluation (ErrorEvaluation _ _) = true
isErrorEvaluation _ = false

isSuccessEvaluation :: ∀ c s g i e a. Evaluation c s g i e a -> Boolean
isSuccessEvaluation (SuccessEvaluation _ _) = true
isSuccessEvaluation _ = false

getEvaluationDepth :: ∀ c g i e a. Evaluation c ParseState g i e a -> Int
getEvaluationDepth (ErrorEvaluation (ParserCont _ s _ _) _) = s.depth
getEvaluationDepth (SuccessEvaluation (ParserCont _ s _ _) _) = s.depth

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
  = fail' $ internalError "no parsers to evaluate"

evalParsers p parsers = do

  config      <- getConfig
  state       <- getState
  globalState <- getGlobalState
  input       <- getInput

  -- Run all parsers and collect their results for further evaluation
  let collected = fromFoldable $ parsers <#> \parser ->
        runParser config state globalState input $ Parser \c s g i ->
          case unParser parser c s g i of
            Step b' c' s' g' i' result ->
              let cont = ParserCont c' s' g' i'
               in Step b' c' s' g' i' case result of
                  Left  (err@(ParseError true _)) -> Left err
                  Left  err -> Right $ ErrorEvaluation   cont err
                  Right val -> Right $ SuccessEvaluation cont val

  -- Now, check to see if we have any fatal errors, winners or soft errors.
  -- We know we must have either, but this is not encoded at the type-level,
  -- we have to fail with an internal error message in the impossible case this
  -- is not true.
  case mlefts collected of
    error:_ -> Parser \c s g i -> Step false c s g i (Left error)
    _       -> pure unit

  let
    results = mrights collected
    errors = filter isErrorEvaluation results
    successes = reverse $ filter isSuccessEvaluation results
    eqByDepth = eq `on` getEvaluationDepth
    cmpByDepth = compare `on` getEvaluationDepth
    deepestErrors = last $ groupBy eqByDepth $ sortBy cmpByDepth errors
    bestSuccess = do
      deepest <- last $ groupBy eqByDepth $ sortBy cmpByDepth successes
      unsafePartial $ flip maximumBy deepest $ compare `on` case _ of
        SuccessEvaluation _ v -> p v

  -- Ok, now that we have a potentially "best error" and a potentially "best
  -- match", take a pick.
  case bestSuccess of
    Just (SuccessEvaluation (ParserCont c s g i) val) -> do
      applyResults results $ Parser \_ _ _ _ -> Step true c s g i (Right val)
    _ -> case deepestErrors of
      Just errors -> case errors of
        (ErrorEvaluation (ParserCont c s g i) e):es | null es || not (null input) -> do
          applyResults results $ Parser \_ _ _ _ -> Step false c s g i (Right unit)
          throw e
        _ -> fail "" -- XXX: explain this
      _ -> fail "The impossible happened. Failure without error"

  where
  applyResults :: ∀ r a. _ -> ArgParser r a -> ArgParser r a
  applyResults results p = do
    out <- p
    -- transport adjacent error messages up, even on success. should we fail
    -- at a later stage, we have access to this valuable information and
    -- can present the best error message possible.
    unsafePartial $ for results case _ of
      ErrorEvaluation (ParserCont _ { depth } { deepestError } _) e -> do
        case deepestError of
          Just (d /\ e) -> setErrorAtDepth d e
          _ -> setErrorAtDepth depth (extractError genericError e)
      SuccessEvaluation (ParserCont _ { depth } { deepestError } _) _ -> do
        case deepestError of
          Just (d /\ e) -> setErrorAtDepth d e
          _ -> pure unit
    pure out
