module Neodoc.ArgParser.Evaluate where

import Prelude
import Data.List (
  List(..), some, singleton, filter, fromFoldable, last, groupBy, sortBy, (:)
, null)
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
import Neodoc.ArgParser.Type
import Neodoc.ArgParser.Options
import Neodoc.ArgParser.Token
import Neodoc.ArgParser.Argument
import Neodoc.ArgParser.Chunk
import Neodoc.ArgParser.Lexer as L

type ChunkedLayout a = Layout (Chunk a)

data ParserCont s c = ParserCont s c
data Evaluation s c e a
  = ErrorEvaluation   (ParserCont s c) Int (ParseError e)
  | SuccessEvaluation (ParserCont s c) Int a

isErrorEvaluation :: ∀ s c e a. Evaluation s c e a -> Boolean
isErrorEvaluation (ErrorEvaluation _ _ _) = true
isErrorEvaluation _ = false

isSuccessEvaluation :: ∀ s c e a. Evaluation s c e a -> Boolean
isSuccessEvaluation (SuccessEvaluation _ _ _) = true
isSuccessEvaluation _ = false

getEvaluationDepth :: ∀ s c e a. Evaluation s c e a -> Int
getEvaluationDepth (ErrorEvaluation _ i _) = i
getEvaluationDepth (SuccessEvaluation _ i _) = i

-- Evaluate multiple parsers, producing a new parser that chooses the best
-- succeeding match or fails otherwise. If any of the parsers yields a fatal
-- error, it is propagated immediately.
evalParsers
  :: ∀ b s a e c f
   . (Foldable f, Functor f, Ord b)
  => (a -> b)
  -> f (Parser e c (List s) a)
  -> Parser e c (List s) a
evalParsers p parsers = do
  config <- getConfig
  input  <- getInput

  -- Run all parsers and collect their results for further evaluation
  let collected = fromFoldable $ parsers <#> \parser ->
        runParser config input $ Parser \c s ->
          case unParser parser c s of
            Step b' c' s' result ->
              let cont = ParserCont c' s'
               in Step b' c' s' case result of
                  Left  (err@(ParseError true _)) -> Left err
                  Left  err -> Right $ ErrorEvaluation   cont 0 {- XXX: store depth on parser -} err
                  Right val -> Right $ SuccessEvaluation cont 0 {- XXX: store depth on parser -} val

  -- Now, check to see if we have any fatal errors, winnders or soft errors.
  -- We know we must have either, but this is not encoded at the type-level,
  -- we have to fail with an internal error message in the impossible case this
  -- is not true.
  case mlefts collected of
    error:_ -> Parser \c s -> Step false c s (Left error)
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
    Just (SuccessEvaluation (ParserCont c s) _ val) ->
      Parser \_ _ -> Step true c s (Right val)
    _ -> case deepestErrors of
      Just errors -> case errors of
        (ErrorEvaluation (ParserCont c s) _ e):es | null es || not (null input) ->
          Parser \_ _ -> Step false c s (Left e)
        _ -> fail "" -- XXX: explain this
      _ -> fail "The impossible happened. Failure without error"
