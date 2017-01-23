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

isErrorEvaluation :: ∀ c s g i e a. Evaluation c s g i e a -> Boolean
isErrorEvaluation (ErrorEvaluation _ _) = true
isErrorEvaluation _ = false

isSuccessEvaluation :: ∀ c s g i e a. Evaluation c s g i e a -> Boolean
isSuccessEvaluation (SuccessEvaluation _ _) = true
isSuccessEvaluation _ = false

getEvaluationDepth :: ∀ c g i e a. Evaluation c ArgParseState g i e a -> Int
getEvaluationDepth (ErrorEvaluation (ParserCont _ s _ _) _) = s.depth
getEvaluationDepth (SuccessEvaluation (ParserCont _ s _ _) _) = s.depth

-- Evaluate multiple parsers, producing a new parser that chooses the best
-- succeeding match or fails otherwise. If any of the parsers yields a fatal
-- error, it is propagated immediately.
evalParsers
  :: ∀ b a r
   . (Ord b)
  => Args2 (a -> b) (List (ArgParser r a))
  -> ArgParser r a
evalParsers (Args2 _ parsers) | length parsers == 0
  = fail' $ internalError "no parsers to evaluate"

evalParsers (Args2 p parsers) = do
  (ParseArgs config state globalState input) <- getParseState

  -- Run all parsers and collect their results for further evaluation
  let collected = parsers <#> \parser ->
        runParser' $ Args5 config state globalState input $ Parser \a ->
          case unParser parser a of
            Step b' a'@(ParseArgs c' s' g' i') result ->
              let cont = ParserCont c' s' g' i'
               in Step b' a' case result of
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
    successes = reverse $ filter isSuccessEvaluation results
    eqByDepth = eq `on` getEvaluationDepth
    cmpByDepth = compare `on` getEvaluationDepth
    errors = filter isErrorEvaluation results
    deepestErrors = last $ groupBy eqByDepth $ sortBy cmpByDepth errors
    bestSuccess = do
      deepest <- last $ groupBy eqByDepth $ sortBy cmpByDepth successes
      unsafePartial $ flip maximumBy deepest $ compare `on` case _ of
        SuccessEvaluation _ v -> p v

  -- Ok, now that we have a potentially "best error" and a potentially "best
  -- match", take a pick.
  case bestSuccess of
    Just (SuccessEvaluation (ParserCont c s g i) val) -> do
      applyResults results $ Parser \_ -> Step true (ParseArgs c s g i) (Right val)
    _ -> case deepestErrors of
      Just errors -> case unwrap errors of
        (ErrorEvaluation (ParserCont c s g i) e) :| es | null es || not (null input) -> do
          applyResults results $ Parser \_ -> Step false (ParseArgs c s g i) (Right unit)
          { depth        } <- getState
          { deepestError } <- getGlobalState
          case deepestError of
            Just (d /\ e) | d > depth -> fail' e
            _ -> throw e
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
          _ -> pure unit
      SuccessEvaluation (ParserCont _ { depth } { deepestError } _) _ -> do
        case deepestError of
          Just (d /\ e) -> setErrorAtDepth d e
          _ -> pure unit
    pure out

type Cont r = ParserCont (ParseConfig r)
                          ArgParseState
                          GlobalArgParseState
                          (List PositionedToken)

{-
  Yield a successful continuation or fail.
  The parse does not influence the outer parser state with
  the exception of "global state".

  The reason for modifying the global state in face of an
  error is that we can propagate the "deepestError" value.
 -}
fork
  :: ∀ r a
   . ArgParser r a
  -> ArgParser r (Tuple (Cont r) a)
fork parser = do
  config      <- getConfig
  state       <- getState
  globalState <- getGlobalState
  input       <- getInput
  let result = runParser' $ Args5 config state globalState input $ Parser \a ->
        case unParser parser a of
          Step b' a' result ->
          --  TODO: use tuple in `ParserCont`
            let cont = ParserCont (getC a') (getS a') (getG a') (getI a')
              in Step b' a' case result of
                Left  err -> Right (Left  (getG a' /\ err))
                Right val -> Right (Right (cont    /\ val))
  unsafePartial case result of
    Right (Left (g /\ error)) ->
      Parser \a ->
        Step false (setG g a) (Left error)
    Right (Right vc) -> pure vc

{-
  Resume a yielded, successful continuation.
 -}
resume
  :: ∀ r a
   . (Tuple (Cont r) a)
  -> ArgParser r a
resume ((ParserCont c s g i) /\ v) = Parser \_ -> Step true (ParseArgs c s g i) (Right v)
