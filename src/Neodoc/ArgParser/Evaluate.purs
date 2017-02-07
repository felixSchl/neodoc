module Neodoc.ArgParser.Evaluate where

import Prelude
import Debug.Trace
import Data.Ord
import Data.Tuple (Tuple, fst, snd, uncurry)
import Data.Tuple.Nested ((/\))
import Data.Optimize.Uncurried
import Data.Pretty
import Data.Newtype (unwrap)
import Data.List (
  List(..), some, singleton, filter, fromFoldable, last, groupBy, sortBy, (:)
, null, length, reverse, catMaybes, head)
import Data.List.NonEmpty as NEL
import Data.Function (on)
import Data.Either (Either(..), fromRight)
import Data.NonEmpty (NonEmpty(..), (:|))
import Data.NonEmpty as NE
import Data.NonEmpty.Extra as NE
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

data Cont c s g i e a = ErrorCont (ParserArgs c s g i) e
                      | SuccessCont (ParserArgs c s g i) a

isError :: ∀ c s g i e a. Cont c s g i e a -> Boolean
isError (ErrorCont _ _) = true
isError _ = false

isSuccess :: ∀ c s g i e a. Cont c s g i e a -> Boolean
isSuccess (SuccessCont _ _) = true
isSuccess _ = false

getC :: ∀ c s g i e a. Cont c s g i e a -> c
getC (ErrorCont a _) = Parser.getC a
getC (SuccessCont a _) = Parser.getC a

getS :: ∀ c s g i e a. Cont c s g i e a -> s
getS (ErrorCont a _) = Parser.getS a
getS (SuccessCont a _) = Parser.getS a

getG :: ∀ c s g i e a. Cont c s g i e a -> g
getG (ErrorCont a _) = Parser.getG a
getG (SuccessCont a _) = Parser.getG a

snapshot :: ∀ e c s g i. Parser e c s g i (ParserArgs c s g i)
snapshot = Parser \(a@(ParseArgs c s g i)) -> Step false a (Right a)

chooseBest
  :: ∀ e c s g i a w
   . (Ord w, Show e, Show a)
  => (e -> Int) -- get depth of error
  -> (a -> w)   -- get orderable to compare against other successes
  -> List (Parser e c s g (List i) a)
  -> Parser e c s g (List i) a
chooseBest fE fA xs = snd <$> do
  chooseBestTag fE (const fA) ((unit /\ _) <$> xs)

chooseBestTag
  :: ∀ e c s g i a v w tag
   . (Ord w, Show e, Show a, Show tag)
  => (e -> Int)      -- get depth of error
  -> (tag -> a -> w) -- get orderable to compare against other successes
  -> List (Tuple tag (Parser e c s g (List i) a))
  -> Parser e c s g (List i) (Tuple tag a)
chooseBestTag getErrorDepth cmpSuccess parsers = do
  a@(ParseArgs c s g i) <- getParseState

  -- Run all parsers and collect their results for further evaluation
  let collected = parsers <#> \(tag /\ parser) ->
        runParser' $ Args5 c s g i $ Parser \a' ->
          case unParser parser a' of
            Step b' a''@(ParseArgs c' s' g' i') result ->
              let args' = ParseArgs c' s' g' i'
              in Step b' a'' case result of
                  Left  (err@(ParseError true _)) -> Left err
                  Left  err -> Right $ ErrorCont   args' err
                  Right val -> Right $ SuccessCont args' (tag /\ val)

  -- Now, check to see if we have any fatal errors, winners or errors.
  -- We know we must have either, but this is not encoded at the type-level.
  -- We have to fail with an internal error message in the impossible case this
  -- is not true.
  case mlefts collected of
    error:_ -> Parser \a -> Step false a (Left error)
    _       -> pure unit

  let
    results = mrights collected
    errors = catMaybes $ results <#> case _ of
      ErrorCont a e -> Just (a /\ e)
      _             -> Nothing
    successes = catMaybes $ results <#> case _ of
      SuccessCont a r -> Just (a /\ r)
      _               -> Nothing
    bestSuccess = do
      let depth = uncurry cmpSuccess <<< snd
      deepestGroup <- last do
        groupBy (eq `on` depth) do
          sortBy (compare `on` depth) successes
      -- note: could apply user-defined function on equal elements here to find
      --       best of the best matches.
      pure $ NEL.head deepestGroup
    bestError = do
      let depth = snd >>> case _ of
            ParseError _ (Left _) -> 0
            ParseError _ (Right e) -> getErrorDepth e
      deepestGroup <- last do
        groupBy (eq `on` depth) do
          sortBy (compare `on` depth) errors

      -- note: could apply user-defined function on equal elements here to find
      --       best of the best matches.

      pure $ NEL.head deepestGroup

  case bestSuccess of
    Just (a /\ r) -> Parser \_-> Step true a (Right r)
    _ -> do
      case bestError of
        Just (a /\ e) -> Parser \_ -> Step true a (Left e)
        _ -> Parser.fail "..." -- this should never happen ...
