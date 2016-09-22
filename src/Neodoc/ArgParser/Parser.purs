-- The neodoc arg parser
--
-- Parse the tokenized argv input in accordance with the layout rules specified
-- in the neodoc specification provided by the developer.
--
-- For example:
--
--      usage: prog -a -b -c
--         or: prog -d -e -f=FILE
--
--      options:
--          -f, --file FILE
--
-- This specification has 2 top-level branches, each of which need to be
-- considered individually and scored. The highest ranking top-level wins.
-- Should an error occur during parsing, the "deepest" parse error is elected
-- to be shown. This requires a single parser to track it's own depth.
--
-- Multiple permutations are trialed by runnign the parser on each branch,
-- provided with the same input state.
--
-- The arg parser, during it's operation, yields values of shape:
--
--      Alias => { Origin, Value }
--
-- where `Origin` denotes the origin of the value: provided as an argument to
-- the program, derived from the environment or a default value.
--
--
-- Parsing semantics
-- -----------------
--
-- The neodoc parser parses chunks of adjacent arguments as a unit.
-- These chunks come in 2 flavours: "free" and "fixed". "free" chunks are those
-- chunks where the contained arguments can be parsed freely in any order.
-- "fixed" chunks, on the other hand enforce a strict parse from left to right.
-- When neodoc operates in `lax-placement` mode, the entire list of arguments
-- become a single "free" chunk, but positionals and commands remain fixed
-- amongst each other, but options can be interspersed anywhere.
--
-- For example:
--
--      usage: prog foo bar -a -b -c qux
--
-- is "chunked" into: `<! foo bar !> <* -a -b -c *>`, where `<! ... !>` denotes
-- a "fixed" chunk and a `<* ... *>` denotes a free chunk.
--
-- Generally speaking, elements are elected as part of a "free" chunk if they
-- are options, or "fixed" otherwise.

module Neodoc.ArgParser.Parser where

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

-- Chunk a branch
--   E(foo) G(-a -b -c) E(-x) => [Fixed([E(foo)]), Free([G(-a -b -c), E(-x)])]
chunkBranch :: NonEmpty List SolvedLayout -> List (Chunk (List SolvedLayout))
chunkBranch = fromFoldable >>> chunk case _ of
  Elem _      -> false
  Group _ _ _ -> false

parse
  :: ∀ r
   . Spec SolvedLayout
  -> Options r
  -> Env
  -> List PositionedToken
  -> Either (ParseError ArgParserError) Unit
parse (Spec { layouts, descriptions }) options env tokens =
  runParser { env, options, descriptions } tokens $
    let parsers = layouts <#> \layout ->
                    pure unit
     in void $ evalParsers (\_  -> 0 {- XXX -}) parsers

parseLayout :: ∀ r. SolvedLayout -> ArgParser r Unit

parseLayout (Group o r xs) = do
  pure unit

parseLayout (Elem x) =
  let nTimes = if Solved.isRepeatable x then some else liftM1 singleton
   in unit <$ nTimes do
        go x

  where
  go (Solved.Positional n _) = positional n
  go (Solved.Command    n _) = command    n
  go (Solved.Stdin         ) = stdin
  go (Solved.EOA           ) = eoa <|> (pure $ ArrayValue [])
  go (Solved.Option a  mA _) = fail "..."

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
            Step c' s' result ->
              let cont = ParserCont c' s'
               in Step c' s' case result of
                  Left  (err@(ParseError true _)) -> Left err
                  Left  err -> Right $ ErrorEvaluation   cont 0 {- XXX: store depth on parser -} err
                  Right val -> Right $ SuccessEvaluation cont 0 {- XXX: store depth on parser -} val

  -- Now, check to see if we have any fatal errors, winnders or soft errors.
  -- We know we must have either, but this is not encoded at the type-level,
  -- we have to fail with an internal error message in the impossible case this
  -- is not true.
  case mlefts collected of
    error:_ -> Parser \c s -> Step c s (Left error)
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
      Parser \_ _ -> Step c s (Right val)
    _ -> case deepestErrors of
      Just errors -> case errors of
        (ErrorEvaluation (ParserCont c s) _ e):es | null es || not (null input) ->
          Parser \_ _ -> Step c s (Left e)
        _ -> fail "" -- XXX: explain this
      _ -> fail "The impossible happened. Failure without error"
