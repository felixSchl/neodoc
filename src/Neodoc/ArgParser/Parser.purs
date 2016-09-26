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
import Debug.Trace
import Data.List (
  List(..), some, singleton, filter, fromFoldable, last, groupBy, sortBy, (:)
, null, concat, mapWithIndex, length, take, drop)
import Data.List.Partial as LU
import Data.Function (on)
import Data.Tuple (Tuple(..), snd)
import Data.Tuple.Nested ((/\))
import Data.Either (Either(..))
import Data.String as String
import Data.NonEmpty (NonEmpty(..), (:|))
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Traversable (for, traverse)
import Data.Foldable (
  class Foldable, maximumBy, all, intercalate, sum, any, elem)
import Data.Map as Map
import Data.Map (Map)
import Data.Pretty (pretty)
import Control.Alt ((<|>))
import Control.MonadPlus.Partial (mrights, mlefts, mpartition)
import Partial.Unsafe

import Neodoc.Value (Value(..))
import Neodoc.Value as Value
import Neodoc.Value.RichValue (RichValue(..), unRichValue)
import Neodoc.Value.RichValue as RichValue
import Neodoc.Value.Origin (Origin(..))
import Neodoc.Value.Origin as Origin
import Neodoc.Spec (Spec(..))
import Neodoc.Env
import Neodoc.Data.Layout
import Neodoc.Data.Layout as Layout
import Neodoc.Data.Description
import Neodoc.Data.SolvedLayout
import Neodoc.Data.SolvedLayout as Solved
import Neodoc.OptionAlias as OptionAlias
import Neodoc.ArgParser.Type
import Neodoc.ArgParser.Options
import Neodoc.ArgParser.Token
import Neodoc.ArgParser.Argument
import Neodoc.ArgParser.Chunk
import Neodoc.ArgParser.Lexer as L
import Neodoc.ArgParser.Evaluate
import Neodoc.ArgParser.Indexed
import Neodoc.ArgParser.Required
import Neodoc.ArgParser.Combinators
import Neodoc.ArgParser.Fallback

type ChunkedLayout a = Layout (Chunk a)
type ValueMapping = Tuple SolvedLayoutArg RichValue

parse
  :: ∀ r
   . Spec SolvedLayout
  -> Options r
  -> Env
  -> List PositionedToken
  -> Either (ParseError ArgParseError) Unit
parse (Spec { layouts, descriptions }) options env tokens =
  runParser { env, options, descriptions } tokens $
    let parsers = concat (fromFoldable layouts) <#> \layout -> do
          {- XXX: unmarkFailed -}
          vs <- {-withLocalCache-} do
            parseExhaustively true false 0 (fromFoldable layout)
          eof $> Tuple layout vs
     in void $ flip evalParsers parsers \(Tuple _ vs) ->
          sum $ (Origin.weight <<< _.origin <<< unRichValue <<< snd) <$> vs
  where
  eof :: ∀ r. ArgParser r Unit
  eof = do
    input <- getInput
    case input of
      Nil -> pure unit
      xs  -> fail' $ UnexpectedInputError xs

parseLayout
  :: ∀ r
   . Boolean  -- ^ can we skip using fallback values?
  -> Boolean  -- ^ are we currently skipping using fallback values?
  -> Int      -- ^ recursive level
  -> SolvedLayout
  -> ArgParser r (List ValueMapping)

parseLayout _ _ _ (Group o r xs) = do
  pure Nil

parseLayout _ _ _ e@(Elem x) =
  let nTimes = if Solved.isRepeatable e then some else liftM1 singleton
   in Nil <$ nTimes do go x

  where
  go (Solved.Positional n _) = positional n
  go (Solved.Command    n _) = command    n
  go (Solved.Stdin         ) = stdin
  go (Solved.EOA           ) = eoa <|> (pure $ ArrayValue [])
  go (Solved.Option a  mA _) = do
    input       <- getInput
    { options } <- getConfig
    case input of
      (PositionedToken { token }) : _
        | case token of
            LOpt _ _   -> true
            SOpt _ _ _ -> true
            _          -> false
        -> do
          aliases /\ def /\ env <- do
            description <- lookupDescription' a
            case description of
              (OptionDescription aliases _ _ def env) ->
                pure $ aliases /\ def /\ env
              _ -> fail' $ InternalError "invalid option description"
          let
            ns = fromFoldable $ aliases <#> case _ of
                  OptionAlias.Short f -> Left  f
                  OptionAlias.Long  n -> Right n
            longAliases = mrights ns
            shortAliases = mlefts ns
            term = any (_ `elem` options.stopAt) $ aliases <#> case _ of
                      OptionAlias.Short s -> "-"  <> String.singleton s
                      OptionAlias.Long  n -> "--" <> n

          traceA $ pretty a <> " => " <> show aliases

          -- note: safe to be unsafe because of pattern match above
          unsafePartial case token of
            LOpt _ _   -> pure true
            SOpt _ _ _ -> pure true
          fail "..."

      -- (PositionedToken { token: SOpt _ _ }) : _ -> fail "Expected long or short option"
      -- (PositionedToken { token: LOpt _ _ }) : _ -> fail "Expected long or short option"
      _ -> fail "Expected long or short option"

parseExhaustively
  :: ∀ r
   . Boolean -- ^ can we skip using fallback values?
  -> Boolean -- ^ are we currently skipping using fallback values?
  -> Int     -- ^ recursive level
  -> List SolvedLayout
  -> ArgParser r (List ValueMapping)
parseExhaustively skippable isSkipping l xs = do
  { options } <- getConfig
  let chunks = chunkBranch options.laxPlacement options.optionsFirst xs
  concat <$> traverse (parseChunk skippable isSkipping l) chunks

parseChunk
  :: ∀ r
   . Boolean  -- ^ can we skip using fallback values?
  -> Boolean  -- ^ are we currently skipping using fallback values?
  -> Int      -- ^ recursive level
  -> Chunk (List SolvedLayout)
  -> ArgParser r (List ValueMapping)
parseChunk skippable isSkipping l = go
  where
  go (Fixed xs) = concat <$> for xs (parseLayout skippable isSkipping l)
  go (Free  xs) =
    let indexedLayouts = flip mapWithIndex xs \x ix -> Required (Indexed ix x)
     in draw Map.empty (length xs) indexedLayouts

    where
    draw
      :: ∀ r
       . Map SolvedLayout (ParseError ArgParseError)
      -> Int
      -> List (Required (Indexed SolvedLayout))
      -> ArgParser r (List ValueMapping)

    -- Try "drawing" from the input list
    draw errs n xss@(x:xs) | n >= 0 =
      let layout = getIndexedElem (unRequired x)
       in catch' (recover layout) $
          let
            mod = if isRequired x then id else option Nil
            p = parseLayout
                  isSkipping  -- propagate the 'isSkipping' property
                  false       -- reset 'isSkipping' to false
                  (l + 1)     -- increase the recursive level
                  if not isSkipping
                      then setLayoutRequired true layout
                      else layout
           in do
            -- Try parsing the argument 'x'. If 'x' fails, enqueue it for a later
            -- try.  Should 'x' fail and should 'x' be skippable (i.e. it defines
            -- a default value or is backed by an environment variable),
            -- substitute x. For groups, temporarily set the required flag to
            -- "true", such that it will fail and we have a chance to retry as
            -- part of the exhaustive parsing mechanism. The 'cached' call
            -- ensures that we only parse a (arg, input) combo once per group.
            vs <- cached x $ try $ mod p
            vss <- try do
              if (Solved.isRepeatable layout &&
                  length (filter (snd >>> isFrom Origin.Argv) vs) > 0)
                  then draw errs (length xss) (xs <> pure (toOptional x))
                  else draw errs (length xs) xs
            pure $ vs <> vss

      where
      recover layout err =
        let
          isFixed = not <<< isFreeLayout
          errs' = if isGroup layout || isFixed layout
                      then Map.alter (const (Just err)) layout errs
                      else errs
         in do
          -- Check if we're done trying to recover.
          -- See the `draw -1` case below (`markFailed`).

          -- XXX: failed <- hasFailed
          if false {- XXX: failed-}
            then throw err
            -- shortcut: there's no point trying again if there's nothing left
            -- to parse.
            else if n == 0 || length xs == 0
              then
                let xs' = if isFixed layout then xss else (xs <> singleton x)
                 in draw errs' (-1) xs'
              else
                let isFixed' = isFixed <<< getIndexedElem <<< unRequired
                    xs' =
                      if isFreeLayout layout
                        then xs <> singleton x
                        -- XXX: Future work could include slicing off those
                        -- branches in the group that are 'free' and re-queueing
                        -- those.
                        else
                            let fs = take n xss
                                rs = drop n xss
                            in sortBy (compare `on` isFixed') fs <> rs
                 in draw errs' (n - 1) xs'

    -- All arguments have been matched (or have failed to be matched) at least
    -- once by now. See where we're at - is there any required argument that was
    -- not matched at all?
    draw errs n xss@(x:xs) | n < 0 = {- XXX: skipIf isDone Nil -} do
      input <- getInput
      { options, env, descriptions } <- getConfig

      let
        xss' = sortBy (compare `on` (getIndex <<< unRequired)) xss
        layouts = getIndexedElem <<< unRequired <$> xss'
        vs = layouts <#> case _ of
          layout@(Group _ _ _) -> Left layout
          layout@(Elem arg) -> maybe (Left layout) (Right <<< Tuple arg) do
            v <- unRichValue <$> getFallbackValue options env descriptions arg
            pure $ RichValue v {
              value = if isRepeatable layout
                  then ArrayValue $ Value.intoArray v.value
                  else v.value
            }
        missing = mlefts vs `flip filter` \layout ->
          isRequired x &&
          -- This may look very counter-intuitive, yet getting fallback
          -- values for entire groups is not possible and not logical.
          -- If a group that is allowed to be omitted fails, there won't
          -- be any values to fall back onto.
          not (isGroup layout && isOptional layout)
        fallbacks = mrights vs

      if isSkipping && length missing > 0
        then do
          {- XXX: markFailed -}
          throwExpectedError missing input
        else
          if skippable || null input
            then
              if not isSkipping
                then {- XXX: withLocalCache do -}
                  parseExhaustively true true l layouts
                else pure fallbacks
            else throwExpectedError layouts input

      pure Nil

      where
        throwExpectedError
          :: ∀ r
           . List SolvedLayout
          -> List PositionedToken
          -> ArgParser r _
        throwExpectedError xs input =
          let x = unsafePartial (LU.head xs)
              e = case Map.lookup x errs of
                _ -> case input of
                  Nil  -> MissingArgumentsError xs
                  toks -> UnexpectedInputError toks
           in fail' e

    draw _ _ _ = pure Nil

setLayoutRequired :: Boolean -> SolvedLayout -> SolvedLayout
setLayoutRequired b (Group o _ xs) = Group o b xs
setLayoutRequired _ x = x

-- Check if a given layout qualifies as a terminating argument for options-first
-- and if so, return the argument it should be associated with.
termAs :: SolvedLayout -> Maybe SolvedLayoutArg
termAs (Group _ gR (((Elem x@(Solved.Positional _ pR)) :| Nil) :| Nil)) | gR || pR = Just x
termAs (Elem x@(Solved.Positional _ r)) | r = Just x
termAs _ = Nothing

-- Is this layout capable of acting as a terminator for options-first?
canTerm :: SolvedLayout -> Boolean
canTerm = isJust <<< termAs

-- TODO: implement
cached _ = id

-- Chunk a branch
--   E(foo) G(-a -b -c) E(-x) => [Fixed([E(foo)]), Free([G(-a -b -c), E(-x)])]
chunkBranch
  :: Boolean -- enable lax-placement mode
  -> Boolean -- enable options-first mode
  -> List SolvedLayout
  -> List (Chunk (List SolvedLayout))
chunkBranch lax optsFirst = fromFoldable >>> chunk \x ->
  (not (optsFirst && canTerm x)) && (lax || isFreeLayout x)

-- Is this layout considered "free"?
isFreeLayout :: SolvedLayout -> Boolean
isFreeLayout (Elem (Solved.Option _ _ _)) = true
isFreeLayout (Elem _) = false
isFreeLayout (Group _ _ xs) = all (all isFreeLayout) xs

isFrom :: Origin -> RichValue -> Boolean
isFrom o rv = o == RichValue.getOrigin rv
