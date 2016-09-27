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
, null, concat, mapWithIndex, length, take, drop, toUnfoldable)
import Data.Array as Array
import Data.List.Partial as LU
import Data.List.Lazy (take, repeat, toUnfoldable) as LL
import Data.Function (on)
import Data.Tuple (Tuple(..), snd)
import Data.Tuple.Nested ((/\))
import Data.Either (Either(..))
import Data.String as String
import Data.NonEmpty (NonEmpty(..), (:|))
import Data.Maybe (Maybe(..), isJust, maybe, fromJust)
import Data.Traversable (for, traverse)
import Data.Foldable (
  class Foldable, maximumBy, all, intercalate, sum, any, elem)
import Data.Map as Map
import Data.Map (Map)
import Data.Pretty (pretty, class Pretty)
import Control.Alt ((<|>))
import Control.MonadPlus.Partial (mrights, mlefts, mpartition)
import Partial.Unsafe

import Neodoc.Value (Value(..))
import Neodoc.Value as Value
import Neodoc.Value.RichValue (RichValue(..), unRichValue)
import Neodoc.Value.RichValue as RichValue
import Neodoc.Value.Origin (Origin(..))
import Neodoc.Value.Origin as Origin
import Neodoc.Spec (Spec(..), Toplevel)
import Neodoc.Env
import Neodoc.Data.Layout
import Neodoc.Data.Layout as Layout
import Neodoc.Data.Description
import Neodoc.Data.SolvedLayout
import Neodoc.Data.SolvedLayout as Solved
import Neodoc.OptionAlias as OptionAlias
import Neodoc.ArgKey (ArgKey)
import Neodoc.ArgKey.Class (toArgKey)
import Neodoc.ArgParser.Type
import Neodoc.ArgParser.Options
import Neodoc.ArgParser.Token
import Neodoc.ArgParser.Token as Token
import Neodoc.ArgParser.Argument
import Neodoc.ArgParser.Chunk
import Neodoc.ArgParser.Lexer as L
import Neodoc.ArgParser.Evaluate
import Neodoc.ArgParser.Indexed
import Neodoc.ArgParser.Required
import Neodoc.ArgParser.Combinators
import Neodoc.ArgParser.Fallback
import Neodoc.ArgParser.Result
import Neodoc.ArgParser.KeyValue

_ENABLE_DEBUG_ :: Boolean
_ENABLE_DEBUG_ = false

initialState :: ParseState
initialState = {
  depth:  0
, done:   false
, failed: false
}

type ChunkedLayout a = Layout (Chunk a)

parse
  :: ∀ r
   . Spec SolvedLayout
  -> Options r
  -> Env
  -> List PositionedToken
  -> Either (ParseError ArgParseError) ArgParseResult
parse (Spec { layouts, descriptions }) options env tokens =
  runParser { env, options, descriptions } initialState tokens $
    let parsers = concat (fromFoldable layouts) <#> \toplevel ->
          traceBracket 0 ("top-level (" <> pretty toplevel <> ")") do
            unsetFailed
            vs <- {-withLocalCache-} do
              parseExhaustively true false 0 (fromFoldable toplevel)
            eof
            pure $ ArgParseResult toplevel vs
     in flip evalParsers parsers \(ArgParseResult _ vs) ->
          sum $ (Origin.weight <<< _.origin <<< unRichValue <<< snd) <$> vs
  where
  eof :: ∀ r. ArgParser r Unit
  eof = do
    input <- getInput
    case input of
      Nil -> pure unit
      xs  -> fail' $ UnexpectedInputError xs Nil

parseLayout
  :: ∀ r
   . Boolean  -- ^ can we skip using fallback values?
  -> Boolean  -- ^ are we currently skipping using fallback values?
  -> Int      -- ^ recursive level
  -> SolvedLayout
  -> ArgParser r (List KeyValue)
parseLayout skippable isSkipping l layout = do
  { options } <- getConfig
  traceBracket l ("layout (" <> pretty layout <> ")") do
    go options layout

  where

  -- Terminate at singleton groups that house only positionals.
  go options x | options.optionsFirst && isJust (termAs x)
    = let y = unsafePartial (fromJust (termAs x))
      in singleton <<< Tuple (toArgKey y) <<< (RichValue.from Origin.Argv) <$>
          terminate y

  go options (Group o r branches) =
    let branches' = fromFoldable branches
        nEvaluations = length branches'
        parsers = flip mapWithIndex branches' \branch ix ->
          traceBracket (l + 1) ("EVALUTATION " <> show (ix + 1) <> "/" <> show nEvaluations) do
          {-withLocalCache do-}
            parseExhaustively skippable isSkipping l (fromFoldable branch)
    in do
        vs <- (if o then option Nil else id) do
          flip evalParsers parsers \vs ->
            sum $ (Origin.weight <<< _.origin <<< unRichValue <<< snd) <$> vs
        hasInput <- not <<< null <$> getInput
        vss <- if (hasInput && r &&
                length (filter (snd >>> isFrom Origin.Argv) vs) > 0)
                  then loop Nil
                  else pure Nil
        pure $ vs <> vss

    where
      loop acc = do
        -- parse this group repeatedly, but make successive matches optional.
        vs <- go options (Group true r branches)
        if (length (filter (snd >>> isFrom Origin.Argv) vs) > 0)
          then loop $ acc <> vs
          else pure acc

  go _ e@(Elem x) =
    let nTimes = if Solved.isRepeatable e then some else liftM1 singleton
     in nTimes do
          Tuple (toArgKey x) <<< (RichValue.from Origin.Argv) <$> go' x
            <* modifyDepth (_ + 1)

    where
    go' (Solved.Positional n _) = positional (pretty x) n
    go' (Solved.Command    n _) = command    (pretty x) n
    go' (Solved.Stdin         ) = stdin
    go' (Solved.EOA           ) = eoa <|> (pure $ ArrayValue [])
    go' (Solved.Option a  mA r) = do
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


            -- note: safe to be unsafe because of pattern match above
            OptRes v canTerm canRepeat <- unsafePartial case token of
              LOpt _ _ ->
                case longAliases of
                  Nil -> fail "Option has no long alias"
                  _   -> choice $ longAliases <#> \alias -> try do
                          longOption term alias mA
              SOpt _ _ _ ->
                case shortAliases of
                  Nil -> fail "Option has no short alias"
                  _   -> choice $ shortAliases <#> \alias -> try do
                          shortOption term alias mA

            -- try terminating at this option
            if term && canTerm
                then do
                  vs <- terminate x
                  pure (ArrayValue (Value.intoArray v <> Value.intoArray vs))
                else do
                  if isJust mA && r && canRepeat
                      then do
                        vs <- Array.many optionArgument
                        pure (ArrayValue (Value.intoArray v <> vs))
                      else pure v
        _ -> fail "Expected long or short option"


parseExhaustively
  :: ∀ r
   . Boolean -- ^ can we skip using fallback values?
  -> Boolean -- ^ are we currently skipping using fallback values?
  -> Int     -- ^ recursive level
  -> List SolvedLayout
  -> ArgParser r (List KeyValue)
parseExhaustively skippable isSkipping l xs = do
  { options } <- getConfig
  let chunks = chunkBranch options.laxPlacement options.optionsFirst xs
  concat <$> traverse (parseChunk skippable isSkipping (l + 2)) chunks

parseChunk
  :: ∀ r
   . Boolean  -- ^ can we skip using fallback values?
  -> Boolean  -- ^ are we currently skipping using fallback values?
  -> Int      -- ^ recursive level
  -> Chunk (List SolvedLayout)
  -> ArgParser r (List KeyValue)
parseChunk skippable isSkipping l chunk = do
  traceBracket l ("chunk (" <> pretty chunk <> ")") do
    go chunk

  where
  traceDraw :: ∀ a. (Pretty a) => Int -> (List a) -> String -> ArgParser r Unit
  traceDraw n xss msg = trace l \input -> do
    "draw (" <> show n <> "|" <> show skippable <> "|" <> show isSkipping <> "): "
      <> (if String.length msg > 0 then " " <> msg else "")
      <> " (elems: " <> pretty xss <> ")"
      <> " (input: " <> pretty input <> ")"

  go (Fixed xs) = concat <$> for xs (parseLayout skippable isSkipping l)
  go (Free  xs) =
    let indexedLayouts = flip mapWithIndex xs \x ix -> Required (Indexed ix x)
     in draw Map.empty (length xs) indexedLayouts

    where
    draw
      :: Map SolvedLayout (ParseError ArgParseError)
      -> Int
      -> List (Required (Indexed SolvedLayout))
      -> ArgParser r (List KeyValue)

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
            traceDraw n xss ""

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
          failed <- hasFailed
          traceDraw n xss $ "! ERROR - (state.failed = " <> show failed
                            <> ", error = " <> pretty err <> ")"
          if failed
            then do
              traceDraw n xss $ "! ABORTING (failed)"
              throw err

            -- shortcut: there's no point trying again if there's nothing left
            -- to parse.
            else if n == 0 || length xs == 0
              then
                let xs' = if isFixed layout then xss else (xs <> singleton x)
                 in do
                    traceDraw n xss $ "! Skipping (shortcut)"
                    draw errs' (-1) xs'
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
                 in do
                    traceDraw n xss $ "...retrying"
                    draw errs' (n - 1) xs'

    -- All arguments have been matched (or have failed to be matched) at least
    -- once by now. See where we're at - is there any required argument that was
    -- not matched at all?
    draw errs n xss@(x:xs) | n < 0 = skipIf isDone Nil do
      input <- getInput
      { options, env, descriptions } <- getConfig

      traceDraw n xss $ ""

      let
        xss' = sortBy (compare `on` (getIndex <<< unRequired)) xss
        layouts = getIndexedElem <<< unRequired <$> xss'
        vs = layouts <#> case _ of
          layout@(Group _ _ _) -> Left layout
          layout@(Elem arg) -> maybe (Left layout) (Right <<< Tuple (toArgKey arg)) do
            let description = case arg of
                  (Option alias _ _) -> findDescription alias descriptions
                  _                  -> Nothing
            v <- unRichValue <$> getFallbackValue options env description arg
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
          setFailed
          throwExpectedError missing input
        else
          if skippable || null input
            then
              if not isSkipping
                then {- XXX: withLocalCache do -}
                  parseExhaustively true true l layouts
                else pure fallbacks
            else throwExpectedError layouts input

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
                  toks -> UnexpectedInputError toks xs
           in fail' e

    draw _ _ _ = pure Nil

setLayoutRequired :: Boolean -> SolvedLayout -> SolvedLayout
setLayoutRequired b (Group _ r xs) = Group (not b) r xs
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


-- Terminate the parser at the given argument and collect all subsequent
-- values int an array ("options-first" and "stop-at")
terminate :: ∀ r. SolvedLayoutArg -> ArgParser r Value
terminate arg = do
  input <- getInput
  let rest = ArrayValue $ toUnfoldable $ StringValue <<< Token.getSource <$> input

  -- XXX: yeah, this is functional programming, alright. this needs to be
  -- re-visited. How can we avoid at least `setDone` and `setDepth`?
  setDone
  setDepth 99999
  setInput Nil

  pure rest

trace :: ∀ r. Int -> (List PositionedToken -> String) -> ArgParser r Unit
trace l f = if _ENABLE_DEBUG_
              then do
                input <- getInput
                traceA $ indent l <> (f input)
              else pure unit

traceError :: ∀ r a. Int -> String -> ArgParser r a  -> ArgParser r a
traceError l s
  = catch' (\e -> trace l (\_ -> "! " <> s <> ": " <> pretty e) *> throw e)

traceInput :: ∀ r. ArgParser r Unit
traceInput = traceA =<< pretty <$> getInput

indent :: Int -> String
indent l = String.fromCharArray $ LL.toUnfoldable $ LL.take (l * 4) $ LL.repeat ' '

traceBracket
  :: ∀ r a
   . (Pretty a)
  => Int
  -> String
  -> ArgParser r a
  -> ArgParser r a
traceBracket l label p = do
  input <- getInput
  trace l \_ -> "parsing " <> label <> "... (input: " <> pretty input <> ")"
  output <- traceError l ("failed to parse " <> label) p
  input <- getInput
  trace l \_ ->
    "successfully parsed " <> label <> "!"
      <> " (output: " <> pretty output <> ")"
      <> " (new input: " <> pretty input <> ")"
  pure output
