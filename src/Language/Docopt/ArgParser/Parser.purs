module Language.Docopt.ArgParser.Parser (
    spec
  , initialState
  , module TypesReexport
  , module OptionsReexport
  ) where

import Prelude
import Debug.Profile
import Control.Plus (empty)
import Debug.Trace
import Data.Foreign
import Data.Map as Map
import Data.Map (Map())
import Control.Apply ((<*), (*>))
import Data.Function (on)
import Data.Function.Uncurried
import Data.Bifunctor (bimap, lmap, rmap)
import Data.Either (Either(Right, Left))
import Data.Maybe (Maybe(..), maybe, fromMaybe, maybe', isNothing, isJust, fromJust)
import Data.List (List(..), reverse, singleton, concat, length, (:),
                  some, filter, head, toUnfoldable, sortBy, groupBy, last, null,
                  tail, many, mapWithIndex, fromFoldable)
import Data.List.Lazy (take, repeat, toUnfoldable) as LL
import Control.Alt ((<|>))
import Data.Traversable (traverse, for)
import Control.Lazy (defer)
import Control.Monad (when, unless)
import Control.MonadPlus (guard)
import Data.Foldable (all, intercalate, maximumBy, sum, any, for_, foldl, elem)
import Data.String as String
import Data.String (fromCharArray, stripPrefix)
import Data.List.Partial as LU
import Data.Array as A
import Data.Array.Partial as AU
import Data.Monoid (class Monoid, mempty)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested ((/\))
import Control.Monad.Transformerless.RWS (RWS(), evalRWS, ask)
import Control.Monad.Transformerless.RWS (modify, get) as State
import Data.StrMap (StrMap())
import Data.Map (Map())
import Data.Map (empty, alter, lookup) as Map
import Control.Monad.Trans (lift)
import Control.MonadPlus.Partial (mrights, mlefts, mpartition)
import Text.Parsing.Parser (PState(..), ParseError(..), ParserT(..), Result(..), fail,
                            parseFailedFatal, parseFailed, unParserT, fatal) as P
import Text.Parsing.Parser.Combinators (option, try, lookAhead, (<?>), choice) as P
import Text.Parsing.Parser.Pos (Position) as P
import Partial.Unsafe (unsafePartial)

import Language.Docopt.ArgParser.Parser.Types as TypesReexport
import Language.Docopt.ArgParser.Parser.Options as OptionsReexport
import Language.Docopt.ArgParser.Parser.Types
import Language.Docopt.ArgParser.Parser.Options
import Language.Docopt.ArgParser.Parser.Token
import Language.Docopt.ArgParser.Parser.Fallback
import Language.Docopt.ArgParser.Parser.Atom

import Language.Docopt.Argument (Argument(..), Branch, isFree,
                                prettyPrintArg, prettyPrintArgNaked,
                                isRepeatable, OptionArgumentObj(),
                                setRequired, isOptional, isGroup,
                                unOptionArgument, isPositional, isCommand) as D
import Language.Docopt.OptionAlias (OptionAlias(..), isLong, isShort) as OptionAlias
import Language.Docopt.Env (Env ())
import Language.Docopt.Specification (Specification())
import Language.Docopt.Env as Env
import Language.Docopt.Origin as Origin
import Language.Docopt.Origin (Origin())
import Language.Docopt.Value as Value
import Language.Docopt.Value (Value(..))
import Language.Docopt.RichValue (RichValue(..), unRichValue, prettyPrintRichValue)
import Language.Docopt.RichValue (from, getOrigin) as RValue
import Language.Docopt.SpecParser.Base (getInput)
import Language.Docopt.ArgParser.Token (getSource) as Token
import Language.Docopt.ArgParser.Token (PositionedToken(..), Token(..),
                                        unPositionedToken, prettyPrintToken)
import Data.String.Ext (startsWith, (~~))

-- | Toggle debugging on/off during development
debug :: Boolean
debug = false

initialState :: StateObj
initialState = {
  depth: 0
, done:  false
, cache: Map.empty
}

withLocalCache :: forall a. Parser a -> Parser a
withLocalCache p = P.ParserT \st -> do
  storedState :: StateObj <- State.get
  State.modify \s -> s { cache = Map.empty :: Cache }
  v <- P.unParserT p st
  State.modify \s -> s { cache = storedState.cache }
  pure v

cached
  :: Required (Indexed D.Argument)
  -> Parser (List (Tuple D.Argument RichValue))
  -> Parser (List (Tuple D.Argument RichValue))
cached a p = P.ParserT \(s@(P.PState i _)) -> do
  state :: StateObj <- State.get
  let key = i /\ a
  maybe'
    (\_ -> do
      v <- P.unParserT p s
      State.modify \s -> s { cache = Map.alter (const (pure v)) key s.cache }
      pure v
    )
    pure
    (Map.lookup key state.cache)

modifyDepth :: (Int -> Int) -> Parser Unit
modifyDepth f = lift (State.modify \s -> s { depth = f s.depth })

markDone :: Parser Unit
markDone = lift (State.modify \s -> s { done = true })

eof :: List D.Branch -> Parser Unit
eof branches = P.ParserT $ \(P.PState s pos) ->
  pure $ case s of
    Nil   -> P.Result s (Right unit) false pos
    (x:_) -> P.parseFailed s pos (unexpected branches x)

unexpected :: List D.Branch -> PositionedToken -> String
unexpected branches (PositionedToken {token: tok, source}) =
  let isKnown = any (any (p tok)) branches -- TODO: cache this
      prefix = if isKnown then "Unexpected " else "Unknown "
    in case tok of
      LOpt _ _   -> prefix <> "option " <> source
      SOpt _ _ _ -> prefix <> "option " <> source
      EOA _      -> prefix <> "option --"
      Stdin      -> prefix <> "option -"
      Lit _      -> prefix <> "command " <> source
  where
    p (LOpt n _)   (D.Option { aliases })   = elem (OptionAlias.Long n) aliases
    p (SOpt s _ _) (D.Option { aliases })   = elem (OptionAlias.Short s) aliases
    p (Lit n)      (D.Command { name: n' }) = n == n'
    p (EOA _)      (D.EOA)                  = true
    p (Stdin)      (D.Stdin)                = true
    p x            (D.Group { branches })   = any (any (p x)) branches
    p _            _                        = false

-- | Parse user input against a program specification.
spec
  :: forall r
   . Specification -- ^ the list of usage branches
  -> Options r    -- ^ argv parser options
  -> Parser (Tuple D.Branch (List ValueMapping))
spec xs options = do
  let
    -- Create a parser for each usage line.
    toplevel = concat xs
    parsers
      = toplevel <#> \branch -> do
          vs <- withLocalCache do
            exhaustP toplevel true false 0 branch
          pure (Tuple branch vs)
          <* eof toplevel

  -- Evaluate the parsers, selecting the result with the most
  -- non-substituted values.
  evalParsers parsers \(Tuple _ vs) ->
    sum $ (Origin.weight <<< _.origin <<< unRichValue <<< snd) <$> vs

  where

  -- Parse a list of arguments.
  -- Take care of clumping free vs. non-free (fixed) arguments.
  exhaustP
    :: forall r
     . List D.Branch   -- ^ the top-level branches
    -> Boolean         -- ^ can we skip using fallback values?
    -> Boolean         -- ^ are we currently skipping using fallback values?
    -> Int             -- ^ recursive level
    -> List D.Argument -- ^ the list of arguments to parse
    -> Parser (List ValueMapping)
  exhaustP toplevel skippable isSkipping l xs = do
    _debug \_->
        "exhaustP: "      <> (intercalate " " $ D.prettyPrintArg <$> xs)
      <> ", skippable: "  <> show skippable
      <> ", isSkipping: " <> show isSkipping

    concat <$> for (clump xs options.laxPlacement
                             options.optionsFirst
                             ) clumpP

    where

    -- Parse a clump of arguments.
    clumpP
      :: forall r
       . Clump (List D.Argument) -- ^ the clumps of arguments to parse.
      -> Parser (List ValueMapping)
    clumpP c = do

      i <- getInput
      _debug \_->
          "parsing clump: " <> prettyPrintArgClump c
        <> ", skippable: "  <> show skippable
        <> ", isSkipping: " <> show isSkipping
        <> ", input: "      <> show (intercalate " " $ Token.getSource <$> i)
      go l c

      where
      go l (Fixed xs) = concat <$> for xs (argP' skippable isSkipping l)

      go l (Free xs) =
        draw
          (xs `mapWithIndex'` \x ix -> Required (Indexed ix x))
          Map.empty
          (length xs)
        where

        draw
          :: List (Required (Indexed D.Argument))
          -> Map D.Argument P.ParseError
          -> Int
          -> Parser (List ValueMapping)

        draw xss@(x:xs) errs n | n >= 0 = (do

          i <- getInput
          _debug \_ ->
              "draw: "          <> (intercalate " " $ prettyPrintRequiredIndexedArg <$> xss)
            <> ", n: "          <> show n
            <> ", skippable: "  <> show skippable
            <> ", isSkipping: " <> show isSkipping
            <> ", input: "      <> show (intercalate " " $ Token.getSource <$> i)

          let
            arg = getIndexedElem (unRequired x)
            mod = if isRequired x then id else P.option Nil
            p   = argP' isSkipping  -- propagate the 'isSkipping' property
                        false       -- reset 'isSkipping' to false
                        (l + 1)     -- increase the recursive level
                        (D.setRequired arg true)

          -- Try parsing the argument 'x'. If 'x' fails, enqueue it for a later
          -- try.  Should 'x' fail and should 'x' be skippable (i.e. it defines
          -- a default value or is backed by an environment variable),
          -- substitute x.  For groups, temporarily set the required flag to
          -- "true", such that it will fail and we have a chance to retry as
          -- part of the exhaustive parsing mechanism.  The 'cached' call
          -- ensures that we only parse a (arg, input) combo once per group.
          vs <- cached x $ P.try $ mod p

          _debug \_->
            "draw: matched: " <> (intercalate ", " $
              (vs) <#> \(Tuple k v) ->
                D.prettyPrintArg k <> " => " <> prettyPrintRichValue v
            )

          vss <- P.try do
            if (D.isRepeatable arg &&
              length (filter (snd >>> isFrom Origin.Argv) vs) > 0)
                then do
                  _debug \_->
                    "draw: repeating as optional: "
                      <> D.prettyPrintArg arg
                  -- Make successive matches of this repeated group optional.
                  P.option Nil do
                    draw (xs <> pure (toOptional x)) errs (length xss)
                else draw xs errs (length xs)

          _debug \_->
            "draw: matched (2): " <> (intercalate ", " $
              (vs <> vss) <#> \(Tuple k v) ->
                D.prettyPrintArg k <> " => " <> prettyPrintRichValue v
            )

          pure (vs <> vss)
        ) `catchParseError` (\e -> do
          let
            arg = getIndexedElem (unRequired x)
            isFixed = not <<< D.isFree
            errs' = if D.isGroup arg || not (D.isFree arg)
                        then Map.alter (const (Just e)) arg errs
                        else errs

          _debug \_->
              "draw: failure"
            <> ", requeueing: " <> prettyPrintRequiredIndexedArg x
            <> ", length of xs: " <> show (length xs)


          if false -- n == 0 || length xs == 0
            -- shortcut: there's no point trying again if there's nothing left
            -- to parse.
            then
              let xs' = if isFixed arg then xss else (xs <> singleton x)
               in draw xs' errs' (-1)
            else
              let isFixed' = isFixed <<< getIndexedElem <<< unRequired
                  xs' =
                    if D.isFree arg
                      then xs <> singleton x
                      -- If a fixed, yet optional argument failed to parse, move
                      -- on without it. We cannot requeue as it would falsify
                      -- the relationship between all positionals.

                      -- XXX: Future work could include slicing off those
                      -- branches in the group that are 'free' and re-queueing
                      -- those.
                      else if D.isOptional arg
                            then xs
                            else sortBy (compare `on` isFixed') xss
               in draw xs' errs' (n - 1)
          )

        -- All arguments have been matched (or have failed to be matched) at least
        -- once by now. See where we're at - is there any required argument that was
        -- not matched at all?
        draw xss@(x:xs) errs n | (n < 0) = do

          i <- getInput
          _debug \_->
              "draw: edge-case"
            <> ", left-over: "  <> (intercalate " " $ prettyPrintRequiredIndexedArg <$> xss)
            <> ", n: "          <> show n
            <> ", skippable: "  <> show skippable
            <> ", isSkipping: " <> show isSkipping
            <> ", input: "      <> show (intercalate " " $ Token.getSource <$> i)

          env <- lift ask

          let
            xss' = sortBy (compare `on` (getIndex <<< unRequired)) xss
            args = getIndexedElem <<< unRequired <$> xss'
            vs   = args <#> \arg -> do
              maybe
                (Left arg)
                (Right <<< Tuple arg)
                do
                  v <- unRichValue <$> getFallbackValue options env arg
                  pure $ RichValue v {
                    value = if D.isRepeatable arg
                        then ArrayValue $ Value.intoArray v.value
                        else v.value
                  }
            missing = mlefts vs `flip filter` (\arg ->
              isRequired x &&
              -- This may look very counter-intuitive, yet getting fallback
              -- values for entire groups is not possible and not logical.
              -- If a group that is allowed to be omitted fails, there won't
              -- be any values to fall back onto.
              not (D.isGroup arg && D.isOptional arg)
            )
            fallbacks = mrights vs

          _debug \_->
            "draw: missing: "
              <> (intercalate " " $ D.prettyPrintArg <$> missing)

          if isSkipping && length missing > 0
            then expected missing i
            else
              if skippable || null i
                then
                  if not isSkipping
                    then withLocalCache do
                      exhaustP toplevel true true l args
                    else pure fallbacks
                else expected args i

          where
          expected xs i =
            let
              x = unsafePartial (LU.head xs)
              msg = maybe'
                (\_ -> case i of
                  i':_ -> unexpected toplevel i'
                            <> ". Expected "
                            <> (intercalate ", " $ D.prettyPrintArgNaked <$> xs)
                  Nil  -> "Missing "
                            <> (intercalate ", " $ D.prettyPrintArgNaked <$> xs)
                )
                (\(P.ParseError msg _ _) -> msg)
                (x `Map.lookup` errs)
            in P.fail msg

        draw _ _ _ = pure Nil

    -- Parse a single argument from argv.
    argP'
      :: forall r
       . Boolean       -- ^ can we skip using fallback values?
      -> Boolean       -- ^ are we currently skipping using fallback values?
      -> Int           -- ^ recursive level
      -> D.Argument    -- ^ the argument to parse
      -> Parser (List ValueMapping)
    argP' skippable isSkipping l x = do
      state :: StateObj <- lift State.get
      if state.done then pure Nil else argP x

      where
      -- Parse a single argument from argv.
      argP :: D.Argument -> Parser (List ValueMapping)

      -- Terminate at singleton groups that house only positionals.
      argP x | options.optionsFirst && isTerm x
        = let y = unsafePartial (fromJust (termAs x))
          in singleton <<< Tuple y <<< (RValue.from Origin.Argv) <$> terminate y

      -- The recursive branch of the argv argument parser.
      -- For groups, each branch is run and analyzed.
      argP g@(D.Group grp) = do
        -- Create a parser for each branch in the group
        let
          parsers = grp.branches <#> \branch ->
            withLocalCache do
              exhaustP toplevel skippable isSkipping l branch

        -- Evaluate the parsers, selecting the result with the most
        -- non-substituted values.
        vs <- (if grp.optional then P.option Nil else id) do
          evalParsers parsers \p ->
            sum $ (Origin.weight <<< _.origin <<< unRichValue <<< snd) <$> p

        hasInput <- not <<< null <$> getInput
        vss <- if (hasInput && grp.repeatable &&
                length (filter (snd >>> isFrom Origin.Argv) vs) > 0)
                  then loop Nil
                  else pure Nil

        pure $ vs <> vss

        where
          loop acc = do
            vs <- do
                argP' skippable isSkipping l
                      (D.Group grp { optional = true })
            if (length (filter (snd >>> isFrom Origin.Argv) vs) > 0)
              then loop $ acc <> vs
              else pure acc

      -- The non-recursive branch of the argv argument parser.
      -- All of these parsers consume one or more adjacent arguments from argv.
      argP x = getInput >>= \i -> (
        (if D.isRepeatable x then some else liftM1 singleton) do
          Tuple x <<< (RValue.from Origin.Argv) <$> do
            P.ParserT $ \s -> do
              (o@(P.Result input result consumed pos)) <- P.unParserT (unsafePartial $ go x) s

              -- Check error messages for fatal errors.
              -- XXX: This should also change.
              case result of
                (Left e) -> do
                  let err = unParseError e
                  if ((startsWith "Option takes no argument" err.message) ||
                      (startsWith "Option requires argument" err.message))
                    then do
                      pure (P.Result
                              input
                              (Left $ P.ParseError err.message err.position true)
                              consumed
                              pos)
                    else pure o
                otherwise -> pure o
          <* modifyDepth (_ + 1)
        ) <|> P.fail ("Expected " <> D.prettyPrintArgNaked x <> butGot i)

        where
        go :: Partial => _ _
        go (D.Positional pos) = positional pos.name
        go (D.Command    cmd) = command    cmd.name
        go (D.Stdin         ) = stdin
        go (D.EOA           ) = eoa <|> (pure $ ArrayValue [])
        go (x@(D.Option   o)) = do

          -- Perform a pre-cursory test in order to capture relevant error
          -- messags which would otherwise be overriden (e.g. a meaningful
          -- error message when trying to match a LOpt against a LOpt would
          -- be overridden by a meaningless try to match a SOpt against a
          -- a LOpt).

          isLoptAhead <- P.option false (P.lookAhead $ token isAnyLopt)
          isSoptAhead <- P.option false (P.lookAhead $ token isAnySopt)

          let
            ns = fromFoldable
                  $ o.aliases <#> case _ of
                    OptionAlias.Short f -> Left  f
                    OptionAlias.Long  n -> Right n
            term = any (_ `elem` options.stopAt) $ o.aliases <#> case _ of
                      OptionAlias.Short s -> "-"  ~~ (String.singleton s)
                      OptionAlias.Long  n -> "--" ~~ n

          -- try each alias
          v /\ canTerm <- case 0 of
            _ | isLoptAhead ->
              let ls = mrights ns
               in if null ls
                    then P.fail "Option has no long alias"
                    else P.choice $ ls <#> \n -> P.try do
                      let mOptArg = D.unOptionArgument <$> o.arg
                      v <- longOption term n mOptArg
                      pure (v /\ true)
            _ | isSoptAhead ->
              let cs = mlefts ns
               in if null cs
                    then P.fail "Option has no short alias"
                    else P.choice $ cs <#> \c -> P.try do
                      let mOptArg = D.unOptionArgument <$> o.arg
                      shortOption term c mOptArg
            _ -> P.fail "Expected long or short option"

          -- try terminating at this option
          if term && canTerm
              then do
                vs <- terminate x
                pure (ArrayValue (Value.intoArray v <> Value.intoArray vs))
              else do
                if isJust o.arg && o.repeatable
                    then do
                      vs <- A.many optionArgument
                      pure (ArrayValue (Value.intoArray v <> vs))
                    else pure v

          where
          isAnyLopt (LOpt _ _)   = pure true
          isAnyLopt _            = pure false
          isAnySopt (SOpt _ _ _) = pure true
          isAnySopt _            = pure false

    butGot ((PositionedToken { source }):_) = ", but got " <> source
    butGot _ = ""

    indentation :: String
    indentation = fromCharArray $ LL.toUnfoldable $ LL.take (l * 4) $ LL.repeat ' '

    _debug s = if debug
                  then traceA $ indentation <> (s unit)
                  else pure unit

-- Evaluate multiple parsers, producing a new parser that chooses the best
-- succeeding match or none.
evalParsers
  :: forall a b
   . (Show a, Ord b)
  => List (Parser a)
  -> (a -> b)
  -> Parser a
evalParsers parsers p = do
  P.ParserT \(pState@(P.PState i pos)) -> do
    env   :: Env      <- ask
    state :: StateObj <- State.get

    let
      -- Run each parser in the underlying monad and capture any errors
      -- that occur as well as the result. This essentially circumvents
      -- `ParserT`'s bind instance.
      collect = for parsers \parser -> do
        -- reset the depth for every branch
        (P.Result s result consumed pos) <- P.unParserT (modifyDepth (const 0) *> parser) pState
        state' :: StateObj <- State.get
        let
          result' = case result of
            (Left err) -> Left  {
              input:    s
            , result:   { error: err, depth: state'.depth + state.depth }
            , consumed: consumed
            , position: pos
            }
            (Right a)  -> Right {
              input:    s
            , result: { value: a, depth: state'.depth + state.depth }
            , consumed: consumed
            , position: pos
            }
        pure result'

      -- Evaluate the parsers.
      results    = fst $ evalRWS collect env initialState
      successes  = mrights results
      successes' = reverse successes
      failures   = mlefts results

      -- Evaluate any failed parses
      losers = last
        $ groupBy (eq `on` (_.depth <<< _.result))
                  (sortBy (compare `on` (_.depth <<< _.result))
                          failures)

      -- Check for any fatal errors. These must take priority!
      -- The first fatal error to occur takes priority.
      fatal = head
        $ filter (\x -> _.fatal $ unParseError (x.result.error))
                  failures

      -- Evaluate any winning candidates.
      -- First, sort the results by their depth, then group consecutive
      -- elements, take the highest element and sort the inner values by using
      -- the user provided function.
      winner = do
        x <- last $ groupBy (eq `on` (_.depth <<< _.result))
                        (sortBy (compare `on` (_.depth <<< _.result))
                                successes')
        maximumBy (compare `on` (\y -> p y.result.value)) x

    -- Finally, evaluate the results!
    maybe
      (do
        maybe'
          (\_ ->
            fromMaybe
              (pure do
                P.parseFailed i pos
                    "The impossible happened. Failure without error")
              (do
                es <- losers
                pure case es of
                  e:es | null es || not (null i) -> do
                    pure (P.Result e.input (Left e.result.error) e.consumed e.position)
                  otherwise -> pure $ P.parseFailed i pos ""
              )
          )
          (\r -> pure (P.Result r.input (Right r.result.value) r.consumed r.position))
          winner
      )
      (\x ->
        let err = unParseError (x.result.error)
         in pure $ P.parseFailedFatal i pos err.message
      )
      fatal

-- Terminate the parser at the given argument and collect all subsequent
-- values int an array ("options-first" and "stop-at")
terminate :: D.Argument -> Parser Value
terminate arg = do
  input <- getInput
  let rest = ArrayValue $ toUnfoldable $ StringValue <<< Token.getSource <$> input
  markDone
  modifyDepth (const 99999)
  P.ParserT \(P.PState _ pos) ->
    pure (P.Result Nil (Right rest) true pos)

isFrom :: Origin -> RichValue -> Boolean
isFrom o rv = o == RValue.getOrigin rv

unParseError :: P.ParseError -> { position :: P.Position, message :: String, fatal :: Boolean }
unParseError (P.ParseError message position fatal) = { message, position, fatal }

mapWithIndex' = flip mapWithIndex

-- Clump together arguments to aid parsing "invisible" subgroups, where
-- arguments may appear in any order, for example. "Fixed" argument lists
-- are parsed in fixed order, whereas "Free" argument lists, can be parsed in
-- any order.
clump
  :: List D.Argument -- ^ the list of arguments to clump
  -> Boolean         -- ^ allow lax placement?
  -> Boolean         -- ^ are we parsing options first?
  -> List (Clump (List D.Argument))
clump xs lax optsFirst = reverse $ foldl go Nil xs
  where
  go (Nil) x = pure $ (if isFree x then Free else Fixed) $ singleton x
  go   ((Free a) :zs) x | isFree x = (Free (a <> (singleton x))) : zs
  go u@((Free _) :_ ) x = (Fixed $ singleton x) : u
  go u@((Fixed _):_ ) x | isFree x = (Free $ singleton x): u
  go   ((Fixed a):zs) x = (Fixed (a <> (singleton x))) : zs
  isFree x = (lax || D.isFree x) && (not (optsFirst && isTerm x))

-- Note: Unfortunate re-implementation of the 'Alt' instance for 'ParserT'.
catchParseError :: forall a. Parser a -> (P.ParseError -> Parser a) -> Parser a
catchParseError p1 f2 = P.ParserT \st ->
  P.unParserT p1 st >>= \(o@(P.Result input result consumed pos)) ->
    case result of
      Left (P.ParseError _ _ true) -> pure o
      Left e | not consumed        -> P.unParserT (f2 e) st
      otherwise                    -> pure o

termAs :: D.Argument -> Maybe D.Argument
termAs (D.Group (grp@{ branches: (x@(D.Positional pos):Nil) : Nil }))
  | pos.repeatable || grp.repeatable = Just x
termAs x@(D.Positional pos) | pos.repeatable = Just x
termAs _ = Nothing

isTerm :: D.Argument -> Boolean
isTerm = isJust <<< termAs
