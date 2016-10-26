module Neodoc.ArgParser.Parser where

import Prelude
import Debug.Trace hiding (trace)
import Debug.Profile
import Data.Generic
import Data.List (
  List(..), some, singleton, filter, fromFoldable, last, groupBy, sortBy, (:)
, null, concat, mapWithIndex, length, take, drop, toUnfoldable, catMaybes, nub
, reverse)
import Data.Array as Array
import Data.List.Partial as LU
import Data.Bifunctor (lmap)
import Data.Set as Set
import Data.List.Lazy (take, repeat, toUnfoldable) as LL
import Data.List.Partial as LU
import Data.Function (on)
import Data.Tuple (Tuple(..), snd, fst)
import Data.Tuple.Nested ((/\))
import Data.Either (Either(..))
import Data.String as String
import Data.NonEmpty (NonEmpty(..), (:|))
import Data.Maybe (Maybe(..), isJust, maybe, fromJust)
import Data.Traversable (for, traverse)
import Data.Foldable.Extra (findMap)
import Data.Foldable (
  class Foldable, maximumBy, all, intercalate, sum, any, elem, find)
import Data.Map as Map
import Data.Map (Map)
import Data.Pretty (pretty, class Pretty)
import Control.Alt ((<|>))
import Control.MonadPlus.Partial (mrights, mlefts, mpartition)
import Control.Monad.State
import Control.Monad.State as State
import Partial.Unsafe
import Data.NonEmpty.Extra as NonEmpty

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
import Neodoc.Data.Chunk
import Neodoc.Data.Indexed
import Neodoc.Data.IndexedLayout
import Neodoc.Data.LayoutConversion
import Neodoc.OptionAlias (OptionAlias)
import Neodoc.OptionAlias as OptionAlias
import Neodoc.ArgKey (ArgKey)
import Neodoc.ArgKey.Class (toArgKey)
import Neodoc.ArgParser.Type
import Neodoc.ArgParser.Options
import Neodoc.ArgParser.Token
import Neodoc.ArgParser.Token as Token
import Neodoc.ArgParser.Argument
import Neodoc.ArgParser.Lexer as L
import Neodoc.ArgParser.Evaluate
import Neodoc.ArgParser.Required
import Neodoc.ArgParser.Combinators
import Neodoc.ArgParser.Fallback
import Neodoc.ArgParser.Result
import Neodoc.ArgParser.KeyValue
import Neodoc.ArgParser.Profile
import Neodoc.ArgParser.Arg hiding (getId)
import Neodoc.ArgParser.Debug
import Neodoc.ArgParser.Arg as Arg
import Neodoc.ArgParser.ParseLayout

type ChunkedLayout a = Layout (Chunk a)

initialState :: ParseState
initialState = {
  depth: 0
, hasTerminated: false
}

initialGlobalState :: GlobalParseState
initialGlobalState = {
  deepestError: Nothing
, isKnownCache: Map.empty
, matchCache: Map.empty
, argCache: Map.empty
}

parse
  :: ∀ r
   . Spec SolvedLayout
  -> Options r
  -> Env
  -> List PositionedToken
  -> Either (ParseError ArgParseError) ArgParseResult
parse (spec@(Spec { layouts, descriptions })) options@{ helpFlags, versionFlags } env tokens =
  let hasEmpty = any null layouts
      toplevels = concat (NonEmpty.toList layouts)

      -- construct a parser for each branch. we must yield the list key-values
      -- along-side the branch that matched the result.
      parsers :: List (ArgParser r ArgParseResult)
      parsers = toplevels <#> \branch ->
        let branch' = toArgBranch options env descriptions branch
            branch'' = toParseBranch (NonEmpty.toList branch')
         in ArgParseResult (Just branch) <$>
              parseBranch 0 true branch'' <* eof

      parsers' :: List (ArgParser r ArgParseResult)
      parsers' = parsers
          -- if there were empty any layouts, such as for example:
          --    usage: prog
          --       or: prog
          -- then we consolidate those into a single, artificial
          -- parse whose only requirement is that there be no input.
          <> if hasEmpty
                then pure $ ArgParseResult Nothing Nil <$ eof
                else Nil

   in runParser { env, options, spec } initialState initialGlobalState tokens $
        let p = if null parsers'
                  then eof $> ArgParseResult Nothing Nil
                  else evalParsers (byOrigin <<< getResult) parsers'
         in p `catch` \_ e ->
            let implicitFlags = helpFlags <> versionFlags
                -- note: must re-set the cache since the running ids in this
                --       implicit branch may overlap.
                mImplicitP = withLocalCaches <$> do
                  mkImplicitToplevelP implicitFlags false
            in case mImplicitP of
                  Just implicitP -> implicitP <|> throw e
                  _              -> throw e

  where
  eof :: ∀ r. ArgParser r Unit
  eof = do
    input <- getInput
    case input of
      Nil  -> pure unit
      toks -> do
        kToks <- for toks \(pTok@(PositionedToken tok _ _)) -> do
          isKnown <- isKnownToken' tok
          pure if isKnown
            then known pTok
            else unknown pTok
        fail' $ unexpectedInputError Nil kToks

  -- create an implicit top-level to make "-h/--help" and "--version" "just
  -- work". The idea is to remove the empty fallback for '--help' and
  -- '--version' in order to fail that top-level branch.
  mkImplicitToplevelP :: List OptionAlias -> Boolean -> _
  mkImplicitToplevelP flags o = case flags of
    Nil -> Nothing
    f : fs -> pure
      let args = toOption f :| (toOption <$> fs)
          branch = (Group o true $ args <#> \a -> Elem a :| Nil) :| Nil
          branch' = (Group o true $ args <#> \a ->
                      (Elem $  Arg 0
                                  a
                                  (toArgKey a)
                                  (if o
                                    then Just $ RichValue.from Origin.Empty $ BoolValue false
                                    else Nothing)
                                  false
                      ) :| Nil
                    ) :| Nil
          branch'' = toParseBranch (NonEmpty.toList branch')
       in ArgParseResult (Just branch) <$>
            parseBranch 0 true branch'' <* eof
    where
    toOption :: OptionAlias -> SolvedLayoutArg
    toOption a = Option a Nothing true

parseBranch
  :: ∀ r
   . Int
  -> Boolean -- allow substitutions?
  -> List ArgParseLayout
  -> ArgParser r (List KeyValue)
parseBranch _ _ Nil = pure Nil
parseBranch l sub xs = do
  { options } <- getConfig
  let xs' = if not options.laxPlacement
            then groupBy (eq `on` _isFree) xs
            else singleton xs
  concat <$> for xs' (solve l options.repeatableOptions sub)
  where
  _isFree :: ArgParseLayout -> Boolean
  _isFree (ParseGroup _ f _ _ _) = f
  _isFree (ParseElem _ f _) = f

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

{-
  we iterate over the set of required arguments `req`.
  should we fail to make a match for any `x` in `req`, we move `x` to `res`.
  should we be able to make a match with a successive `x'` in `req`, we
  release all arguments from the "reserve" (`res`) back into `req` and re-
  evaluate. should all `x` in `req` fail, we try to consume *anything* with
  `rep`. if the input changes, we keep going - if it does not we conclude that
  there's no match on this branch.
-}
solve
  :: ∀ r
   . Int                 -- recursive level
  -> Boolean             -- `opts.repeatableOptions`
  -> Boolean             -- allow substitutions?
  -> List ArgParseLayout -- the required arguments
  -> ArgParser r (List KeyValue)
solve l repOpts sub req = skipIf hasTerminated Nil $ go l sub req Nil true Nil
  where
  go
    :: Int                 -- the recursive level
    -> Boolean             -- allow substitutions?
    -> List ArgParseLayout -- the required arguments
    -> List ArgParseLayout -- repeatable arguments
    -> Boolean             -- can apply repetition?
    -> List KeyValue       -- the output
    -> ArgParser r (List KeyValue)
  go l' sub' req rep canRep out = do
    input       <- getInput
    { options } <- getConfig
    { depth }   <- getState

    -- trace l' \i->
    --   "solve: req = " <> pretty req
    --     <> ", rep = " <> pretty rep
    --     <> ", sub = " <> show sub
    --     <> ", out = " <> pretty out
    --     <> ", i = "   <> pretty i


    -- 1. try making a match for any arg in `req` w/o allowing substitutions.
    --    if we make a match, we proceed *and never look back*. If we do not
    --    succeed with any argument, we try the `rep` list.
    -- trace l' \i-> "solve: trying via argv, i = " <> pretty i
    mKv /\ req' /\ _ /\ mNewRep <-
      (_lmap Just <$> (try $ match (l' + 1) false req)) <|>
        pure (Nothing /\ req /\ false /\ Nothing)

    -- trace l' \i-> "solve: req' = " <> pretty req'
    --                 <> " out = " <> show (pretty <$> mKv)
    --                 <> ", i = " <> pretty i

    case mKv of
      Just kvs@(_:_) -> do
        -- trace l' \i-> "solve: matched on argv: " <> pretty kvs <> ", i = " <> pretty i
        let rRep = _toElem <$> filter (_isRepeatable) (fst <$> kvs)
            rep' = nub ((maybe Nil singleton mNewRep) <> rep <> rRep)
        go (l' + 1) sub' req' rep' true (out <> kvs)
      _ -> do
        -- 2. if we did not manage to make a single `req` parse, we ought to try
        --    if any of our args in `rep` is now able to parse. We consume at
        --    most one and re-run in case of success. The reason for re-running
        --    is, is that a success w/o substitution *necessarily*
        --    means we consumed input - hence we must try previously failed
        --    `req` again.
        --    note: the insertion of `res` back into `req` *does matter*, needs
        --          more thought though, as to put it into the front or back.
        -- trace l' \i-> "solve: trying via rep, i = " <> pretty i
        mKv' /\ rep' /\ _ /\ mNewRep' <-
          if canRep
            then do
              (_lmap Just <$> (try $ match (l' + 1) false rep)) <|>
                 pure (Nothing /\ rep /\ false /\ Nothing)
            else if options.repeatableOptions
              then
                let repOpts = flip filter rep case _ of
                                (ParseElem _ _ x) -> _isRepeatable x
                                _ -> false
                 in (_lmap Just <$> (try $ match (l' + 1) false repOpts)) <|>
                      pure (Nothing /\ rep /\ false /\ Nothing)
              else pure (Nothing /\ rep /\ false /\ Nothing)
        case mKv' of
          Just kvs@(_:_) -> do
            -- trace l' \i-> "solve: matched via rep: " <> pretty kvs <> ", i = " <> pretty i
            let rRep = _toElem <$> filter (_isRepeatable) (fst <$> kvs)
                rep'' = nub ((maybe Nil singleton mNewRep') <> rep' <> rRep)
            if any (isFrom Origin.Argv <<< snd) kvs
              then go (l' + 1) sub' req rep'' true (out <> kvs)
              else go (l' + 1) sub' req rep'' false (out <> kvs)
          -- 3. If we still did not manage to make a match, things aren't
          --    looking too great. It's time for drastic measures. If the parent
          --    is allowing us to, we repeat this above, but allow substitutions
          --    this time. At this point, "anything" goes. If we find a match
          --    whose result contains any value with origin `Argv`, we must
          --    again try again all prior inputs as new input means new chances
          --    of a successful match.
          _ -> do
            case req' of
              Nil -> do
                -- trace l' \i -> "solve: empty req' after rep. done. i = " <> pretty i
                pure out
              _:_ | sub ->
                let
                  exhaust
                    :: List ArgParseLayout
                    -> List KeyValue
                    -> ArgParser r (List KeyValue)
                  exhaust Nil out' = pure out'
                  exhaust req'' out' = do
                    kVs'' /\ req''' /\ changed /\ _ <- try $ match (l' + 1) true req''
                    -- trace l' \i ->
                    --      "solve: matched via sub"
                    --   <> " kVs''" <> pretty kVs''
                    --   <> ", changed = " <> show changed
                    --   <> ", req''' = " <> pretty req'''
                    --   <> ", i = " <> pretty i
                    -- check if we consumed input. If we did, we must rinse
                    -- and repeat the entire process with the new input.
                    if (not (null kVs'')) && (any (isFrom Origin.Argv <<< snd) kVs'' || changed)
                      then go (l' + 1) false req''' rep true (out' <> kVs'')
                      else exhaust req''' (out' <> kVs'')
                 in do
                  -- trace l' \i-> "solve: trying via sub, i = " <> pretty i
                  exhaust req' out
              xs -> do
                -- trace l' \_-> "solve: failed to match: " <> pretty xs
                fail "..." -- XXX: throw proper error here

  _lmap f (a /\ b /\ c /\ d) = f a /\ b /\ c /\ d

  _toElem :: Arg -> ArgParseLayout
  _toElem x =
    -- note: we know it's not fixed since we only use this function
    -- to re-inject options for `opts.repeatableOptions`.
    let isFree = true
     in ParseElem (Arg.getId x) isFree x

  _isRepeatable :: Arg -> Boolean
  _isRepeatable x = Arg.isArgRepeatable x || (repOpts && Arg.isOption x)

  {-
    Try to make a match from any of the input layouts.
    This parser deals with subsituting values, culling optional args and ensures
    order is maintained in lax-placement mode.

    Value substitution takes place after exhausting all other options have been
    exhausted, it's an effort to make anything match to consume input and
    (possible) layouts. Substitutions are a frickle beast, since it's hard to
    determine *when* to substitute. How do we know that if we substitute now, we
    won't make a match later?

    For example:

    usage: -a [-b -c] -d
    $ -b -a -d

    Here, consumption w/o substitutions is a dead end, since `[-b -c]` won't be
    able to match (it requires either `-b -c` or `-c -b`). Hence, we *must*
    use subsitutions to yield a match. But which argument should be substituted?
    We select the most eligble argument by see
  -}
  match l sub xs = cachedMatch (getId <$> xs) sub $ match' l sub xs
  match'
    :: Int -- the recursive level
    -> Boolean -- allow substitutions?
    -> List ArgParseLayout
    -> ArgParser r (Tuple (Tuple (Tuple
        (List KeyValue)         -- the key-value pairs that where yielded
        (List ArgParseLayout))  -- the layouts that did *NOT* match
        Boolean)                -- has this changed the input?
        (Maybe ArgParseLayout)) -- element to repeat
  match' l sub xs = go' Nothing false xs Nil Nil
    where
    go' errs locked (x:xs) ys matched = (do
      -- trace l \i-> "match: try"
      --   <> " x = " <> pretty x
      --   <> ", xs = " <> pretty xs
      --   <> ", ys = " <> pretty ys
      --   <> ", locked = " <> show locked
      --   <> ", sub = " <> show sub
      --   <> ", i = " <> pretty i
      if _isFixed x && locked
        then go' errs true xs (x:ys) matched
        else do
          let sub' = sub && not locked
          cvs <- fork $ parseLayout (l + 1) sub' x
          -- trace l \i-> "match: return"
          --   <> " x = " <> pretty x
          --   <> ", xs = " <> pretty xs
          --   <> ", ys = " <> pretty ys
          --   <> ", vs = " <> pretty (snd cvs)
          --   <> ", locked = " <> show locked
          --   <> ", sub = " <> show sub
          --   <> ", i = " <> pretty i
          go' errs (locked || _isFixed x) xs ys ((x /\ cvs) : matched)
    ) `catch` \{ depth } e -> do
      let errs' = case errs of
                    Just (d /\ _) | depth > d -> Just (depth /\ e)
                    Nothing -> Just (depth /\ e)
                    x -> x
      go' errs' (locked || _isFixed x) xs (x:ys) matched

    {-
      Evaluate the result. We've tried all `req` items agains the same input
      now and none of them managed to make a match. Since all of these items
      are "free", substituting or removing one of them is not going to make a
      difference for the others.
    -}
    go' errs locked Nil ys Nil = do
      i <- getInput

      -- re-arrange the pattern into it's original order.
      let ys' = sortBy (compare `on` getId) ys

      -- drop optional, fixed layouts.
      ys'' /\ changed <- pure do
       if locked
        then dropFirst (\x -> _isOptionalGroup x && _isFixed x) ys'
        else ys' /\ false

      -- trace l \i' -> "match: eval"
      --     <> " ys = " <> pretty ys
      --     <> ", ys' = " <> pretty ys'
      --     <> ", ys'' = " <> pretty ys''
      --     <> ", locked = " <> show locked
      --     <> ", sub = " <> show sub
      --     <> ", i = " <> pretty i'

      case if sub then filter (not <<< _isOptionalGroup) ys'' else ys'' of
        Nil -> do
          -- trace l \i' -> "match: succeeded!, i = " <> pretty i'

          -- substitute all leaf elements. we ignore groups because these groups
          -- have failed to parse irrespective of substitution, so they are a
          -- not considered a legible match.
          let subVs = catMaybes $ ys'' <#> case _ of
                        e@(ParseElem _ _ x) -> do
                          v <- Arg.getFallback x
                          pure (e /\ x /\ v)
                        _ -> Nothing

          -- return as a triplet, the values (only fallbacks), the layouts
          -- that where responsible for the values and finally if either have
          -- been locked (which indicates a possible change in input) or if
          -- we noticed a change during `dropFirst` (also releated to locking /
          -- positionals)
          pure ((_rest <$> subVs) /\ (_fst <$> subVs) /\ (locked || changed) /\ Nothing)

        zs | changed -> go' errs false zs Nil Nil

        z:zs -> do
          -- trace l \i' -> "match: failed!" <> pretty (z:zs) <> ", i = " <> pretty i'
          i <- getInput
          { depth } <- getState
          case errs of
            Just (d /\ e) | d > depth -> do
              setErrorAtDepth d (extractError genericError e)
              throw e
            _ -> case i of
              Nil ->
                let c = toSimpleBranch (z:zs)
                    e = missingArgumentsError (unsafePartial $ NonEmpty.fromList' c)
                in setErrorAtDepth depth e *> fail' e
              _ -> do
                kToks <- for i \(pTok@(PositionedToken tok _ _)) -> do
                  isKnown <- isKnownToken' tok
                  pure if isKnown
                    then known pTok
                    else unknown pTok
                let e = unexpectedInputError (toSimpleBranch zs) kToks
                setErrorAtDepth depth e *> fail' e

    {-
      Evaluate the matches.
      Each match is a triplet of (arg, continuation, values). We choose the best
      ranking triplet, inject it's parse state ("resume" the parse), yield it's
      values and return all losing arguments as non-matched.
    -}
    go' _ locked Nil ys (matches@(_:_)) =
      let
          -- select the best-ranking match based on the values it yielded
          matches' = sortBy (compare `on` (byOrigin <<< snd <<< snd)) matches
          x = fst $ unsafePartial (LU.last matches')
          cvs = snd $ unsafePartial (LU.last matches')
          vs = snd cvs
          xs' = fst <$> unsafePartial (LU.init matches')
          rep = if _isLayoutRepeatable x then Just x else Nothing
       in do
        -- resume the parser state with the continuation and the yielded value
        resume cvs

        -- re-sort the remaining elements (XXX: could this be skipped?)
        pure (vs /\ (sortBy (compare `on` getId) $ xs' <> ys) /\ locked /\ rep)

  dropFirst f xs = go' xs Nil
    where go' Nil out = out /\ false
          go' (x:xs) out = if f x then (xs <> out) /\ true else go' xs (x:out)

  _fst  (a /\ b /\ c) = a
  _rest (a /\ b /\ c) = b /\ c

  _isFixed :: ArgParseLayout -> Boolean
  _isFixed (ParseGroup _ f _ _ _) = not f
  _isFixed (ParseElem _ f _) = not f

  _hasFallback :: ArgParseLayout -> Boolean
  _hasFallback (ParseElem _ _ x) = isJust (Arg.getFallback x)
  _hasFallback _ = false

  _isOptionalGroup :: ArgParseLayout -> Boolean
  _isOptionalGroup (ParseGroup _ _ o _ _) = o
  _isOptionalGroup _ = false

  _isLayoutRepeatable :: ArgParseLayout -> Boolean
  _isLayoutRepeatable (ParseElem _ _ x) = Arg.isArgRepeatable x
  _isLayoutRepeatable (ParseGroup _ _ _ r _) = r


byOrigin :: List KeyValue -> Int
byOrigin vs = sum $ (Origin.weight <<< _.origin <<< unRichValue <<< snd) <$> vs

isFrom :: Origin -> RichValue -> Boolean
isFrom o rv = o == RichValue.getOrigin rv

parseLayout
  :: ∀ r
   . Int
  -> Boolean
  -> ArgParseLayout
  -> ArgParser r (List KeyValue)
parseLayout l sub x = do
 skipIf hasTerminated Nil do
  { options } <- getConfig
  go options x
  where
   -- Terminate at singleton groups that house only positionals.
  go options x | options.optionsFirst && isJust (termAs x)
    = let y = unsafePartial (fromJust (termAs x))
      in singleton <<< Tuple y <<< (RichValue.from Origin.Argv) <$>
        terminate (Arg.getArg y)

  go opts (g@(ParseGroup _ _ _ r branches)) =
    let parsers = NonEmpty.toList branches <#> \branch ->
                    let args = NonEmpty.toList branch
                     in parseBranch l true args
        p = evalParsers byOrigin parsers
     in do
      vs <- p
      if r && any (isFrom Origin.Argv <<< snd) vs
        then loop p vs
        else pure vs

     where
     loop p acc = do
        vs <- p <|> pure Nil
        if any (isFrom Origin.Argv <<< snd) vs
          then loop p (acc <> vs)
          else pure (acc <> vs)

  go opts (ParseElem _ _ x) =
    let arg = Arg.getArg x
        fromArgv = do
          RichValue.from Origin.Argv <$> do
            cachedArg (Arg.getId x) $ parseArg arg
          <* modifyDepth (_ + 1)
     in do
      if sub
        then singleton <<< Tuple x <$> case Arg.getFallback x of
              Just v -> fromArgv <|> pure v
              _      -> fromArgv
        else
          let nTimes = if Arg.isArgRepeatable x then some else liftM1 singleton
           in nTimes $ Tuple x <$> fromArgv

{-
  Parse a single argument. We do not substitute and ignore repetitions.
-}
parseArg
  :: ∀ r
   . SolvedLayoutArg
  -> ArgParser r Value
parseArg x = go x
  where
  go (Solved.Positional n _) = positional n n
  go (Solved.Command    n _) = command    n n
  go (Solved.Stdin         ) = stdin
  go (Solved.EOA           ) = eoa <|> (pure $ ArrayValue [])
  go (Solved.Option a  mA r) = do
    input       <- getInput
    { options } <- getConfig
    case input of
      (PositionedToken token _ _) : _
        | case token of
            LOpt _ _   -> true
            SOpt _ _ _ -> true
            _          -> false
        -> do
          aliases /\ def /\ env <- do
            description <- lookupDescription' a
            case description of
              (OptionDescription aliases' _ _ def' env') ->
                pure $ aliases' /\ def' /\ env'
              _ -> fail' $ internalError "invalid option description"
          let
            ns = NonEmpty.toList $ aliases <#> case _ of
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

{-
  Convert an ordinary layout to a layout suitable for parsing. Each layout and
  element get their own unique id in a shared namespace. For example:
    `G0(E1, G2(E3))`
      where `G` denotes a Group and `E` denotes an element.
  Further, we determine up-front which layout is "free" and which one is "fixed",
  saving us unncessary traversals during the hot phase of the parse.
-}
toParseBranch
  :: List ArgLayout
  -> List ArgParseLayout
toParseBranch xs = evalState (for xs go) 0
  where
  nextId = State.get <* State.modify (_ + 1)
  go (e@(Elem x)) = do
    i <- nextId
    pure (ParseElem i (Arg.isFreeLayout e) (Arg.setId i x))
  go (g@(Group o r xs)) = do
    i <- nextId
    ParseGroup i (Arg.isFreeLayout g) o r <$> do
      for xs (traverse go)

toSimpleBranch
  :: List ArgParseLayout
  -> List SolvedLayout
toSimpleBranch Nil = Nil
toSimpleBranch (x:xs) = go x : toSimpleBranch xs
  where
  go (ParseElem _ _ x) = Elem $ Arg.getArg x
  go (ParseGroup _ _ o r xs) = Group o r $ (go <$> _) <$> xs

{-
  Convert an ordinary branch of solved arguments into a branch of `Arg`s.
  We perform this step to cache some information right along-side each argument,
  such as it's `ArgKey` and fallback value. This is going to save us from doing
  this during the hot phase of the parse.

  note: we set all `id`s to 0 and copy them later from the containing layout's
        `id`s. this allows us to cache on the arg and layout level, and to
        associate across the two.
-}
toArgBranch
  :: ∀ r
   . Options r
  -> Env
  -> List Description
  -> Branch SolvedLayoutArg
  -> Branch Arg
toArgBranch options env descriptions x = go <$> x
  where
  go (Elem x) =
    let description = case x of
          (Option alias _ _) -> findDescription alias descriptions
          _                  -> Nothing
        fallback = do
          v <- unRichValue <$> getFallbackValue options env description x
          pure $ RichValue v {
            value = if Solved.isElemRepeatable x
                      then ArrayValue $ Value.intoArray v.value
                      else v.value
          }
     in Elem $ Arg 0 x (toArgKey x) fallback false
  -- note: uncomment to collapse [-a] into an optional -a
  -- go (Group o r (((e@(Elem _)):|Nil):|Nil))
  --   = let e' = go e
  --      in unsafePartial $ case e' of
  --         Elem (Arg i x k mV o') ->
  --           Elem $ Arg i (Solved.setElemRepeatable r x) k mV (o || o')
  go (Group o r xs) = Group o r $ (go <$> _) <$> xs

-- Check if a given layout qualifies as a terminating argument for options-first
-- and if so, return the argument it should be associated with.
termAs :: ArgParseLayout -> Maybe Arg
termAs x = go x
  where
  go (ParseGroup _ _ _ gR (((ParseElem _ _ x@(Arg _ (Solved.Positional _ pR) _ _ _)) :| Nil) :| Nil)) | gR || pR = Just x
  go (ParseElem _ _ x@(Arg _ (Solved.Positional _ r) _ _ _)) | r = Just x
  go _ = Nothing

{-
  Cached lookup if a token is known or not
-}
isKnownToken'
  :: ∀ r
   . Token
  -> ArgParser r Boolean
isKnownToken' tok = do
  { spec } <- getConfig
  { isKnownCache } <- getGlobalState
  case Map.lookup tok isKnownCache of
    Just b -> pure b
    Nothing ->
      let isKnown = isKnownToken spec tok
       in isKnown <$ modifyGlobalState \s -> s {
            isKnownCache = Map.alter (const (Just isKnown)) tok s.isKnownCache
          }

{-
  Determine if a given token is considered known
-}
isKnownToken
  :: Spec SolvedLayout
  -> Token
  -> Boolean
isKnownToken (Spec { layouts, descriptions }) tok = occuresInDescs || occuresInLayouts
  where
  occuresInDescs = any matchesDesc descriptions
    where
    matchesDesc (OptionDescription as _ _ _ _) = test tok
      where
      test (Token.LOpt n _)   = elem (OptionAlias.Long n) as
      test (Token.SOpt s _ _) = elem (OptionAlias.Short s) as
      test _ = false
    matchesDesc _ = false
  occuresInLayouts = any (any (any matchesLayout)) layouts
    where
    matchesLayout (Group _ _ xs) = any (any matchesLayout) xs
    matchesLayout (Elem x) = test tok x
      where
      test (Token.LOpt n _)   (Solved.Option a _ _) = OptionAlias.Long n == a
      test (Token.SOpt s _ _) (Solved.Option a _ _) = OptionAlias.Short s == a
      test (Token.Lit n)      (Solved.Command n' _) = n == n'
      test (Token.EOA _)      (Solved.EOA)          = true
      test (Token.Stdin)      (Solved.Stdin)        = true
      test _ _ = false

{-
  Pre-emptively determine if a branch can match the empty input.
-}
canMatchEmptyInput
  :: NonEmpty List ArgLayout
  -> Boolean
canMatchEmptyInput xs = any go xs
  where go (Group o _ xs) = if o then true
                                 else all canMatchEmptyInput xs
        go (Elem x) = isJust $ getFallback x

