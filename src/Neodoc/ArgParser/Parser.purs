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

type ChunkedLayout a = Layout (Chunk a)

initialState :: ParseState
initialState = {
  depth: 0
, hasTerminated: false
, trackedOpts: Set.empty
}

initialGlobalState :: GlobalParseState
initialGlobalState = {
  deepestError: Nothing
}

{-
A recursive layout structure suited for parsing.

Each nested grouping indicates if it is ought to be considered "fixed", or
"free" in terms of the groups' occurence rules. The rules are simple:
Groups that contain any branch that contains any positional argument are now
"fixed". This is restriction is set in place in order to allow re-evaluating
the group in face of repetition:

"u: (-a | -b)..." says that the group *in it's entirety* should be able to occur
one or more times. This means that all of these inputs are valid: "-ab", "-bb",
"-abab", "-abbbab".

An alternative contemplation was to "lock" the branch after the initial match.
This, in effect, alters the spec "on-the-fly" for this pattern's evaluation.
This would cause "u: (-a | b)..." to only match inputs: "-a", "-aa", ..., "-b",
"-bb" and so on, mixing "a"s and "b"s would be considered a failure. Neodoc
does not yet implement such a pattern because it is less general. The above
could also be achieved, more flexibly, using "u: (-a...|-b...)".

Secondly, this data structure presents and "id", suitable for quick cache
lookups and identification of elements later on.
-}
type ParseBranch a = NonEmpty List (ParseLayout a)
data ParseLayout a
  = ParseElem
      Int     -- id
      Boolean -- free?
      a
  | ParseGroup
      Int     -- id
      Boolean -- free?
      Boolean -- optional?
      Boolean -- repeatable?
      (NonEmpty List (ParseBranch a))
type ArgParseLayout = ParseLayout Arg

getElem :: ∀ a. ParseLayout a -> Maybe a
getElem (ParseElem _ _ x) = Just x
getElem _ = Nothing

getId :: ∀ a. ParseLayout a -> Int
getId (ParseElem id _ _) = id
getId (ParseGroup id _ _ _ _) = id

derive instance genericParseLayout :: (Generic a) => Generic (ParseLayout a)
instance showParseLayout :: (Generic a, Show a) => Show (ParseLayout a) where
  show = gShow

instance eqParseLayout :: Eq (ParseLayout a) where
  eq = eq `on` getId

instance prettyParseLayout :: (Pretty a) => Pretty (ParseLayout a) where
  pretty (ParseElem id free x)
    = "E(" <> show id <> ":"
        <> (if free then "<*" else "<!")
        <> pretty x
        <> (if free then "*>" else "!>")
        <> ")"
  pretty (ParseGroup id free o r xs)
    = "G"
        <> (if o then "[" else "(")
        <> show id <> ":"
        <> (if free then "<*" else "<!")
        <> (intercalate "|" (pretty <$> xs))
        <> (if free then "*>" else "!>")
        <> (if o then "]" else ")")
        <> (if r then "..." else "")

parse
  :: ∀ r
   . Spec SolvedLayout
  -> Options r
  -> Env
  -> List PositionedToken
  -> Either (ParseError ArgParseError) ArgParseResult
parse (Spec { layouts, descriptions }) options env tokens = do
  runParser { env, options, descriptions } initialState initialGlobalState tokens $
    let hasEmpty = any null layouts
        toplevels = concat (NonEmpty.toList layouts)

        parsers :: List (ArgParser r ArgParseResult)
        parsers = toplevels <#> \branch ->
          let branch' = toArgBranch options env descriptions branch
              branch'' = toParseBranch (NonEmpty.toList branch')
           in do
            vs <- parseBranch 0 true branch''
            eof
            pure $ ArgParseResult (Just branch) vs

        parsers' :: List (ArgParser r ArgParseResult)
        parsers' = parsers
            -- if there were empty any layouts, such as for example:
            --    usage: prog
            --       or: prog
            -- then we consolidate those into a single, artificial
            -- parse whose only requirement is that there be no input.
            <> if hasEmpty
                  then singleton $ eof $> ArgParseResult Nothing Nil
                  else Nil
     in if null parsers
          then eof $> ArgParseResult Nothing Nil
          else evalParsers (byOrigin <<< getResult) parsers
  where
  eof :: ∀ r. ArgParser r Unit
  eof = do
    input <- getInput
    case input of
      Nil  -> pure unit
      toks -> do
        { deepestError } <- getGlobalState
        case deepestError of
          Just (_ /\ e) -> fail' e
          Nothing -> fail $ "expected EOF, got: " <> pretty toks

parseBranch
  :: ∀ r
   . Int
  -> Boolean -- allow substitutions?
  -> List ArgParseLayout
  -> ArgParser r (List KeyValue)
parseBranch _ _ Nil = pure Nil
parseBranch l sub xs = profile "parse-branch" \_-> do
  { options } <- getConfig
  solve l options.repeatableOptions sub xs

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
solve l repOpts sub req = go l sub req Nil Nil Nil
  where
  go
    :: Int                 -- the recursive level
    -> Boolean             -- allow substitutions?
    -> List ArgParseLayout -- the required arguments
    -> List ArgParseLayout -- repeatable arguments
    -> List ArgParseLayout -- the "reserve"
    -> List KeyValue       -- the output
    -> ArgParser r (List KeyValue)
  go l' sub' req rep res out = do
    input       <- getInput
    { options } <- getConfig
    { depth }   <- getState
    trace l' \_->
           "req = " <> pretty req
      <> ", rep = " <> pretty rep
      <> ", res = " <> pretty res
      <> ", sub = " <> show sub
      <> ", in = "  <> pretty input
      <> ", out = " <> pretty out

    -- 1. try making a match for any arg in `req` w/o allowing substitutions.
    --    if we make a match, we proceed *and never look back*. If we do not
    --    succeed with any argument, we try the `rep` list.
    trace l' \_-> "trying via argv"
    mKv /\ req' /\ _ <- (_lmap Just <$> (try $ match l' false req))
                    <|> pure (Nothing /\ req /\ false)
    trace l' \_-> "req' = " <> pretty req' <> " out = " <> show (pretty <$> mKv)
    case mKv of
      Just kvs@(_:_) -> do
        trace l' \_-> "matched on argv: " <> pretty kvs
        let rRep = _toElem <$> filter (_isRepeatable) (fst <$> kvs)
            rep' = nub (rep <> rRep)
        go (l' + 1) sub' (req' <> res) rep' Nil (out <> kvs)
      _ -> do
        -- 2. if we did not manage to make a single `req` parse, we ought to try
        --    if any of our args in `rep` is now able to parse. We consume at
        --    most one and flush `res` in case of success. The reason for
        --    flushing is, is that a success w/o substitution *necessarily*
        --    means we consumed input - hence we must try previously failed
        --    `req` again.
        --    note: the insertion of `res` back into `req` *does matter*, needs
        --          more thought though, as to put it into the front or back.
        trace l' \_-> "trying via rep"
        mKv' /\ rep' /\ _ <- (_lmap Just <$> (try $ match l' false rep))
                          <|> pure (Nothing /\ rep /\ false)
        case mKv' of
          Just kvs@(_:_) -> do
            trace l' \_-> "matched via rep: " <> pretty kvs
            let rRep = _toElem <$> filter (_isRepeatable) (fst <$> kvs)
                rep'' = nub (rep' <> rRep)
            go (l' + 1) sub' (req <> res) rep'' Nil (out <> kvs)
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
                trace l' \_-> "empty req' after rep. done"
                pure out
              _:_ | sub ->
                let
                  exhaust
                    :: List ArgParseLayout
                    -> List KeyValue
                    -> ArgParser r (List KeyValue)
                  exhaust Nil out' = pure out'
                  exhaust req'' out' = do
                    kVs'' /\ req''' /\ changed <- try $ match l' true req''
                    trace l' \_ ->
                         "matched via sub"
                      <> " kVs''" <> pretty kVs''
                      <> ", changed = " <> show changed
                      <> ", req''' = " <> pretty req'''
                    -- check if we consumed input. If we did, we must rinse
                    -- and repeat the entire process with the new input.
                    if (not (null kVs'')) && (any (isFrom Origin.Argv <<< snd) kVs'' || changed)
                      then go (l' + 1) false req''' rep res (out' <> kVs'')
                      else exhaust req''' (out' <> kVs'')
                 in do
                  trace l' \_-> "trying via sub"
                  exhaust req' out
              xs -> do
                trace l' \_-> "failed to match: " <> pretty xs
                fail "... (no sub)"

  _lmap f (a /\ b /\ c) = f a /\ b /\ c

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
  -}
  match
    :: Int -- the recursive level
    -> Boolean -- allow substitutions?
    -> List ArgParseLayout
    -> ArgParser r (Tuple (Tuple
        (List KeyValue)        -- the key-value pairs that where yielded
        (List ArgParseLayout)) -- the layouts that did *NOT* match
        Boolean)               -- has this changed the input?
  match l sub xs = go' false xs Nil
    where
    go' locked (x:xs) ys = (do
      trace l \_-> "match"
        <> " x = " <> pretty x
        <> ", xs = " <> pretty xs
        <> ", ys = " <> pretty ys
        <> ", locked = " <> show locked
        <> ", sub = " <> show sub
      if _isFixed x && locked
        then go' true xs (x:ys)
        else do
          vs <- try $ parseLayout (l + 1) (sub && not locked) x
          trace l \_-> "match made!"
            <> " x = " <> pretty x
            <> ", xs = " <> pretty xs
            <> ", ys = " <> pretty ys
            <> ", vs = " <> pretty vs
            <> ", locked = " <> show locked
            <> ", sub = " <> show sub
          pure (vs /\ (sortBy (compare `on` getId) $ xs <> ys) /\ locked)
    ) `catch` \_ e -> do
      go' (locked || _isFixed x) xs (x:ys)

    {-
      Evaluate the result. We've tried all `req` items agains the same input
      now and none of them managed to make a match. Since all of these items
      are "free", substituting or removing one of them is not going to make abs
      difference for the others.
    -}
    go' locked Nil ys = do
      i <- getInput
      let ys' = sortBy (compare `on` getId) ys
      ys'' /\ changed <- pure do
       if locked
        then dropFirst (\x -> _isOptional x && _isFixed x) ys'
        else ys' /\ false

      trace l \_-> "match eval"
          <> " ys = " <> pretty ys
          <> ", ys' = " <> pretty ys'
          <> ", ys'' = " <> pretty ys''
          <> ", locked = " <> show locked
          <> ", sub = " <> show sub

      case if sub then filter (not <<< _isOptional) ys'' else ys'' of
        Nil -> do
          trace l \_-> "match succeeded!"

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
          pure ((_rest <$> subVs) /\ (_fst <$> subVs) /\ (locked || changed))
        zs | changed -> go' false zs Nil
        zs -> do
          trace l \_-> "match failed!"
          fail $ "expected " <> pretty zs <> ", but got: " <>  pretty i

  dropFirst f xs = go' xs Nil
    where go' Nil out = out /\ false
          go' (x:xs) out = if f x then (out <> xs) /\ true else go' xs (x:out)

  _fst  (a /\ b /\ c) = a
  _rest (a /\ b /\ c) = b /\ c

  _isFixed :: ArgParseLayout -> Boolean
  _isFixed (ParseGroup _ f _ _ _) = not f
  _isFixed (ParseElem _ f _) = not f

  _hasFallback :: ArgParseLayout -> Boolean
  _hasFallback (ParseElem _ _ x) = isJust (Arg.getFallback x)
  _hasFallback _ = false

  _isOptional :: ArgParseLayout -> Boolean
  _isOptional (ParseGroup _ _ o _ _) = o
  _isOptional (ParseElem _ _ x) = Arg.isOptional x

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
  { options } <- getConfig
  go options x
  where
  go opts (g@(ParseGroup _ _ _ r branches)) =
    let parsers = NonEmpty.toList branches <#> \branch ->
                    let args = NonEmpty.toList branch
                     in parseBranch l sub args
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
          RichValue.from Origin.Argv <$> parseArg arg
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
parseArg = go
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
                vs <- fail "not implemented" -- TODO: terminate x
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