-- Reduce the neodoc output into a easily consume key => value map.
--
-- This is done by:
--      1. reducing the matched branch down to a set of arguments, merging them
--         as needed (lossy).
--      2. reducing the matched key-values down to a set of key -> [ value ]
--         (lossless)
--      3. applying values to the arguments in the branch processed in (1)
--         merging duplicate occurences as makes sense for that option
--      4. culling values considered empty

module Neodoc.Evaluate.Reduce (reduce) where

import Prelude
import Debug.Trace
import Data.Tuple (Tuple, fst, snd)
import Data.Pretty (pretty)
import Data.Tuple.Nested ((/\))
import Data.NonEmpty ((:|))
import Data.Bifunctor (rmap, lmap, bimap)
import Data.Map (Map)
import Data.Map as Map
import Data.StrMap as StrMap
import Data.StrMap (StrMap())
import Data.Maybe (Maybe(..), maybe, fromJust, fromMaybe)
import Data.List (
  List(..), concat, head, filter, singleton, reverse, nub, catMaybes
, fromFoldable, (:))
import Data.Array as A
import Data.NonEmpty (NonEmpty)
import Data.NonEmpty.Extra as NonEmpty
import Data.Foldable (any, foldl, maximum, all)
import Data.Traversable (for)
import Control.Alt ((<|>))
import Control.Monad.State (execState)
import Control.Monad.State as State
import Neodoc.Env (Env)
import Neodoc.Data.Layout (Layout(..), Branch, getElem)
import Neodoc.Data.Description (Description(..))
import Neodoc.Data.SolvedLayout (SolvedLayout, SolvedLayoutArg)
import Neodoc.Data.SolvedLayout as Solved
import Neodoc.Data.OptionArgument
import Neodoc.OptionAlias as OptionAlias
import Neodoc.Value.RichValue
import Neodoc.ArgParser.KeyValue (KeyValue)
import Neodoc.ArgParser.Arg
import Neodoc.ArgKey (ArgKey(..))
import Neodoc.ArgKey.Class (class ToArgKey, toArgKey)
import Neodoc.Value
import Neodoc.Value as Value
import Neodoc.Value.Origin as Origin
import Neodoc.Value.RichValue as RichValue
import Neodoc.Evaluate.Annotate
import Neodoc.Evaluate.Key
import Partial.Unsafe (unsafePartial)

type IsOptional = Boolean
type IsRepeatable = Boolean
type IsNegative = Boolean
type FacelessLayout = Layout FacelessLayoutArg
data FacelessLayoutArg
  = Command     Boolean
  | Positional  Boolean
  | Option      IsNegative (Maybe IsOptional) IsRepeatable
  | EOA
  | Stdin

reduce
  :: Env
  -> List Description
  -> Maybe (Branch SolvedLayoutArg)
  -> List KeyValue
  -> _ -- Map ArgKey RichValue
reduce _ _ Nothing _ = StrMap.empty
reduce env descriptions (Just branch) kvs = (_.value <<< unRichValue) <$>
  let -- 0. bloat up descriptions by expanding all possible aliases
      descriptions' = expandDescription <$> descriptions

      -- 1. annotate all layout elements with their description
      annotedBranch = annotateLayout descriptions' <$> branch

      -- 2. derive a set of arguments and their description for the matched
      --    branch. this removes all levels of nesting and is a lossy operation.
      --    it is essentially a target of values we are ought to fill from what
      --    the parser derived
      target = expandLayout (Group false false (annotedBranch :| Nil))

      -- 3. Collect the input map. This map reduces the matched values by their
      --    denominator. Currently, this denominator is the `Key` derived from
      --    the option's name as it was matched.
      input = collect descriptions' kvs

      -- 3. fill the values for each key of the target map
      values = fillValues target input

   in finalFold values

  where

  collect
    :: List Description
    -> List KeyValue
    -> Map Key (List RichValue)
  collect descriptions kvs =
    let kvs' = lmap getArgKey <$> kvs
        kvs'' = kvs' <#> \(k /\ v) ->
          (Key $ findArgKeys descriptions k) /\ k /\ v
     in go kvs'' Map.empty

    where
    go
      :: List (Tuple Key (Tuple ArgKey RichValue))
      -> Map Key (List RichValue)
      -> Map Key (List RichValue)
    go ((k /\ (OptionKey a) /\ v):kvs) m | OptionAlias.isNegative a =
      -- replace all prior occurences if a negative options is met
      go kvs (Map.insert k (pure v) m)
    go ((k /\ _ /\ v):kvs) m =
      -- otherwise, just accumulate
      go kvs (Map.alter (combine v) k m)
    go Nil m = m

    combine v (Just v') = Just (v' <> singleton v)
    combine v Nothing = Just (singleton v)

  mergeVals _ (a@(Option true _ _) /\ d /\ RichValue v)
    | v.origin == Origin.Argv
    = a /\ d /\ (RichValue v { value = BoolValue false })

  mergeVals (a /\ d /\ RichValue v) (_ /\ _ /\ RichValue v') = a /\ d /\ (RichValue $ {
    origin: unsafePartial $ fromJust $ maximum [ v.origin, v'.origin ]
  , value:  ArrayValue $ Value.intoArray v'.value
                      <> Value.intoArray v.value
  })

  fillValues
    :: Map Key (WithDescription FacelessLayoutArg)
    -> Map Key (List RichValue)
    -> Map Key _
  fillValues target input =
    let
      -- 1. look up the values. Note that the lookup may yield `Nothing`,
      --    meaning that it is ought to be omitted.
      values = Map.toList target <#> \(k /\ (a /\ d)) -> do
        vs <- Map.lookup k input
        let vs' = filter (origin (/=) Origin.Empty) vs
            vs'' = filter (origin (/=) Origin.Default) vs'
            vs''' = case vs'' of
                        Nil -> nub vs'
                        vs  -> vs
            vs'''' = vs''' <#> \(RichValue v) -> RichValue $ v {
                      value = if isRepeatable a
                                then ArrayValue $ Value.intoArray v.value
                                else v.value
                      }
        -- return: k => arg , description , value
        pure $ vs'''' <#> \v -> k /\ (a /\ d /\ v)
     in Map.fromFoldableWith mergeVals $ concat $ catMaybes $ values

    where
    origin cmp o = \x -> (_.origin $ unRichValue x) `cmp` o

  finalFold :: Map Key _ -> StrMap RichValue
  finalFold m =
    let x = Map.toList m <#> \(k /\ (a /\ _ /\ RichValue rv)) ->
              let v = fromMaybe rv.value do
                    if isFlag a || isCommand a
                      then case rv.value of
                        ArrayValue xs -> pure
                          if all isBoolValue xs && not (A.null xs)
                            then
                              IntValue $ A.length $ flip A.filter xs \x ->
                                  case x of
                                      BoolValue b -> b
                                      _           -> false
                            else ArrayValue xs
                        BoolValue b ->
                          if isRepeatable a
                              then Just $ IntValue if b then 1 else 0
                              else Nothing
                        _ -> Nothing
                      else Nothing
                in toStrKeys k <#> (_ /\ (RichValue $ rv { value = v }))
    in StrMap.fromFoldable $ concat x

-- Reduce a solved layout arg to a faceless layout arg, that is a layout arg
-- whose identifying properties have been stripped since they are already
-- present in that object's key.
toFacelessLayoutArg
  :: SolvedLayoutArg
  -> FacelessLayoutArg
toFacelessLayoutArg = go
  where
  go (Solved.Option a mA  r) = Option (OptionAlias.isNegative a)
                                      (isOptionArgumentOptional <$> mA)
                                      r
  go (Solved.Positional _ r) = Positional r
  go (Solved.Command    _ r) = Command r
  go Solved.EOA              = EOA
  go Solved.Stdin            = Stdin

-- Expand a layout into a map of `Key => Argument`, where `Key` must uniquely
-- identify the argument in order to avoid loss.
expandLayout
  :: AnnotatedLayout SolvedLayoutArg
  -> Map Key (WithDescription FacelessLayoutArg)
expandLayout (Elem x) = Map.singleton (toKey x) (lmap toFacelessLayoutArg x)
expandLayout (Group o r xs) =
  let -- 1. expand each branch, reducing each to a `Map`
      branches = fold inSameBranch <<< (expandLayout <$> _) <$> xs
      -- 2. apply this group's repeatablity to all elements across all branches
      branches' = (lmap (setRepeatableOr r) <$> _) <$> branches
   in -- 3. reduce all branches into a single branch
      fold acrossBranches branches'

  where

  inSameBranch   = mergeArgs true
  acrossBranches = mergeArgs false

  mergeArgs
    :: Boolean
    -> WithDescription FacelessLayoutArg
    -> WithDescription FacelessLayoutArg
    -> WithDescription FacelessLayoutArg

  -- note: two options identified by the same key clashed.
  --       we can only keep one, so we combine them as best possible.
  --
  --       we apply the first round of value omission here, if the option to the
  --       right is marked as "negative". In such a case, the result should
  --       simply become "false"
  --
  --       we simply choose the left option's name since the name won't matter
  --       too much as long as it resolves to the same description which is
  --       implicitely true due to the encapsulating `Key`. The same applies
  --       for the option's option-argument.
  -- mergeargs forcer _ ((option true nothing r) /\ mdesc)
  --   = option true nothing r /\ mdesc
  mergeArgs forceR (x@((Option neg mA r) /\ mDesc)) ((Option _ mA' r') /\ mDesc')
    = let
        -- find a median for the option-argument
        mA'' = do
          aO  <- mA  <|> mA'
          aO' <- mA' <|> mA
          pure $ aO || aO'

        -- find a median for the option description
        mDesc'' = do
          desc  <- mDesc  <|> mDesc'
          desc' <- mDesc' <|> mDesc
          case desc /\ desc' of
            (OptionDescription a b c d e) /\ (OptionDescription a' b' c' d' e') ->
              pure $ OptionDescription a b (c <|> c') (d <|> d') (e <|> e')
            o@(OptionDescription _ _ _ _ _) /\ _ -> pure o
            _ /\ o@(OptionDescription _ _ _ _ _) -> pure o
            _ -> Nothing
      in Option neg mA'' (forceR || r || r') /\  mDesc''
  mergeArgs forceR (x /\ mDesc) (y /\ _)
    = setRepeatableOr (forceR || isRepeatable y) x /\ mDesc

  fold f = foldl (Map.unionWith f) Map.empty

isRepeatable :: FacelessLayoutArg -> Boolean
isRepeatable (Command    r) = r
isRepeatable (Positional r) = r
isRepeatable (Option _ _ r) = r
isRepeatable _ = false

setRepeatable :: Boolean -> FacelessLayoutArg -> FacelessLayoutArg
setRepeatable r (Command    _) = Command    r
setRepeatable r (Positional _) = Positional r
setRepeatable r (Option y x _) = Option y x r
setRepeatable _ x = x

setRepeatableOr :: Boolean -> FacelessLayoutArg -> FacelessLayoutArg
setRepeatableOr r x = setRepeatable (isRepeatable x || r) x

isFlag :: FacelessLayoutArg -> Boolean
isFlag (Option _ Nothing _) = true
isFlag _ = false

isCommand :: FacelessLayoutArg -> Boolean
isCommand (Command _) = true
isCommand _ = false

expandDescription :: Description -> Description
expandDescription (OptionDescription as r mA mD mE)
  | maybe true isOptionArgumentOptional mA -- no opt-arg or optional
  =
    let as' = NonEmpty.concat $ as <#> \a ->
                OptionAlias.setNegative true a :|
                  OptionAlias.setNegative false a :
                    Nil
     in OptionDescription as' r mA mD mE
expandDescription d = d
