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

module Neodoc.Evaluate.Reduce (reduce, expandLayout) where

import Prelude
import Data.Tuple (Tuple, fst, snd)
import Data.Tuple.Nested ((/\))
import Data.NonEmpty ((:|))
import Data.Bifunctor (lmap)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust)
import Data.List (List(..), head, filter, singleton)
import Data.NonEmpty (NonEmpty)
import Data.Foldable (any, foldl, maximum, all)
import Control.Alt ((<|>))
import Neodoc.Env (Env)
import Neodoc.Data.Layout (Layout(..), Branch, getElem)
import Neodoc.Data.Description (Description(..))
import Neodoc.Data.SolvedLayout (SolvedLayout, SolvedLayoutArg)
import Neodoc.Data.SolvedLayout as Solved
import Neodoc.Data.OptionArgument
import Neodoc.Value.RichValue
import Neodoc.ArgParser.KeyValue (KeyValue)
import Neodoc.ArgKey (ArgKey(..))
import Neodoc.ArgKey.Class (class ToArgKey, toArgKey)
import Neodoc.Value
import Neodoc.Value as Value
import Neodoc.Value.RichValue as RichValue
import Neodoc.Evaluate.Annotate
import Neodoc.Evaluate.Key
import Partial.Unsafe (unsafePartial)

reduce
  :: Env
  -> List Description
  -> Branch SolvedLayoutArg
  -> List KeyValue
  -> Map ArgKey RichValue
reduce env descriptions branch vs =
  let -- 1. annotate all layout elements with their description
      annotedBranch = annotate descriptions <$> branch

      -- 2. derive a set of arguments and their description for the matched
      --    branch. this removes all levels of nesting and is a lossy operation.
      --    it is essentially a target of values we are ought to fill from what
      --    the parser derived
      target = expandLayout (Group false false (annotedBranch :| Nil))

   in Map.empty

  where

  mergeVals :: RichValue -> RichValue -> RichValue
  mergeVals (RichValue v) (RichValue v') = RichValue $ {
    origin: unsafePartial $ fromJust $ maximum [ v.origin, v'.origin ]
  , value:  ArrayValue $ Value.intoArray v'.value
                      <> Value.intoArray v.value
  }


-- Expand a layout into a map of `Key => Argument`, where `Key` must uniquely
-- identify the argument in order to avoid loss.
expandLayout
  :: AnnotatedLayout (SolvedLayoutArg)
  -> Map Key (WithDescription SolvedLayoutArg)
expandLayout (Elem x) = Map.singleton (toKey x) x
expandLayout (Group o r xs) =
  let -- 1. expand each branch, reducing each to a `Map`
      branches = fold inSameBranch <<< (expandLayout <$> _) <$> xs
      -- 2. apply this group's repeatablity to all elements across all branches
      branches' = (lmap (_setRepeatableOr r) <$> _) <$> branches
   in -- 3. reduce all branches into a single branch
      fold acrossBranches branches'

  where

  inSameBranch   = mergeArgs true
  acrossBranches = mergeArgs false

  mergeArgs
    :: Boolean
    -> WithDescription SolvedLayoutArg
    -> WithDescription SolvedLayoutArg
    -> WithDescription SolvedLayoutArg

  -- note: two options identified by the same key clashed.
  --       we can only keep one, so we combine them as best possible.
  -- note: we simply choose the left option's name since the name won't matter
  --       too much as long as it resolves to the same description which is
  --       implicitely true due to the encapsulating `Key`. The same applies
  --       for the option's option-argument.
  -- idea: maybe we need a more appropriate data structure to capture the
  --       semantics of this reduction?
  mergeArgs forceR (x@((Solved.Option a mA r) /\ mDesc)) ((Solved.Option _ mA' r') /\ mDesc')
    = let
        mA'' = do
          OptionArgument aN aO  <- mA  <|> mA'
          OptionArgument _  aO' <- mA' <|> mA
          pure $ OptionArgument aN (aO || aO')
        mDesc'' = do
          desc  <- mDesc  <|> mDesc'
          desc' <- mDesc' <|> mDesc
          case desc /\ desc' of
            (OptionDescription a b c d e) /\ (OptionDescription a' b' c' d' e') ->
              pure $ OptionDescription a b (mA'' <|> c <|> c') (d <|> d') (e <|> e')
            (OptionDescription a b c d e) /\ _ ->
              pure $ OptionDescription a b (mA'' <|> c) d e
            _ /\ (OptionDescription a b c d e) ->
              pure $ OptionDescription a b (mA'' <|> c) d e
            _ -> Nothing
      in Solved.Option a mA'' (forceR || r || r') /\  mDesc''
  mergeArgs forceR (x /\ mDesc) (y /\ _)
    = _setRepeatableOr (forceR || _isRepeatable y) x /\ mDesc

  _isRepeatable = Solved.isRepeatable <<< Elem

  _setRepeatable r x =
    let y = Solved.setRepeatable r (Elem x)
     in unsafePartial (getElem y)

  _setRepeatableOr r x =
    let y = setRepeatableOr r (Elem x)
     in unsafePartial (getElem y)

  fold f = foldl (Map.unionWith f) Map.empty

-- Set a layout's repeatablity using logical OR.
-- note: this could be moved to a more appropriate place
setRepeatableOr
  :: Boolean
  -> SolvedLayout
  -> SolvedLayout
setRepeatableOr r l =
  -- optimal: use an if/else for short-circuiting
  if r  then Solved.setRepeatable r l
        else Solved.setRepeatable (r || Solved.isRepeatable l) l
