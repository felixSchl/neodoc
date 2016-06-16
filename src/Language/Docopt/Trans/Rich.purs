module Language.Docopt.Trans.Rich (
    reduce
  ) where

import Prelude

import Debug.Trace
import Data.List (List(..), catMaybes, singleton, concat, toList, filter,
                  reverse, null, nub)
import Data.Array (length, filter) as A
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Maybe.Unsafe (fromJust)
import Data.Map as Map
import Data.Tuple (Tuple(Tuple))
import Data.Map (Map())
import Data.StrMap as StrMap
import Data.StrMap (StrMap())
import Data.Bifunctor (rmap, lmap)
import Data.Foldable (foldl, maximum, all)
import Control.Alt ((<|>))

import Language.Docopt.Value (Value(..), isBoolValue)
import Language.Docopt.Value as Value
import Language.Docopt.Usage (Usage(Usage), runUsage) as D
import Language.Docopt.Env (Env)
import Language.Docopt.Env as Env
import Language.Docopt.Argument (Argument(..), Branch(), isRepeatable,
                                setRepeatable, setRepeatableOr,
                                isCommand, isFlag) as D
import Language.Docopt.Compiler (ValueMapping)
import Language.Docopt.RichValue (RichValue(..), unRichValue)
import Language.Docopt.Origin as Origin
import Language.Docopt.Origin (Origin())
import Language.Docopt.Trans.Key (Key(..), key, toKeys)

reduce :: List D.Usage       -- ^ the program specification
       -> Env                -- ^ the environment
       -> D.Branch           -- ^ the matched specification
       -> List ValueMapping  -- ^ the parse result
       -> StrMap RichValue   -- ^ the output set of (arg => val)
reduce us env b vs =
  let vm = Map.fromFoldableWith (<>) (rmap singleton <$>
                                            lmap key <$>
                                            reverse vs)
      m = applyValues vm $ reduceUsage (D.Usage (singleton b))
   in finalFold m

  where

  mergeVals :: RichValue -> RichValue -> RichValue
  mergeVals (RichValue v) (RichValue v') = RichValue $ {
    origin: fromJust $ maximum [ v.origin, v'.origin ]
  , value:  ArrayValue $ Value.intoArray v'.value
                      <> Value.intoArray v.value
  }

  applyValues :: Map Key (List RichValue) -> List D.Argument -> Map Key RichValue
  applyValues vm as = Map.fromFoldableWith mergeVals
    $ concat
    $ catMaybes
    $ as <#> \a -> do
      vs <- Map.lookup (key a) vm
      let vs' = filter (origin (/=) Origin.Empty) vs
          vs'' = filter (origin (/=) Origin.Default) vs'
          vs''' = case vs'' of
                       Nil -> nub vs'
                       vs  -> vs
          vs'''' = vs''' <#> \(RichValue v) -> RichValue $ v {
                    value = if D.isRepeatable a
                              then ArrayValue $ Value.intoArray v.value
                              else v.value
                    }

      pure $ (Tuple (key a)) <$> vs''''

    where
    isRelevant a = Map.member (key a) vm
    origin cmp o = \x -> (_.origin $ unRichValue x) `cmp` o

  finalFold :: Map Key RichValue -> StrMap RichValue
  finalFold m =
    StrMap.fromFoldable $ concat
      $ Map.toList m
          <#> \(Tuple (Key { arg: a }) (RichValue rv)) ->
                  let v = fromMaybe rv.value do
                        if D.isFlag a || D.isCommand a
                          then case rv.value of
                            ArrayValue xs -> pure
                              if all isBoolValue xs && A.length xs > 0
                                then
                                  IntValue (A.length $ flip A.filter xs \x ->
                                      case x of
                                          BoolValue b -> b
                                          _           -> false
                                  )
                                else ArrayValue xs
                            BoolValue b ->
                              if D.isRepeatable a
                                  then
                                    pure if b
                                              then IntValue 1
                                              else IntValue 0
                                  else Nothing
                            _ -> Nothing
                          else Nothing
                    in flip Tuple (RichValue $ rv { value = v }) <$> do
                        toList $ toKeys a

-- | Reduce a usage application specification down to a list of unique
-- | arguments, merging declarations where requried. This will later indicate
-- | wether or not to collect values in to an array, etc.
-- |
-- | XXX: Explain more here how merging is done and how repetition is merged
-- |      and so on.
reduceUsage :: D.Usage -> List D.Argument
reduceUsage = Map.values <<< reduceBranches false <<< D.runUsage

    where
    reduceBranches :: Boolean -- ^ force repeatablity? This is used so that
                              -- nested groups inherit super-group's
                              -- repeatablity, collecting values into arrays.
                   -> List D.Branch
                   -> Map Key D.Argument
    reduceBranches r bs =
      let ms = combine <<< (expand <$> _) <$> bs
      in foldl (Map.unionWith resolveAcrossBranches)
                Map.empty
                ((flip D.setRepeatableOr r <$> _) <$> ms)
      where
      combine :: List (Map Key D.Argument) -> Map Key D.Argument
      combine xs = foldl (Map.unionWith resolveInSameBranch)
                          Map.empty
                          xs

    expand :: D.Argument -> Map Key D.Argument
    expand (D.Group grp) =
      reduceBranches grp.repeatable
        $ ((flip D.setRepeatableOr grp.repeatable) <$> _)
            <$> grp.branches

    expand arg = Map.singleton (key arg) arg

    resolveAcrossBranches :: D.Argument -> D.Argument -> D.Argument
    resolveAcrossBranches (D.Option o) (D.Option o')
        = D.Option (o {
                    arg = do
                      a  <- o.arg  <|> o'.arg
                      a' <- o'.arg <|> o.arg
                      pure {
                        name:     a.name
                      , default:  a.default  <|> a'.default
                      , optional: a.optional || a'.optional
                      }
                    , repeatable = o.repeatable || o'.repeatable
                    })
    resolveAcrossBranches a b = D.setRepeatable a (D.isRepeatable a ||
                                                   D.isRepeatable b)

    resolveInSameBranch :: D.Argument -> D.Argument -> D.Argument
    resolveInSameBranch (D.Option o) (D.Option o')
        = D.Option (o {
                    arg = do
                      a  <- o.arg  <|> o'.arg
                      a' <- o'.arg <|> o.arg
                      pure {
                        name:     a.name
                      , default:  a.default  <|> a'.default
                      , optional: a.optional || a'.optional
                      }
                    , repeatable = true
                    })
    resolveInSameBranch a b = D.setRepeatable a true
