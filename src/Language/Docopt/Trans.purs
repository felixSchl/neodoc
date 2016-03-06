module Language.Docopt.Trans (
    reduce
  , byName
  ) where

import Prelude
import Debug.Trace
import Data.Maybe (Maybe(..), isJust, isNothing, maybe)
import Data.Foldable (foldl)
import Control.Plus (empty)
import Data.Bifunctor (lmap)
import Data.Array as A
import Data.StrMap as StrMap
import Data.StrMap (StrMap())
import Data.Map (Map())
import Data.String (fromChar)
import Data.Map as Map
import Data.String as Str
import Data.Tuple (Tuple(..), fst)
import Control.MonadPlus (guard)
import Language.Docopt.Env (Env())
import Language.Docopt.Errors   as D
import Language.Docopt.Env      as D
import Language.Docopt.Value    as D
import Language.Docopt.Argument as D
import Language.Docopt.Option   as O
import Language.Docopt.Env      as Env
import Data.String.Ext ((^=))
import Data.List (List(..), toList, concat)
import Data.List as L
import Language.Docopt.ParserGen.ValueMapping

-- Transform the map of (Argument, Value) mappings to a map of (String, Value),
-- where the String is the name of the option and it's aliases.
-- This means that Arguments that resolve to the same name/alias must be
-- somehow resolved.
byName :: Map D.Argument D.Value -> Map String D.Value
byName m
  = foldl (Map.unionWith resolve)
          Map.empty
          (L.concat $ Map.toList m <#> \(Tuple k v) ->
            toList $ toKeys k <#> \k' -> Map.singleton k' v)
  where

    toKeys :: D.Argument -> Array String
    toKeys (D.Command n)      = [n]
    toKeys (D.Positional n _) = [ "<" ++ Str.toLower n ++ ">"
                                , Str.toUpper n
                                ]
    toKeys (D.Group _ _ _)    = []
    toKeys (D.EOA)            = ["--"]
    toKeys (D.Stdin)          = ["-"]
    toKeys (D.Option (O.Option o))
                              = []
                              ++ maybe [] (\c -> [ "-"  ++ fromChar c ]) o.flag
                              ++ maybe [] (\s -> [ "--" ++ s ]) o.name

    -- This *should* be safe, since all confusion should have been
    -- lifted by the `reduce` function. This function cannot be implemented
    -- other than selecting one of the values verbatim, because it's lacking
    -- all context about these values (i.e. the Argument they belong to).
    resolve :: D.Value -> D.Value -> D.Value
    resolve _ v' = v'

-- Reduce the list of (Argument, Value) mappings down to a Set.
-- This means that duplicate Arguments must somehow be resolved.
--
-- XXX: How to solve the following issues:
--
--  * Two options differ in:
--      * Their argument name           -> ?
--      * Their argument default value  -> ?
--
reduce :: D.Env                  -- ^ the environment
       -> D.Branch               -- ^ the specification
       -> List ValueMapping      -- ^ the parse result
       -> Map D.Argument D.Value -- ^ the output set of (arg => val)
reduce env b m =
  let unified = lmap unify <$> m
   in toValMap unified
    # applyEnvVals b
    # applyDefVals b

  where
    -- Find the lowest comment denominator between two arguments
    unify :: D.Argument -> D.Argument
    unify a = foldl step a (fst <$> m)
      where
        step :: D.Argument -> D.Argument -> D.Argument
        step (D.Positional n r) (D.Positional n' r') | n ^= n'
          = D.Positional n (r || r')
        step (D.Option (O.Option o)) (D.Option (O.Option o'))
          |    (o.name == o'.name)
            && (o.flag == o'.flag)
            && ((isNothing o.arg && isNothing o'.arg) || isJust do
                  (O.Argument { name: an  }) <- o.arg
                  (O.Argument { name: an' }) <- o'.arg
                  guard (an ^= an')
                )
          = D.Option $ O.Option $ o {
              repeatable = o.repeatable || o'.repeatable
            }
        step a _ = a

    toValMap :: List (Tuple D.Argument D.Value) -> Map D.Argument D.Value
    toValMap vs = foldl step Map.empty (prepare <$> vs)
      where
        step :: Map D.Argument D.Value
             -> Tuple D.Argument D.Value
             -> Map D.Argument D.Value
        step m (Tuple k v) = Map.unionWith (resolve k) (Map.singleton k v) m

        prepare :: Tuple D.Argument D.Value -> Tuple D.Argument D.Value
        prepare (Tuple k v) | D.isRepeatable k
          = Tuple k (D.ArrayValue $
              (case v of
                D.ArrayValue xs -> xs
                _               -> [v]
              ))
        prepare k = k

        resolve :: D.Argument -> D.Value -> D.Value -> D.Value
        resolve _ (D.ArrayValue xs) (D.ArrayValue xs') = D.ArrayValue (xs' ++ xs)
        resolve _ (D.ArrayValue xs) v = D.ArrayValue (v A.: xs)
        resolve a v v' | D.isRepeatable a
          = D.ArrayValue $
              (case v' of
                D.ArrayValue xs -> xs
                _               -> [v']
              ) ++ [v]
        resolve _ v v' = v

    applyEnvVals :: D.Branch               -- ^ the specification
                 -> Map D.Argument D.Value -- ^ the input set
                 -> Map D.Argument D.Value -- ^ the output set
    applyEnvVals (D.Branch b) m = foldl step m b
      where
        step :: Map D.Argument D.Value
             -> D.Argument
             -> Map D.Argument D.Value
        step m d = maybe m (`Map.unionWith resolve` m)
                           (Map.singleton d <$> toEnvVal d)

        resolve :: D.Value -> D.Value -> D.Value
        resolve _ v = v -- choose existing value, not env

        toEnvVal :: D.Argument -> Maybe D.Value
        toEnvVal (D.Option (O.Option o@{ env: Just k }))
          = D.StringValue <$> Env.lookup k env
        toEnvVal _ = Nothing

    applyDefVals :: D.Branch               -- ^ the specification
                 -> Map D.Argument D.Value -- ^ the input set
                 -> Map D.Argument D.Value -- ^ the output set
    applyDefVals (D.Branch b) m = foldl step m b
      where
        step :: Map D.Argument D.Value
             -> D.Argument
             -> Map D.Argument D.Value
        step m d = maybe m (`Map.unionWith resolve` m)
                           (Map.singleton d <$> toDefVal d)

        resolve :: D.Value -> D.Value -> D.Value
        resolve _ v = v -- choose existing value, not default

        toDefVal :: D.Argument -> Maybe D.Value
        toDefVal (D.Option (O.Option o@{
                  arg: Just (O.Argument { default: Just v })
                }))
          = return $
              if (D.isArrayValue v || not o.repeatable)
                then v
                else D.ArrayValue [v]
        toDefVal (D.Positional _ r)
          = if r
              then return $ D.ArrayValue []
              else Nothing
        toDefVal (D.EOA) = Just $ D.ArrayValue []
        toDefVal _ = Nothing