module Language.Docopt.ParserGen.Trans (
    reduce
  , byName
  ) where

import Prelude
import Debug.Trace
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Foldable (foldl)
import qualified Data.Array as A
import Data.Map (Map())
import Data.String (fromChar)
import qualified Data.Map as Map
import qualified Data.String as Str
import Data.Tuple (Tuple(..))
import qualified Language.Docopt.Errors   as D
import qualified Language.Docopt.Value    as D
import qualified Language.Docopt.Argument as D
import qualified Language.Docopt.Option   as O
import Data.List (List(..), toList, concat)
import qualified Data.List as L
import Language.Docopt.ParserGen.Types (ValueMapping())

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
    toKeys (D.Option f n _ _) = []
                              ++ maybe [] (\c -> [ "-"  ++ fromChar c ]) f
                              ++ maybe [] (\s -> [ "--" ++ s ]) n

    -- XXX: How to resolve this?
    resolve :: D.Value -> D.Value -> D.Value
    resolve v v' = v'

-- Reduce the list of (Argument, Value) mappings down to a Set.
-- This means that duplicate Arguments must somehow be resolved.
--
-- XXX: `toValMap` needs to be extended to merge 'similar' keys.
-- Consider, for example, two options that are talking about the same
-- flag and name combination, then how to solve the following issues:
--
--  * Two options differ in:
--      * Their argument name           -> ?
--      * Their argument default value  -> ?
--      * Their ability to repeat       -> repeat if any option does
--      * Their names:
--          * (f, foo) vs. (b, foo)
--          * (f, foo) vs. (f, baz)
--
reduce :: Tuple D.Branch (List ValueMapping) -> Map D.Argument D.Value
reduce (Tuple b vs) = mergeDefVals b $ toValMap vs
  where
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

    mergeDefVals :: D.Branch -> Map D.Argument D.Value -> Map D.Argument D.Value
    mergeDefVals (D.Branch b) m = foldl step m b
      where
        step :: Map D.Argument D.Value
            -> D.Argument
            -> Map D.Argument D.Value
        step m d = maybe m (`Map.unionWith resolve` m)
                           (Map.singleton d <$> toDefVal d)

        resolve :: D.Value -> D.Value -> D.Value
        resolve _ v = v

        toDefVal :: D.Argument -> Maybe D.Value
        toDefVal (D.Option _ _ (Just (O.Argument _ (Just v))) r)
          = return $
              if (D.isArrayValue v || not r)
                then v
                else D.ArrayValue [v]
        toDefVal (D.Positional _ r)
          = if r
              then return $ D.ArrayValue []
              else Nothing
        toDefVal (D.EOA) = Just $ D.ArrayValue []
        toDefVal _ = Nothing
