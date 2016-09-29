-- Annotate a layout with description information

module Neodoc.Evaluate.Annotate where

import Prelude
import Data.Tuple (Tuple, fst, snd)
import Data.Tuple.Nested ((/\))
import Data.Map (Map)
import Data.Map as Map
import Data.Set (Set)
import Data.Set as Set
import Data.Maybe (Maybe(..))
import Data.List (List(..), (:), head, filter, singleton, fromFoldable)
import Data.NonEmpty (NonEmpty)
import Data.Foldable (any)
import Neodoc.Env (Env)
import Neodoc.Data.Layout (Layout(..), Branch)
import Neodoc.Data.Description (Description(..))
import Neodoc.Data.SolvedLayout (SolvedLayout, SolvedLayoutArg)
import Neodoc.Data.SolvedLayout as Solved
import Neodoc.Value.RichValue
import Neodoc.ArgParser.KeyValue (KeyValue)
import Neodoc.ArgKey (ArgKey(..))
import Neodoc.ArgKey.Class (class ToArgKey, toArgKey)

-- Decorate an `a` with an accompanying description
type AnnotatedLayout a = Layout (WithDescription a)
type WithDescription a = Tuple a (Maybe Description)

getArg :: ∀ a. WithDescription a -> a
getArg = fst

getDescription :: ∀ a. WithDescription a -> Maybe Description
getDescription = snd

-- Annotate sth that can resolve an `ArgKey` with it's description
annotate'
  :: ∀ a
   . (ToArgKey a)
  => List Description
  -> a
  -> WithDescription a
annotate' descriptions x = x /\ (findDescription descriptions (toArgKey x))

-- Annotate a layout of arguments with it's description
annotate
  :: List Description
  -> ArgKey
  -> WithDescription ArgKey
annotate descriptions x = x /\ (findDescription descriptions x)

-- Annotate a layout of arguments with it's description
annotateLayout
  :: ∀ a
   . (ToArgKey a)
  => List Description
  -> Layout a
  -> AnnotatedLayout a
annotateLayout descriptions l = l <#> \x ->
  x /\ (findDescription descriptions $ toArgKey x)

-- XXX: This could be more efficient using a table lookup
-- XXX: This is copied from `ArgParser.Type` - {c,sh}ould be moved
findDescription
  :: List Description
  -> ArgKey
  -> Maybe Description
findDescription descriptions (OptionKey alias)
  = head $ filter matchesAlias descriptions
    where
    matchesAlias (OptionDescription aliases _ _ _ _) = any (_ == alias) aliases
    matchesAlias _ = false
findDescription _ _ = Nothing

-- Find the set of arg keys that describe the same option
findArgKeys
  :: List Description
  -> ArgKey
  -> Set ArgKey
findArgKeys descriptions k =
  let xs = case findDescription descriptions k of
        Just (OptionDescription aliases _ _ _ _) ->
          k : (fromFoldable $ OptionKey <$> aliases)
        _ -> singleton k
   in Set.fromFoldable xs
