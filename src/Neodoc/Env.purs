module Neodoc.Env
  ( Env ()
  , empty
  , fromFoldable
  , member
  , lookup
  )
where

import Data.Foldable (class Foldable)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)


type Env = Map String String


empty :: Map String String
empty = Map.empty


fromFoldable :: ∀ f. (Foldable f) => f (Tuple String String) -> Map String String
fromFoldable = Map.fromFoldable


member :: String -> Map String String -> Boolean
member = Map.member


lookup :: String -> Map String String -> Maybe String
lookup = Map.lookup
