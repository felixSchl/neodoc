module Language.Docopt.Env (
    Env ()
  , empty
  , fromFoldable
  , member
  , lookup
  ) where

import Prelude
import Data.StrMap as StrMap
import Data.Foldable (class Foldable)
import Data.Maybe (Maybe)
import Data.StrMap (StrMap)
import Data.Tuple (Tuple)

type Env = StrMap String

empty :: StrMap String
empty = StrMap.empty

fromFoldable :: ∀ f. (Foldable f) => f (Tuple String String) -> StrMap String
fromFoldable = StrMap.fromFoldable

member :: String -> StrMap String -> Boolean
member = StrMap.member

lookup :: String -> StrMap String -> Maybe String
lookup = StrMap.lookup
