module Language.Docopt.Env (
    Env ()
  , empty
  , fromFoldable
  , member
  , lookup
  ) where

import Data.StrMap as StrMap
import Data.StrMap (StrMap ())

type Env = StrMap String

empty :: StrMap String
empty = StrMap.empty

-- XXX: Add type annontations
fromFoldable = StrMap.fromFoldable
member = StrMap.member
lookup = StrMap.lookup
