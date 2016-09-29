module Neodoc.Env (
  Env ()
, empty
, fromFoldable
, member
, lookup
, unwrapEnv
, EnvWrapper ()
) where

import Prelude
import Data.StrMap as StrMap
import Data.Foldable (class Foldable)
import Data.Maybe (Maybe)
import Data.StrMap (StrMap)
import Data.Tuple (Tuple)
import Data.Foreign (F)
import Data.Foreign as F
import Data.Foreign.Class as F
import Data.Foreign.Index as F
import Data.Foreign.Index ((!))
import Data.Foreign.Class
import Data.Foreign.Extra as F
import Unsafe.Coerce (unsafeCoerce)

type Env = StrMap String

empty :: StrMap String
empty = StrMap.empty

fromFoldable :: ∀ f. (Foldable f) => f (Tuple String String) -> StrMap String
fromFoldable = StrMap.fromFoldable

member :: String -> StrMap String -> Boolean
member = StrMap.member

lookup :: String -> StrMap String -> Maybe String
lookup = StrMap.lookup

-- `EnvWrapper` is a newtype i.o. to be able to add an `IsForeign` instance.
newtype EnvWrapper = EnvWrapper Env

unwrapEnv :: EnvWrapper -> Env
unwrapEnv (EnvWrapper e) = e

instance isForeignEnvWrapper :: IsForeign EnvWrapper where
  read v = unsafeCoerce <$> F.readObject v
