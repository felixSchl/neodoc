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
import Data.Foldable (class Foldable)
import Data.Map
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)
import Data.Foreign (F)
import Data.Foreign as F
import Data.Foreign.Class as F
import Data.Foreign.Index as F
import Data.Foreign.Index ((!))
import Data.Foreign.Class
import Data.Foreign.Extra as F
import Unsafe.Coerce (unsafeCoerce)

type Env = Map String String

empty :: Map String String
empty = Map.empty

fromFoldable :: ∀ f. (Foldable f) => f (Tuple String String) -> Map String String
fromFoldable = Map.fromFoldable

member :: String -> Map String String -> Boolean
member = Map.member

lookup :: String -> Map String String -> Maybe String
lookup = Map.lookup

-- `EnvWrapper` is a newtype i.o. to be able to add an `IsForeign` instance.
newtype EnvWrapper = EnvWrapper Env

unwrapEnv :: EnvWrapper -> Env
unwrapEnv (EnvWrapper e) = e

instance isForeignEnvWrapper :: IsForeign EnvWrapper where
  read v = unsafeCoerce <$> F.readObject v
