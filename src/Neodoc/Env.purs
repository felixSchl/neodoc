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
import Data.Map (Map(..))
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)
import Foreign (F)
import Foreign as F
import Foreign.Index as F
import Foreign.Index ((!))
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

-- instance isForeignEnvWrapper :: Foreign EnvWrapper where
--   read v = unsafeCoerce <$> F.readObject v
