module Neodoc.Evaluate.Reduce (reduce) where

import Prelude
import Data.Map (Map)
import Data.Map as Map
import Data.List (List)
import Neodoc.Env (Env)
import Neodoc.Value.RichValue
import Neodoc.Spec (Branch)
import Neodoc.ArgParser.KeyValue (KeyValue)
import Neodoc.ArgKey (ArgKey)
import Neodoc.ArgKey.Class (class ToArgKey, toArgKey)

reduce
  :: ∀ a
   . (ToArgKey a)
  => Env
  -> Branch a
  -> List KeyValue
  -> Map ArgKey RichValue
reduce env branch vs = Map.empty
