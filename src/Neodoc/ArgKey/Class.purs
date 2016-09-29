module Neodoc.ArgKey.Class where

import Neodoc.ArgKey (ArgKey(..))
import Neodoc.OptionAlias

class ToArgKey a where
  toArgKey :: a -> ArgKey

instance toArgKeyOptionAlias :: ToArgKey OptionAlias where
  toArgKey x = OptionKey x
