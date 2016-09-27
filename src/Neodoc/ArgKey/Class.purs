module Neodoc.ArgKey.Class where

import Neodoc.ArgKey (ArgKey)

class ToArgKey a where
  toArgKey :: a -> ArgKey
