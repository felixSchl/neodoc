module Data.String.Ext (
  (^=)
  ) where

import Prelude
import Data.String as Str
import Data.Function (on)

(^=) :: String -> String -> Boolean
(^=) = eq `on` Str.toUpper
