module Data.String.Ext (
  (^=)
  ) where

import Prelude
import Data.String as Str

(^=) :: String -> String -> Boolean
(^=) a b = Str.toUpper a == Str.toUpper b

