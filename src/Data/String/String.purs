module Data.String.Ext (
    (^=), (^/=)
  , endsWith
  ) where

import Prelude
import Debug.Trace
import Data.Maybe (maybe)
import Data.String as Str
import Data.Function

(^=) :: String -> String -> Boolean
(^=) = eq `on` Str.toUpper

(^/=) :: String -> String -> Boolean
(^/=) a b = not (a ^= b)

endsWith :: String -> String -> Boolean
endsWith needle haystack = maybe false id do
  ix <- Str.lastIndexOf needle haystack
  return $ ix == (Str.length haystack - Str.length needle)
