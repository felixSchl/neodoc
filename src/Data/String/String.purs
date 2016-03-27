module Data.String.Ext (
    (^=), (^/=)
  , endsWith
  , startsWith
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

startsWith :: String -> String -> Boolean
startsWith needle haystack = maybe false id do
  ix <- Str.indexOf needle haystack
  return $ ix == 0

endsWith :: String -> String -> Boolean
endsWith needle haystack = maybe false id do
  ix <- Str.lastIndexOf needle haystack
  return $ ix == (Str.length haystack - Str.length needle)
