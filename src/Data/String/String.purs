module Data.String.Ext (
    (^=), (^/=)
  , upperCaseEq
  , notUpperCaseEq
  , endsWith
  , startsWith
  ) where

import Prelude
import Data.Maybe (maybe)
import Data.String as Str
import Data.Function (on)

upperCaseEq :: String -> String -> Boolean
upperCaseEq = eq `on` Str.toUpper
infixl 9 upperCaseEq as ^=

notUpperCaseEq :: String -> String -> Boolean
notUpperCaseEq a b = not (a ^= b)
infixl 9 notUpperCaseEq as ^/=

startsWith :: String -> String -> Boolean
startsWith needle haystack = maybe false id do
  ix <- Str.indexOf needle haystack
  return $ ix == 0

endsWith :: String -> String -> Boolean
endsWith needle haystack = maybe false id do
  ix <- Str.lastIndexOf needle haystack
  return $ ix == (Str.length haystack - Str.length needle)
