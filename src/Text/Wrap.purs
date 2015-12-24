module Text.Wrap where

import Prelude
import qualified Data.Array as A
import Data.String (toCharArray, fromCharArray)
import qualified Data.String as Str
import Data.Maybe

dedent :: String -> String
dedent txt =
  let lines :: Array String
      lines = Str.split "\n" txt
      nonEmpty :: String -> Boolean
      nonEmpty = (/= 0) <<< Str.length <<< Str.trim
      shortestLeading :: Int
      shortestLeading = maybe 0 id (A.head $ A.sort $ countLeading
                               <$> (A.filter nonEmpty lines))
      isWhitespace :: Char -> Boolean
      isWhitespace ' '  = true
      isWhitespace '\n' = true
      isWhitespace '\t' = true
      isWhitespace _    = false
      countLeading :: String -> Int
      countLeading line = A.length $ A.takeWhile isWhitespace (toCharArray line)
   in Str.joinWith "\n" ((Str.drop shortestLeading) <$> lines)

