module Docopt.Gen where

import Prelude
import Data.Maybe (Maybe(..), maybe)
import Data.String (fromCharArray)
import qualified Data.Array as A

-- | Represents each item in ARGV
data Token
  = LOpt String (Maybe String)
  | SOpt Char (Array Char) (Maybe String)
  | Lit  String

prettyPrintToken :: Token -> String
prettyPrintToken (Lit s) = show s
prettyPrintToken (LOpt n a) = "--" ++ n ++ arg
  where arg = maybe "" ("=" ++) a
prettyPrintToken (SOpt n s a) = "-"  ++ (fromCharArray (A.cons n s)) ++ arg
  where arg = maybe "" ("=" ++) a

instance showToken :: Show Token where
  show (LOpt s a)    = "LOpt " ++ show s ++ " " ++ show a
  show (SOpt c cs a) = "SOpt " ++ show c ++ " " ++ show cs ++ " " ++ show a
  show (Lit  s)      = "Lit "  ++ show s

