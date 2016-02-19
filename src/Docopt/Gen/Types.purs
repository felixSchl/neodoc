module Docopt.Gen.Types where

import Prelude
import Data.Maybe (Maybe(..), maybe)
import Data.String (fromCharArray)
import Data.Tuple (Tuple(..))
import qualified Data.Array as A
import qualified Text.Parsing.Parser as P
import Docopt (Argument(..), Value(..))

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

-- | Represents the mapping of a parsed argument to a user-provided value
-- | E.g.: ("-f, --foo", "100")
type ValueMapping = Tuple Argument Value
