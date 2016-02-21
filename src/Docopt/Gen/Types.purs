module Docopt.Gen.Types where

import Prelude
import Data.Maybe (Maybe(..), maybe)
import Data.List (List(..))
import Data.String (fromCharArray)
import Data.Tuple (Tuple(..))
import qualified Data.Array as A
import qualified Text.Parsing.Parser as P
import qualified Docopt.Types as Docopt

-- | Represents each item in ARGV
data Token
  = LOpt String (Maybe String)
  | SOpt Char (Array Char) (Maybe String)
  | EOA (List Docopt.Value)
  | Lit String

instance showToken :: Show Token where
  show (LOpt s a)    = "LOpt " ++ show s ++ " " ++ show a
  show (SOpt c cs a) = "SOpt " ++ show c ++ " " ++ show cs ++ " " ++ show a
  show (Lit  s)      = "Lit "  ++ show s
  show (EOA  xs)     = "EOA "  ++ show xs

-- | Represents the mapping of a parsed argument to a user-provided value
-- | E.g.: ("-f, --foo", "100")
-- |
-- | XXX: Move this (!)
type ValueMapping = Tuple Docopt.Argument Docopt.Value
