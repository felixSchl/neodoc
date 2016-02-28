module Language.Docopt.ParserGen.Types where

import Prelude
import Data.Maybe (Maybe(..), maybe)
import Data.List (List(..))
import Data.String (fromCharArray)
import Data.Tuple (Tuple(..))
import qualified Data.Array as A
import qualified Text.Parsing.Parser as P

import qualified Language.Docopt.Types    as D
import qualified Language.Docopt.Value    as D
import qualified Language.Docopt.Argument as D

-- | Represents each item in ARGV
data Token
  = LOpt String (Maybe String)
  | SOpt Char (Array Char) (Maybe String)
  | EOA (List D.Value)
  | Lit String

instance showToken :: Show Token where
  show (LOpt s a)    = "LOpt " ++ show s ++ " " ++ show a
  show (SOpt c cs a) = "SOpt " ++ show c ++ " " ++ show cs ++ " " ++ show a
  show (Lit  s)      = "Lit "  ++ show s
  show (EOA  xs)     = "EOA "  ++ show xs

type ValueMapping = Tuple D.Argument D.Value
