module Language.Docopt.Parser.Usage.Argument (
    Argument (..)
  , IsRepeatable ()
  , IsOptional ()
  , Branch ()
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.List (List(..))
import qualified Language.Docopt.Parser.Usage.Option as O

type IsRepeatable = Boolean
type IsOptional = Boolean
type Branch = List Argument
data Argument
  = Command     String
  | Positional  String IsRepeatable
  | Option      String (Maybe O.Argument) IsRepeatable
  | OptionStack Char (Array Char) (Maybe O.Argument) IsRepeatable
  | Group       IsOptional (List Branch) IsRepeatable
  | EOA

instance showArgument :: Show Argument where
  show (EOA)                 = "--"
  show (Command n)           = "Command " ++ n
  show (Positional n b)      = "Positional " ++ n ++ " " ++ show b
  show (Option n a b)        = "Option " ++ show n ++ " " ++ show a ++ " " ++ show b
  show (OptionStack n s a b) = "OptionStack " ++ show n ++ " " ++ show s ++ " " ++ show a ++ " " ++ show b
  show (Group n b o)         = "Group " ++ show n ++ " " ++ show b ++ " " ++ show o

instance eqArgument :: Eq Argument where
  eq (EOA)                  (EOA)                      = true
  eq (Command s)            (Command s')               = (s == s')
  eq (Positional s r)       (Positional s' r')         = (s == s') && (r == r')
  eq (Option s a r)         (Option s' a' r')          = (s == s') && (a == a') && (r == r')
  eq (Group b xs r)         (Group b' xs' r')          = (b == b') && (xs == xs') && (r == r')
  eq (OptionStack c cs a r) (OptionStack c' cs' a' r') = (c == c') && (cs == cs') && (a == a') && (r == r')
  eq _                      _                          = false

