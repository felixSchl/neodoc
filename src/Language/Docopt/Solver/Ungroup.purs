-- | Ungroup groups into their super groups if they only have one branch and all
-- | contained elements are optional.

module Language.Docopt.Solver.Ungroup where

import Prelude
import Data.List (List(), singleton)
import Language.Docopt

unGroup :: Specification -> Specification
unGroup spec = do
  usage <- spec
  pure do
    branch <- usage
    pure do
      arg <- branch
      unGroupArg arg

unGroupArg :: Argument -> List Argument
-- unGroupArg (Group { branches }) = 
unGroupArg a = singleton a
