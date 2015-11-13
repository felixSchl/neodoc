module Docopt.Solve where

import Data.Maybe (Maybe(..))
import Data.List (List(..))

type ProgramSpecification = List ProgramApplication
type ProgramApplication   = List Branch
type Branch               = List Argument
type Name                 = String
type IsRepeatable         = Boolean
type IsOptional           = Boolean
type TakesArgument        = Boolean
type Flag                 = Char

data Argument
  = Command     String
  | Positional  String IsRepeatable
  | Option      (Maybe Flag)
                (Maybe Name)
                (TakesArgument)
                (Maybe String)
                IsRepeatable
  | Group       IsOptional (List Branch) IsRepeatable

solve :: (List P.Usage) -> (List P.Option) -> ProgramSpecification
solve _ _ = Nil
