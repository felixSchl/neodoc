module Docopt where

import Prelude
import Data.Either
import Data.Maybe
import Data.List

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

import qualified Text.Parsing.Parser as P

data DocoptError
  = ScanError   P.ParseError
  | LexError    P.ParseError
  | ParseError  P.ParseError
  | GenError    String
  | SolverError String
  | RunError    P.ParseError

instance showError :: Show DocoptError where
  show (ScanError err)   = "ScanError "   ++ show err
  show (LexError err)    = "LexError "    ++ show err
  show (ParseError err)  = "ParseError "  ++ show err
  show (GenError msg)    = "GenError "    ++ show msg
  show (SolverError msg) = "SolverError " ++ show msg
  show (RunError err)    = "RunError "    ++ show err
