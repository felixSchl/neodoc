module Docopt where

import Prelude
import Data.Either
import Data.Maybe
import Data.List
import Data.Foldable (intercalate)
import Data.Monoid (Monoid)

--------------------------------------------------------------------------------

type ProgramSpecification = List ProgramApplication
type ProgramApplication   = List Branch
type Name                 = String
type IsRepeatable         = Boolean
type IsOptional           = Boolean
type TakesArgument        = Boolean
type Flag                 = Char
type Default              = String

data Branch      = Branch (List Argument)
data Application = Application (List Branch)
data Argument
  = Command     String
  | Positional  String IsRepeatable
  | Option      (Maybe Flag)
                (Maybe Name)
                (Maybe String)
                (Maybe Default)
                IsRepeatable
  | Group       IsOptional (List Branch) IsRepeatable

prettyPrintArg :: Argument -> String
prettyPrintArg (Command name)      = name
prettyPrintArg (Positional name r) = name ++ (if r then "..." else "")
prettyPrintArg (Option flag name arg def r)
  = short ++ long ++ arg' ++ rep ++ default
  where
    short   = maybe "" (\f -> "-" ++ (show f)) flag
    long    = maybe "" ("--" ++) name
    arg'    = maybe "" ("="  ++) arg
    rep     = if r then "..." else ""
    default = maybe "" (\d -> " [default: " ++ d ++  "]") def
prettyPrintArg (Group o bs r) = open ++ inner ++ close ++ repetition
  where
    open       = if o then "[" else "("
    close      = if o then "]" else ")"
    inner      = intercalate " | " (show <$> bs)
    repetition = if r then "..." else ""

prettyPrintBranch :: Branch -> String
prettyPrintBranch (Branch xs) = intercalate " " (prettyPrintArg <$> xs)

prettyPrintApplication :: Application -> String
prettyPrintApplication (Application xs)
  = intercalate " | " (prettyPrintBranch <$> xs)

instance showApplication :: Show Application where
  show (Application xs) = "Application " ++ show (show <$> xs)

instance showBranch :: Show Branch where
  show (Branch xs) = "Branch " ++ show (show <$> xs)

instance showArgument :: Show Argument where
  show (Command n)
    = intercalate " " [ "Command", show n ]
  show (Positional n r)
    = intercalate " " [ "Positional", show n, show r ]
  show (Group o bs r) 
    = intercalate " " [ "Group", show o, show bs, show r ]
  show (Option f n a d r)
    = intercalate " " [ "Option", show f, show n, show a, show d, show r ]

instance semigroupApplication :: Semigroup Application where
  append (Application xs) (Application ys) = Application (xs <> ys)

instance monoidApplication :: Monoid Application where
  mempty = Application Nil

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
