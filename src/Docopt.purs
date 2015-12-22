module Docopt where

import Prelude
import Data.Either
import Data.Maybe
import Data.List
import Data.Foldable (intercalate)
import Data.Monoid (Monoid)
import Data.String (fromChar)

--------------------------------------------------------------------------------

type Name          = String
type IsRepeatable  = Boolean
type IsOptional    = Boolean
type TakesArgument = Boolean
type Flag          = Char

data Program     = Program (List Application)
data Application = Application (List Branch)
data Branch      = Branch (List Argument)
data Argument
  = Command     String
  | Positional  String IsRepeatable
  | Option      (Maybe Flag)
                (Maybe Name)
                (Maybe OptionArgument)
                IsRepeatable
  | Group       IsOptional (List Branch) IsRepeatable
data OptionArgument = OptionArgument Name (Maybe Value)

data Value
  = StringValue String
  | BoolValue   Boolean

--------------------------------------------------------------------------------

prettyPrintArg :: Argument -> String
prettyPrintArg (Command name)      = name
prettyPrintArg (Positional name r) = name ++ (if r then "..." else "")
prettyPrintArg (Option flag name arg r)
  = short ++ long ++ arg' ++ rep ++ default
  where
    short   = maybe "" (\f -> "-" ++ (fromChar f)) flag
    long    = maybe "" (const ", ") flag ++ maybe "" ("--" ++) name
    rep     = if r then "..." else ""
    arg'    = flip (maybe "") arg \(OptionArgument n _) -> "="  ++ n
    default = flip (maybe "") arg \(OptionArgument _ d) ->
                flip (maybe "") d \d' -> " [default: " ++ (show d') ++  "]"

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

prettyPrintValue :: Value -> String
prettyPrintValue (StringValue s) = (show s)
prettyPrintValue (BoolValue b)   = (show b)

--------------------------------------------------------------------------------

instance showApplication :: Show Application where
  show (Application xs) = "Application " ++ show (show <$> xs)

instance showBranch :: Show Branch where
  show (Branch xs) = "Branch " ++ show (show <$> xs)

instance eqBranch :: Eq Branch where
  eq (Branch xs) (Branch xs') = (xs == xs')

instance showArgument :: Show Argument where
  show (Command n)
    = intercalate " " [ "Command", show n ]
  show (Positional n r)
    = intercalate " " [ "Positional", show n, show r ]
  show (Group o bs r) 
    = intercalate " " [ "Group", show o, show bs, show r ]
  show (Option f n a r)
    = intercalate " " [ "Option", show f, show n, show a, show r ]

instance eqArgument :: Eq Argument where
  eq (Command n)      (Command n')         = (n == n')
  eq (Positional n r) (Positional n' r')   = (n == n') && (r == r')
  eq (Group o bs r)   (Group o' bs' r')    = (o == o') && (bs == bs') && (r == r')
  eq (Option f n a r) (Option f' n' a' r') = (f == f') && (n == n') && (a == a') && (r == r')

instance showOptionArgument :: Show OptionArgument where
  show (OptionArgument n a) = (show n) ++ " " ++ (show a)

instance eqOptionArgument :: Eq OptionArgument where
  eq (OptionArgument n a) (OptionArgument n' a') = (n == n') && (a == a')

instance showValue :: Show Value where
  show (StringValue s) = "StringValue " ++ s
  show (BoolValue b)   = "BoolValue "   ++ (show b)

instance eqValue :: Eq Value where
  eq (StringValue s) (StringValue s') = (s == s')
  eq (BoolValue b)   (BoolValue b')   = (b == b')

instance semigroupApplication :: Semigroup Application where
  append (Application xs) (Application ys) = Application (xs <> ys)

instance monoidApplication :: Monoid Application where
  mempty = Application Nil

isRepeatable :: Argument -> Boolean
isRepeatable (Option _ _ _ r) = r
isRepeatable (Positional _ r) = r
isRepeatable _                = false

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
