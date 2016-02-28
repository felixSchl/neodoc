module Language.Docopt.Types where

import Prelude
import Debug.Trace
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.List (List(..))
import Data.Function (on)
import Data.Foldable (intercalate)
import Data.Monoid (Monoid)
import Data.String (fromChar)
import Control.Apply ((*>))
import Data.Generic
import qualified Data.String as Str

type Name = String
type IsRepeatable = Boolean
type IsOptional = Boolean
type TakesArgument = Boolean
type Flag = Char

type Program = List Usage
newtype Usage = Usage (List Branch)
newtype Branch = Branch (List Argument)
data Argument
  = Command     String
  | Positional  String IsRepeatable
  | Option      (Maybe Flag)
                (Maybe Name)
                (Maybe OptionArgument)
                IsRepeatable
  | Group       IsOptional (List Branch) IsRepeatable
  | EOA

data OptionArgument = OptionArgument Name (Maybe Value)

data Value
  = StringValue String
  | BoolValue   Boolean
  | ArrayValue  (Array Value)

--------------------------------------------------------------------------------
-- Instances -------------------------------------------------------------------
--------------------------------------------------------------------------------

instance showUsage :: Show Usage where
  show (Usage xs) = "Usage " ++ show (show <$> xs)

instance eqUsage :: Eq Usage where
  eq (Usage xs) (Usage ys) = xs == ys

instance showBranch :: Show Branch where
  show (Branch xs) = "Branch " ++ show (show <$> xs)

instance eqBranch :: Eq Branch where
  eq (Branch xs) (Branch xs') = (xs == xs')

instance showArgument :: Show Argument where
  show (EOA) = "--"
  show (Command n)
    = intercalate " " [ "Command", show n ]
  show (Positional n r)
    = intercalate " " [ "Positional", show n, show r ]
  show (Group o bs r) 
    = intercalate " " [ "Group", show o, show bs, show r ]
  show (Option f n a r)
    = intercalate " " [ "Option", show f, show n, show a, show r ]

instance ordArgument :: Ord Argument where
  -- XXX: Implement a more efficient `compare` function
  compare = compare `on` show

instance eqArgument :: Eq Argument where
  eq (EOA) (EOA) = true
  eq (Command n) (Command n') = (n == n')
  eq (Positional n r) (Positional n' r')
    = (Str.toUpper n == Str.toUpper n') && (r == r')
  eq (Group o bs r) (Group o' bs' r') = (o == o') && (bs == bs') && (r == r')
  eq (Option f  n  (Just (OptionArgument a  _)) r)
     (Option f' n' (Just (OptionArgument a' _)) r')
      = (f == f') && (n == n') && (Str.toUpper a == Str.toUpper a') && (r == r')
  eq (Option f  n  Nothing r)
     (Option f' n' Nothing r')
      = (f == f') && (n == n') && (r == r')
  eq _ _ = false

instance showOptionArgument :: Show OptionArgument where
  show (OptionArgument n a) = (show n) ++ " " ++ (show a)

instance eqOptionArgument :: Eq OptionArgument where
  eq (OptionArgument n a) (OptionArgument n' a')
    = (Str.toUpper n == Str.toUpper n') && (a == a')

instance showValue :: Show Value where
  show (StringValue s) = "StringValue " ++ s
  show (BoolValue b)   = "BoolValue "   ++ (show b)
  show (ArrayValue xs) = "ArrayValue "  ++ show (show <$> xs)

instance eqValue :: Eq Value where
  eq (StringValue s) (StringValue s') = (s == s')
  eq (BoolValue b)   (BoolValue b')   = (b == b')
  eq (ArrayValue xs) (ArrayValue xs') = (xs == xs')
  eq _               _                = false

instance semigroupUsage :: Semigroup Usage where
  append (Usage xs) (Usage ys) = Usage (xs <> ys)

instance monoidUsage :: Monoid Usage where
  mempty = Usage Nil

--------------------------------------------------------------------------------
-- Errors (XXX: needs migration and improvement) -------------------------------
--------------------------------------------------------------------------------

import qualified Text.Parsing.Parser as P

data DescriptionError
  = ArgumentMismatchError {
      option :: {
        flag :: Maybe Flag
      , name :: Maybe Name
      , arg  :: Maybe String
      }
    , description :: {
        arg :: Maybe String
      }
    }

data SolveError
  = DescriptionError DescriptionError

data DocoptError
  = DocoptScanError   P.ParseError
  | DocoptParseError  P.ParseError
  | DocoptSolveError  SolveError

derive instance genericSolveError       :: Generic SolveError
derive instance genericDescriptionError :: Generic DescriptionError

instance showSolveError :: Show SolveError where
  show = gShow

instance showDescriptionError :: Show DescriptionError where
  show = gShow

instance showDocoptError :: Show DocoptError where
  show (DocoptScanError err)  = "DocoptScanError "  ++ show err
  show (DocoptParseError err) = "DocoptParseError " ++ show err
  show (DocoptSolveError err) = "DocoptSolveError"  ++ show err

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

isRepeatable :: Argument -> Boolean
isRepeatable (Option _ _ _ r) = r
isRepeatable (Positional _ r) = r
isRepeatable _                = false

hasDefault :: Argument -> Boolean
hasDefault (Option _ _ (Just (OptionArgument _ (Just _))) _) = true
hasDefault _                                                 = false

takesArgument :: Argument -> Boolean
takesArgument (Option _ _ (Just _) _) = true
takesArgument _                       = false

isFlag :: Argument -> Boolean
isFlag (Option _ _ (Just (OptionArgument _ (Just (BoolValue _)))) _) = true
isFlag _                                                             = false

isOption :: Argument -> Boolean
isOption (Option _ _ _ _) = true
isOption _                = false

isSameValueType :: Value -> Value -> Boolean
isSameValueType (StringValue _) (StringValue _) = true
isSameValueType (BoolValue _)   (BoolValue _)   = true
isSameValueType (ArrayValue _)  (ArrayValue _)  = true
isSameValueType _               _               = false

isBoolValue :: Value -> Boolean
isBoolValue (BoolValue _) = true
isBoolValue _             = false

isArrayValue :: Value -> Boolean
isArrayValue (ArrayValue _) = true
isArrayValue _              = false
