module Language.Docopt.Argument (
    Argument (..)
  , IsRepeatable ()
  , Branch (..)
  , Flag ()
  , Name ()
  , IsOptional ()
  , IsRepeatable ()
  , prettyPrintBranch
  , prettyPrintArg
  , isRepeatable
  , hasDefault
  , takesArgument
  , isOption
  , isFlag
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import qualified Language.Docopt.Option as O
import Data.List (List())
import Data.Foldable (intercalate)
import qualified Data.String as Str
import Data.Function (on)
import Data.Maybe (maybe)
import Data.String (fromChar)
import Control.Apply ((*>))
import Language.Docopt.Value (Value(..), prettyPrintValue)

type Flag = Char
type Name = String
type IsRepeatable = Boolean
type IsOptional = Boolean

newtype Branch = Branch (List Argument) -- XXX: Move to type alias

data Argument
  = Command     String
  | Positional  String IsRepeatable
  | Option      (Maybe Flag)
                (Maybe Name)
                (Maybe O.Argument)
                IsRepeatable
  | Group       IsOptional (List Branch) IsRepeatable
  | EOA

instance showBranch :: Show Branch where
  show (Branch xs) = "Branch " ++ show (show <$> xs)

instance eqBranch :: Eq Branch where
  eq (Branch xs) (Branch xs') = (xs == xs')

instance showArgument :: Show Argument where
  show (EOA) = "EOA"
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
  eq (Option f  n  (Just (O.Argument a  _)) r)
     (Option f' n' (Just (O.Argument a' _)) r')
      = (f == f') && (n == n') && (Str.toUpper a == Str.toUpper a') && (r == r')
  eq (Option f  n  Nothing r)
     (Option f' n' Nothing r')
      = (f == f') && (n == n') && (r == r')
  eq _ _ = false

prettyPrintBranch :: Branch -> String
prettyPrintBranch (Branch xs) = intercalate " " (prettyPrintArg <$> xs)

prettyPrintArg :: Argument -> String
prettyPrintArg (EOA)               = "--"
prettyPrintArg (Command name)      = name
prettyPrintArg (Positional name r) = name ++ (if r then "..." else "")
prettyPrintArg (Option flag name arg r)
  = short ++ long ++ arg' ++ rep ++ default
  where
    short   = maybe "" (\f -> "-" ++ (fromChar f)) flag
    long    = maybe "" (const ", ") (flag *> name) ++ maybe "" ("--" ++) name
    rep     = if r then "..." else ""
    arg'    = flip (maybe "") arg \(O.Argument n _) -> "="  ++ n
    default = flip (maybe "") arg \(O.Argument _ d) ->
                flip (maybe "") d \d' ->
                  " [default: " ++ (prettyPrintValue d') ++  "]"

prettyPrintArg (Group o bs r) = open ++ inner ++ close ++ repetition
  where
    open       = if o then "[" else "("
    close      = if o then "]" else ")"
    inner      = intercalate " | " (prettyPrintBranch <$> bs)
    repetition = if r then "..." else ""


isRepeatable :: Argument -> Boolean
isRepeatable (Option _ _ _ r) = r
isRepeatable (Positional _ r) = r
isRepeatable _                = false

hasDefault :: Argument -> Boolean
hasDefault (Option _ _ (Just (O.Argument _ (Just _))) _) = true
hasDefault _                                                 = false

takesArgument :: Argument -> Boolean
takesArgument (Option _ _ (Just _) _) = true
takesArgument _                       = false

isFlag :: Argument -> Boolean
isFlag (Option _ _ (Just (O.Argument _ (Just (BoolValue _)))) _) = true
isFlag _                                                             = false

isOption :: Argument -> Boolean
isOption (Option _ _ _ _) = true
isOption _                = false
