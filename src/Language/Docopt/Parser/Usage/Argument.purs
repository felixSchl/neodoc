module Language.Docopt.Parser.Usage.Argument (
    Argument (..)
  , IsRepeatable ()
  , IsOptional ()
  , Branch ()
  , isFree
  , isOption
  , isPositional
  , isCommand
  , prettyPrintArg
  , prettyPrintBranch
  ) where

import Prelude
import Data.List (List())
import Data.Foldable (intercalate, all)
import Language.Docopt.Parser.Usage.Option as O

type IsRepeatable = Boolean
type IsOptional = Boolean
type Branch = List Argument
data Argument
  = Command     String IsRepeatable
  | Positional  String IsRepeatable
  | Option      O.LOptObj
  | OptionStack O.SOptObj
  | Group       IsOptional (List Branch) IsRepeatable
  | EOA
  | Stdin
  | Reference String

isFree :: Argument -> Boolean
isFree (Option _)     = true
isFree (Group _ bs _) = all (all isFree) bs
isFree _              = false

isOption :: Argument -> Boolean
isOption (Option _)      = true
isOption (OptionStack _) = true
isOption _               = false

isPositional :: Argument -> Boolean
isPositional (Positional _ _) = true
isPositional _                = false

isCommand :: Argument -> Boolean
isCommand (Command _ _) = true
isCommand _             = false

instance showArgument :: Show Argument where
  show (EOA)            = "--"
  show (Stdin)          = "-"
  show (Reference r)    = "Reference "   <> r
  show (Command n r)    = "Command "     <> n <> show r
  show (Positional n r) = "Positional "  <> n <> " " <> show r
  show (Option o)       = "Option "      <> O.showLOptObj o
  show (OptionStack o)  = "OptionStack " <> O.showSOptObj o
  show (Group n b o)    = "Group "       <> show n <> " " <> show b <> " " <> show o

instance eqArgument :: Eq Argument where
  eq (Stdin)          (Stdin)            = true
  eq (EOA)            (EOA)              = true
  eq (Command s r)    (Command s' r')    = (s == s') && (r == r')
  eq (Positional s r) (Positional s' r') = (s == s') && (r == r')
  eq (Option o)       (Option o')        = o `O.eqLOptObj` o'
  eq (Group b xs r)   (Group b' xs' r')  = (b == b') && (xs == xs') && (r == r')
  eq (OptionStack o)  (OptionStack o')   = o `O.eqSOptObj` o'
  eq (Reference r)    (Reference r')     = r == r'
  eq _                _                  = false

prettyPrintBranch :: Branch -> String
prettyPrintBranch xs = intercalate " " (prettyPrintArg <$> xs)

prettyPrintArg :: Argument -> String
prettyPrintArg (Command n r)    = n <> if r then "..." else ""
prettyPrintArg (Positional n r) = n <> if r then "..." else ""
prettyPrintArg (Option o)       = O.prettyPrintLOptObj o
prettyPrintArg (OptionStack o)  = O.prettyPrintSOptObj o
prettyPrintArg (EOA)            = "--"
prettyPrintArg (Stdin)          = "-"
prettyPrintArg (Reference r)    = "[" <> show r <> " options...]"
prettyPrintArg (Group b xs r)
  =  (if b then "[" else "(")
  <> (intercalate " | " (prettyPrintBranch <$> xs))
  <> (if b then "]" else ")")
  <> (if r then "..." else "")
