module Language.Docopt.Parser.Usage.Argument (
    Argument (..)
  , IsRepeatable ()
  , IsOptional ()
  , Branch ()
  , CommandObj ()
  , PositionalObj ()
  , isFree
  , isOption
  , isPositional
  , isCommand
  , prettyPrintArg
  , prettyPrintBranch
  , module O
  ) where

import Prelude
import Data.List (List())
import Data.Foldable (intercalate, all)
import Language.Docopt.Parser.Usage.Option as O

type CommandObj = { name       :: String
                  , repeatable :: Boolean
                  }

showCommandObj :: CommandObj -> String
showCommandObj x
  =  "{ name: "       <> x.name
  <> ", repeatable: " <> show x.repeatable
  <> "}"

eqCommandObj :: CommandObj -> CommandObj -> Boolean
eqCommandObj x x' = x.name       == x'.name
                 && x.repeatable == x'.repeatable

type PositionalObj = { name       :: String
                     , repeatable :: Boolean
                     }

showPositionalObj :: PositionalObj  -> String
showPositionalObj x
  =  "{ name: "       <> x.name
  <> ", repeatable: " <> show x.repeatable
  <> "}"

eqPositionalObj :: PositionalObj -> PositionalObj -> Boolean
eqPositionalObj x x' = x.name       == x'.name
                    && x.repeatable == x'.repeatable

type IsRepeatable = Boolean
type IsOptional = Boolean
type Branch = List Argument
data Argument
  = Command     CommandObj
  | Positional  PositionalObj
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
isPositional (Positional _) = true
isPositional _              = false

isCommand :: Argument -> Boolean
isCommand (Command _) = true
isCommand _           = false

instance showArgument :: Show Argument where
  show (EOA)            = "--"
  show (Stdin)          = "-"
  show (Reference r)    = "Reference "   <> r
  show (Command cmd)    = "Command "     <> showCommandObj cmd
  show (Positional pos) = "Positional "  <> showPositionalObj pos
  show (Option o)       = "Option "      <> O.showLOptObj o
  show (OptionStack o)  = "OptionStack " <> O.showSOptObj o
  show (Group n b o)    = "Group "       <> show n <> " " <> show b <> " " <> show o

instance eqArgument :: Eq Argument where
  eq (Stdin)          (Stdin)            = true
  eq (EOA)            (EOA)              = true
  eq (Command cmd)    (Command cmd')     = cmd `eqCommandObj` cmd'
  eq (Positional pos) (Positional pos')  = pos `eqPositionalObj` pos'
  eq (Option o)       (Option o')        = o `O.eqLOptObj` o'
  eq (Group b xs r)   (Group b' xs' r')  = (b == b') && (xs == xs') && (r == r')
  eq (OptionStack o)  (OptionStack o')   = o `O.eqSOptObj` o'
  eq (Reference r)    (Reference r')     = r == r'
  eq _                _                  = false

prettyPrintBranch :: Branch -> String
prettyPrintBranch xs = intercalate " " (prettyPrintArg <$> xs)

prettyPrintArg :: Argument -> String
prettyPrintArg (Command cmd)    = cmd.name <> if cmd.repeatable then "..." else ""
prettyPrintArg (Positional pos) = pos.name <> if pos.repeatable then "..." else ""
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
