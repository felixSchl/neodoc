module Language.Docopt.SpecParser.Usage.Argument (
    Argument (..)
  , IsRepeatable ()
  , IsOptional ()
  , Branch ()
  , GroupObj ()
  , CommandObj ()
  , PositionalObj ()
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
import Language.Docopt.SpecParser.Usage.Option as O

-- A command argument is literal that must be matched exactly.
-- It's value, if present must always be a boolean 'true' or any positive
-- integer in case the command repeats.
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

-- A positional argument, e.g. '<foo-bar>' or w/o angles 'FOO-BAR'.
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

-- A group of arguments. An explicitly grouped set of argument branches,
-- where each branch is mutually exclusive from the branches around it,
-- separated by a vertical bar '|'. Within each branch, a list of arguments
-- is present (this is the recursive branch of the `Argument` ADT).
type GroupObj = { optional   :: Boolean
                , branches   :: List Branch
                , repeatable :: Boolean
                }

showGroupObj :: GroupObj -> String
showGroupObj x
  =  "{ optional: "   <> show x.optional
  <> ", branches: "   <> show x.branches
  <> ", repeatable: " <> show x.repeatable
  <> "}"

eqGroupObj :: GroupObj -> GroupObj -> Boolean
eqGroupObj x x' = x.optional  == x'.optional
              && x.branches   == x'.branches
              && x.repeatable == x'.repeatable

type IsRepeatable = Boolean
type IsOptional = Boolean
type Branch = List Argument
data Argument
  = Command     CommandObj
  | Positional  PositionalObj
  | Option      O.LOptObj
  | OptionStack O.SOptObj
  | Group       GroupObj
  | EOA
  | Stdin
  | Reference String

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
  show (Group grp)      = "Group "       <> showGroupObj grp

instance eqArgument :: Eq Argument where
  eq (Stdin)          (Stdin)            = true
  eq (EOA)            (EOA)              = true
  eq (Command cmd)    (Command cmd')     = cmd `eqCommandObj` cmd'
  eq (Positional pos) (Positional pos')  = pos `eqPositionalObj` pos'
  eq (Option o)       (Option o')        = o `O.eqLOptObj` o'
  eq (Group grp)      (Group grp')       = grp `eqGroupObj` grp'
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
prettyPrintArg (Group grp)
  =  (if grp.optional then "[" else "(")
  <> (intercalate " | " (prettyPrintBranch <$> grp.branches))
  <> (if grp.optional then "]" else ")")
  <> (if grp.repeatable then "..." else "")
