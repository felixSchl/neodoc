module Language.Docopt.Argument (
    Argument (..)
  , Branch ()
  , IsOptional ()
  , IsRepeatable ()
  , CommandObj ()
  , PositionalObj ()
  , prettyPrintBranch
  , prettyPrintArg
  , prettyPrintArgNaked
  , isRepeatable
  , setRepeatable
  , setRequired
  , setRepeatableOr
  , hasDefault
  , getEnvKey
  , hasEnvBacking
  , takesArgument
  , getArgument
  , isOption
  , isFlag
  , isCommand
  , isFree
  ) where

import Prelude
import Data.Maybe (Maybe(..), maybe)
import Data.List (List())
import Data.Foldable (intercalate, all)
import Data.Function (on)
import Data.String.Ext ((^=))
import Data.String as String

import Language.Docopt.Argument.Option as O
import Language.Docopt.Argument.Option (OptionObj)
import Language.Docopt.Env as Env
import Language.Docopt.Env (Env())

type IsRepeatable = Boolean
type IsOptional = Boolean

type Branch = List Argument

type CommandObj = { name :: String
                  , repeatable :: Boolean
                  }

showCommandObj :: CommandObj -> String
showCommandObj x
  =  "{ name: " <> x.name
  <> ", isRepeatable: " <> show x.repeatable
  <> "}"

type PositionalObj = { name :: String
                     , repeatable :: Boolean
                     }

showPositionalObj :: CommandObj -> String
showPositionalObj x
  =  "{ name: " <> x.name
  <> ", isRepeatable: " <> show x.repeatable
  <> "}"

data Argument
  = Command     CommandObj
  | Positional  PositionalObj
  | Option      OptionObj
  | Group       IsOptional (List Branch) IsRepeatable
  | EOA
  | Stdin

instance showArgument :: Show Argument where
  show (EOA)          = "EOA"
  show (Stdin)        = "Stdin"
  show (Command c)    = "Command "    <> showCommandObj c
  show (Positional p) = "Positional " <> showPositionalObj p
  show (Group o bs r) = "Group " <> show o
                          <> " " <> show bs
                          <> " " <> show r
  show (Option o)     = "Option " <> O.showOptionObj o

instance ordArgument :: Ord Argument where
  -- XXX: Implement a more efficient `compare` function
  compare = compare `on` show

instance eqArgument :: Eq Argument where
  eq (EOA)            (EOA)             = true
  eq (Stdin)          (Stdin)           = true
  eq (Command x)      (Command x')      = (x.name == x'.name)
                                       && (x.repeatable == x'.repeatable)
  eq (Positional x)   (Positional x')   = (x.name ^= x'.name)
                                       && (x.repeatable == x'.repeatable)
  eq (Group o bs r)   (Group o' bs' r') = (o == o') && (bs == bs') && (r == r')
  eq (Option o)       (Option o')       = O.eqOptionObj o o'
  eq _                _                 = false

prettyPrintBranch :: Branch -> String
prettyPrintBranch xs = intercalate " " (prettyPrintArg <$> xs)

prettyPrintArg :: Argument -> String
prettyPrintArg (Stdin)        = "-"
prettyPrintArg (EOA)          = "--"
prettyPrintArg (Command x)    = x.name <> (if x.repeatable then "..." else "")
prettyPrintArg (Positional x) = name   <> (if x.repeatable then "..." else "")
  where
    -- TODO: Capture the real name and avoid this hack!!
    name = if String.toUpper x.name == x.name
              then x.name
              else "<" <> x.name <> ">"
prettyPrintArg (Option o)     = O.prettyPrintOption o
prettyPrintArg (Group o bs r) = open <> inner <> close <> repetition
  where
    open       = if o then "[" else "("
    close      = if o then "]" else ")"
    inner      = intercalate " | " (prettyPrintBranch <$> bs)
    repetition = if r then "..." else ""

prettyPrintBranchNaked :: Branch -> String
prettyPrintBranchNaked xs = intercalate " " (prettyPrintArgNaked <$> xs)

prettyPrintArgNaked :: Argument -> String
prettyPrintArgNaked (Stdin)        = "-"
prettyPrintArgNaked (EOA)          = "-- ARGS..."
prettyPrintArgNaked (Command x)    = x.name <> (if x.repeatable then "..." else "")
prettyPrintArgNaked (Positional x) = x.name <> (if x.repeatable then "..." else "")
prettyPrintArgNaked (Option o)     = O.prettyPrintOptionNaked o
prettyPrintArgNaked (Group o bs r) = inner <> repetition
  where
    inner      = intercalate " | " (prettyPrintBranchNaked <$> bs)
    repetition = if r then "..." else ""

isRepeatable :: Argument -> Boolean
isRepeatable (Option x)     = x.repeatable
isRepeatable (Positional x) = x.repeatable
isRepeatable (Command x)    = x.repeatable
isRepeatable _              = false

setRepeatable :: Argument -> Boolean -> Argument
setRepeatable (Option x)     r = Option     x { repeatable = r }
setRepeatable (Positional x) r = Positional x { repeatable = r }
setRepeatable (Command x)    r = Command    x { repeatable = r }
setRepeatable x              _ = x

setRepeatableOr :: Argument -> Boolean -> Argument
setRepeatableOr (Option x)     r  = Option     x { repeatable = x.repeatable || r }
setRepeatableOr (Positional x) r  = Positional x { repeatable = x.repeatable || r }
setRepeatableOr (Command x)    r  = Command    x { repeatable = x.repeatable || r }
setRepeatableOr x              _  = x

setRequired :: Argument -> Boolean -> Argument
setRequired (Group _ bs r) o = Group (not o) bs r
setRequired x              _ = x

hasDefault :: Argument -> Boolean
hasDefault (Option o) = O.hasDefault o
hasDefault _          = false

takesArgument :: Argument -> Boolean
takesArgument (Option o) = O.takesArgument o
takesArgument _          = false

getArgument :: Argument -> Maybe O.OptionArgumentObj
getArgument (Option o) = o.arg
getArgument _          = Nothing

getEnvKey :: Argument -> Maybe String
getEnvKey (Option o) = o.env
getEnvKey _          = Nothing

hasEnvBacking :: Argument -> Env -> Boolean
hasEnvBacking p env = maybe false id $ flip Env.member env <$> getEnvKey p

isFlag :: Argument -> Boolean
isFlag (Option o) = O.isFlag o
isFlag _          = false

isCommand :: Argument -> Boolean
isCommand (Command _) = true
isCommand _           = false

isPositional :: Argument -> Boolean
isPositional (Positional _) = true
isPositional _              = false

isOption :: Argument -> Boolean
isOption (Option _) = true
isOption _          = false

isFree :: Argument -> Boolean
isFree (Option _)     = true
isFree (Group _ bs _) = all (all isFree) bs
isFree _              = false
