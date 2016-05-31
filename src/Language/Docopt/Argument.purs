module Language.Docopt.Argument (
    Argument (..)
  , Branch ()
  , IsOptional ()
  , IsRepeatable ()
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

data Argument
  = Command     String IsRepeatable
  | Positional  String IsRepeatable
  | Option      OptionObj
  | Group       IsOptional (List Branch) IsRepeatable
  | EOA
  | Stdin

instance showArgument :: Show Argument where
  show (EOA)            = "EOA"
  show (Stdin)          = "Stdin"
  show (Command n r)    = "Command " <> show n
                              <> " " <> show r
  show (Positional n r) = "Positional " <> show n
                                 <> " " <> show r
  show (Group o bs r)   = "Group " <> show o
                            <> " " <> show bs
                            <> " " <> show r
  show (Option o)       = "Option " <> O.showOptionObj o

instance ordArgument :: Ord Argument where
  -- XXX: Implement a more efficient `compare` function
  compare = compare `on` show

instance eqArgument :: Eq Argument where
  eq (EOA)            (EOA)              = true
  eq (Stdin)          (Stdin)            = true
  eq (Command n r)    (Command n' r')    = (n == n') && (r == r')
  eq (Positional n r) (Positional n' r') = (n ^= n') && (r == r')
  eq (Group o bs r)   (Group o' bs' r')  = (o == o') && (bs == bs') && (r == r')
  eq (Option o)       (Option o')        = O.eqOptionObj o o'
  eq _                _                  = false

prettyPrintBranch :: Branch -> String
prettyPrintBranch xs = intercalate " " (prettyPrintArg <$> xs)

prettyPrintArg :: Argument -> String
prettyPrintArg (Stdin)          = "-"
prettyPrintArg (EOA)            = "--"
prettyPrintArg (Command name r) = name <> (if r then "..." else "")
prettyPrintArg (Positional n r) = name <> (if r then "..." else "")
  where
    name = if String.toUpper n == n then n else "<" <> n <> ">"
prettyPrintArg (Option o)          = O.prettyPrintOption o
prettyPrintArg (Group o bs r)      = open <> inner <> close <> repetition
  where
    open       = if o then "[" else "("
    close      = if o then "]" else ")"
    inner      = intercalate " | " (prettyPrintBranch <$> bs)
    repetition = if r then "..." else ""

prettyPrintBranchNaked :: Branch -> String
prettyPrintBranchNaked xs = intercalate " " (prettyPrintArgNaked <$> xs)

prettyPrintArgNaked :: Argument -> String
prettyPrintArgNaked (Stdin)             = "-"
prettyPrintArgNaked (EOA)               = "-- ARGS..."
prettyPrintArgNaked (Command name r)    = name <> (if r then "..." else "")
prettyPrintArgNaked (Positional name r) = name <> (if r then "..." else "")
prettyPrintArgNaked (Option o)          = O.prettyPrintOptionNaked o
prettyPrintArgNaked (Group o bs r)      = inner <> repetition
  where
    inner      = intercalate " | " (prettyPrintBranchNaked <$> bs)
    repetition = if r then "..." else ""

isRepeatable :: Argument -> Boolean
isRepeatable (Option o)       = o.repeatable
isRepeatable (Positional _ r) = r
isRepeatable (Command _ r)    = r
isRepeatable _                = false

setRepeatable :: Argument -> Boolean -> Argument
setRepeatable (Option o) r            = Option $ o { repeatable = r }
setRepeatable (Positional n _)      r = (Positional n r)
setRepeatable (Command n _)         r = (Command n r)
setRepeatable x                     _ = x

setRepeatableOr :: Argument -> Boolean -> Argument
setRepeatableOr (Option o) r
  = Option $ o { repeatable = o.repeatable || r }
setRepeatableOr (Positional n r) r' = (Positional n (r || r'))
setRepeatableOr (Command n r)    r' = (Command n (r || r'))
setRepeatableOr x                _  = x

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
isCommand (Command _ _) = true
isCommand _             = false

isPositional :: Argument -> Boolean
isPositional (Positional _ _) = true
isPositional _                = false

isOption :: Argument -> Boolean
isOption (Option _) = true
isOption _          = false

isFree :: Argument -> Boolean
isFree (Option _)     = true
isFree (Group _ bs _) = all (all isFree) bs
isFree _              = false
