module Language.Docopt.Argument (
    Argument (..)
  , Branch ()
  , CommandObj ()
  , PositionalObj ()
  , GroupObj ()
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
  , isGroup
  , isFlag
  , isPositional
  , isCommand
  , isFree
  , eqTypes
  , isOptional
  , module OptionReexport
  ) where

import Prelude
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple.Nested ((/\))
import Data.List (List(..), length, (:))
import Data.Foldable (intercalate, all)
import Data.Function (on)
import Data.String.Ext ((^=))
import Data.String as String
import Data.Function.Memoize
import Data.NonEmpty (NonEmpty(..), fromNonEmpty)
import Data.NonEmpty as NonEmpty
import Data.Lazy (defer)
import Data.Generic
import Partial.Unsafe

import Language.Docopt.Value (Value(..))
import Language.Docopt.OptionAlias (Aliases())
import Language.Docopt.Argument.Option as O
import Language.Docopt.Argument.Option (OptionObj, OptionArgument)
import Language.Docopt.Argument.Option hiding (hasDefault, isFlag, takesArgument
                                              ) as OptionReexport
import Language.Docopt.Env as Env
import Language.Docopt.Env (Env())

type Branch = List Argument

type CommandObj = { name       :: String
                  , repeatable :: Boolean
                  }

showCommandObj :: CommandObj -> String
showCommandObj x
  =  "{ name: "       <> x.name
  <> ", repeatable: " <> show x.repeatable
  <> "}"

type PositionalObj = { name       :: String
                     , repeatable :: Boolean
                     }

showPositionalObj :: PositionalObj -> String
showPositionalObj x
  =  "{ name: "       <> x.name
  <> ", repeatable: " <> show x.repeatable
  <> "}"

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

-- Note: We cannot use type aliases to build this ADT, unfortunately, as
-- deriving the generic instance won't work.
data Argument
  = Command     { name       :: String
                , repeatable :: Boolean
                }
  | Positional  { name       :: String
                , repeatable :: Boolean
                }
  | Option      { aliases    :: Aliases
                , arg        :: Maybe OptionArgument
                , env        :: Maybe String
                , repeatable :: Boolean
                }
  | Group       { optional   :: Boolean
                , branches   :: List Branch
                , repeatable :: Boolean
                }
  | EOA
  | Stdin

derive instance genericArgument :: Generic Argument

instance tabulateArgument :: Tabulate Argument where
  tabulate = unsafePartial gTabulate

instance showArgument :: Show Argument where
  show (EOA)          = "EOA"
  show (Stdin)        = "Stdin"
  show (Command c)    = "Command "    <> showCommandObj c
  show (Positional p) = "Positional " <> showPositionalObj p
  show (Group g)      = "Group "      <> showGroupObj g
  show (Option o)     = "Option " <> O.showOptionObj o

-- | The ord instance for arguments uses the Tuple semantics for Ord
instance ordArgument :: Ord Argument where
  compare (EOA)   (EOA)   = GT
  compare (Stdin) (Stdin) = GT
  compare (Command x) (Command x') = x `(compare `on` f)` x'
    where f x = x.repeatable /\ x.name
  compare (Positional x) (Positional x') = x `(compare `on` f)` x'
    where f x = x.repeatable /\ x.name
  compare (Option x) (Option x') = x `(compare `on` f)` x'
    where f x = x.repeatable /\ x.aliases /\ x.arg /\ x.env
  compare (Group x) (Group x') = x `(compare `on` f)` x'
    where f x = x.repeatable /\ x.optional /\ x.branches
  compare _ _ = LT

instance eqArgument :: Eq Argument where
  eq (EOA)            (EOA)             = true
  eq (Stdin)          (Stdin)           = true
  eq (Command x)      (Command x')      = (x.name == x'.name)
                                       && (x.repeatable == x'.repeatable)
  eq (Positional x)   (Positional x')   = (x.name ^= x'.name)
                                       && (x.repeatable == x'.repeatable)
  eq (Group g)        (Group g')        = (g.optional   == g'.optional)
                                       && (g.branches   == g'.branches)
                                       && (g.repeatable == g'.repeatable)
  eq (Option o)       (Option o')       = O.eqOptionObj o o'
  eq _                _                 = false

eqTypes :: Argument -> Argument -> Boolean
eqTypes (EOA)          (EOA)            = true
eqTypes (Stdin)        (Stdin)          = true
eqTypes (Command _)    (Command _)      = true
eqTypes (Positional _) (Positional _)   = true
eqTypes (Group _)      (Group _)        = true
eqTypes (Option _)     (Option _)       = true
eqTypes _              _                = false

isOptional :: Argument -> Boolean
isOptional (Group g) = g.optional
isOptional _         = false

prettyPrintBranch :: Branch -> String
prettyPrintBranch xs = intercalate " " (prettyPrintArg <$> xs)

prettyPrintArg :: Argument -> String
prettyPrintArg (Stdin)        = "-"
prettyPrintArg (EOA)          = "--"
prettyPrintArg (Command x)    = x.name <> (if x.repeatable then "..." else "")
prettyPrintArg (Positional x) = x.name <> (if x.repeatable then "..." else "")
prettyPrintArg (Option o)     = O.prettyPrintOption o
prettyPrintArg (Group g)      = open <> inner <> close <> repetition
  where
    open       = if g.optional then "[" else "("
    close      = if g.optional then "]" else ")"
    inner      = intercalate " | " (prettyPrintBranch <$> g.branches)
    repetition = if g.repeatable then "..." else ""

prettyPrintBranchNaked :: Branch -> String
prettyPrintBranchNaked xs = intercalate " " (prettyPrintArgNaked <$> xs)

prettyPrintArgNaked :: Argument -> String
prettyPrintArgNaked (Stdin)        = "-"
prettyPrintArgNaked (EOA)          = "-- ARGS..."
prettyPrintArgNaked (Command x)    = x.name <> (if x.repeatable then "..." else "")
prettyPrintArgNaked (Positional x) = x.name <> (if x.repeatable then "..." else "")
prettyPrintArgNaked (Option o)     = O.prettyPrintOptionNaked o
prettyPrintArgNaked (Group g)      = open <> inner <> close <> repetition
  where
    showParens = g.optional ||
                  case g.branches of
                    x:Nil -> length x > 1
                    xs    -> true
    open       = if showParens then (if g.optional then "[" else "(") else ""
    close      = if showParens then (if g.optional then "]" else ")") else ""
    inner      = intercalate " | " (prettyPrintBranchNaked <$> g.branches)
    repetition = if g.repeatable then "..." else ""

isRepeatable :: Argument -> Boolean
isRepeatable (Option x)     = x.repeatable
isRepeatable (Positional x) = x.repeatable
isRepeatable (Command x)    = x.repeatable
isRepeatable (Group x)      = x.repeatable
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
setRequired (Group x) o = Group x { optional = not o }
setRequired x         _ = x

hasDefault :: Argument -> Boolean
hasDefault (Option o) = O.hasDefault o
hasDefault _          = false

takesArgument :: Argument -> Boolean
takesArgument (Option o) = O.takesArgument o
takesArgument _          = false

getArgument :: Argument -> Maybe O.OptionArgumentObj
getArgument (Option o) = O.unOptionArgument <$> o.arg
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

isGroup :: Argument -> Boolean
isGroup (Group _) = true
isGroup _         = false

isOption :: Argument -> Boolean
isOption (Option _) = true
isOption _          = false

isFree :: Argument -> Boolean
isFree (Option _)           = true
isFree (Group { branches }) = all (all isFree) branches
isFree _                    = false
