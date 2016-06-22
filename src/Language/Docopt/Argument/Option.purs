module Language.Docopt.Argument.Option (
    OptionObj ()
  , OptionArgumentObj ()
  , showOptionObj
  , eqOptionObj
  , hasDefault
  , isFlag
  , takesArgument
  , prettyPrintOption
  , prettyPrintOptionNaked
  , empty
  ) where

import Prelude
import Data.Function (on)
import Control.Apply ((*>))
import Data.Maybe (Maybe(..), maybe)
import Data.Generic (class Generic, gEq, gShow)
import Data.String (singleton) as String

import Language.Docopt.Value (Value(..), prettyPrintValue)

type OptionObj =  { flag       :: Maybe Char
                  , name       :: Maybe String
                  , arg        :: Maybe OptionArgumentObj
                  , env        :: Maybe String
                  , repeatable :: Boolean
                  }

showOptionObj :: OptionObj -> String
showOptionObj o = "{ flag: "       <> show o.flag
               <> ", name: "       <> show o.name
               <> ", arg: "        <> showArg o.arg
               <> ", env: "        <> show o.env
               <> ", repeatable: " <> show o.repeatable
               <> "}"
  where
    showArg (Just a) = showOptionArgumentObj a
    showArg Nothing  = "Nothing"

eqOptionObj :: OptionObj -> OptionObj -> Boolean
eqOptionObj o o' = o.flag       == o'.flag
                && o.name       == o'.name
                && o.env        == o'.env
                && o.repeatable == o'.repeatable
                && argsEqual o.arg o'.arg
  where
    argsEqual (Just a) (Just a') = eqOptionArgumentObj a a'
    argsEqual Nothing Nothing    = true
    argsEqual _ _                = false

type OptionArgumentObj = { name     :: String
                         , default  :: Maybe Value
                         , optional :: Boolean
                         }

showOptionArgumentObj :: OptionArgumentObj -> String
showOptionArgumentObj a = "{ name: "     <> show a.name
                       <> ", default: "  <> show a.default
                       <> ", optional: " <> show a.optional
                       <> "}"

eqOptionArgumentObj :: OptionArgumentObj -> OptionArgumentObj -> Boolean
eqOptionArgumentObj a a' = a.name     == a'.name
                        && a.default  == a'.default
                        && a.optional == a'.optional

empty :: OptionObj
empty = { flag:       Nothing
        , name:       Nothing
        , arg:        Nothing
        , env:        Nothing
        , repeatable: false
        }

hasDefault :: OptionObj -> Boolean
hasDefault { arg: Just { default: Just _ } } = true
hasDefault _                                 = false

takesArgument :: OptionObj -> Boolean
takesArgument { arg: Just _ } = true
takesArgument _               = false

isFlag :: OptionObj -> Boolean
isFlag { arg: Just { default: Just (BoolValue _)}} = true
isFlag { arg: Nothing }                            = true
isFlag _                                           = false

prettyPrintOption :: OptionObj -> String
prettyPrintOption o
  = short <> long <> arg' <> rep <> default <> env
  where
    short = maybe "" (\f -> "-" <> (String.singleton f)) o.flag
    long  = maybe "" (const ", ") (o.flag *> o.name)
              <> maybe "" ("--" <> _) o.name
    rep  = if o.repeatable then "..." else ""
    arg' = flip (maybe "") o.arg \({ name, optional }) ->
                (if optional then "[" else "")
                  <> "=" <> name
                  <> (if optional then "]" else "")
    default = flip (maybe "") o.arg \({ default }) ->
                flip (maybe "") default \v->
                  " [default: " <> (prettyPrintValue v) <>  "]"
    env = flip (maybe "") o.env \k -> " [env: " <> k <> "]"

prettyPrintOptionNaked :: OptionObj -> String
prettyPrintOptionNaked o
  = short <> long <> arg' <> rep
  where
    short = maybe "" (\f -> "-" <> (String.singleton f)) o.flag
    long  = maybe "" (const "|") (o.flag *> o.name)
              <> maybe "" ("--" <> _) o.name
    rep  = if o.repeatable then "..." else ""
    arg' = flip (maybe "") o.arg \({ name }) -> "="  <> name
