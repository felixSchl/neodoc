module Language.Docopt.Argument.Option (
    OptionObj ()
  , OptionArgument (..)
  , OptionArgumentObj ()
  , unOptionArgument
  , showOptionObj
  , eqOptionObj
  , hasDefault
  , isFlag
  , takesArgument
  , prettyPrintOption
  , prettyPrintOptionNaked
  ) where

import Prelude
import Data.Function (on)
import Data.Foldable (intercalate)
import Data.Tuple.Nested ((/\))
import Data.List (List(), (:), length, sort)
import Control.Apply ((*>))
import Data.Maybe (Maybe(..), maybe, isJust)
import Data.Generic (class Generic, gEq, gShow)
import Data.String (singleton) as String
import Data.NonEmpty (NonEmpty(..), fromNonEmpty)
import Data.NonEmpty as NonEmpty
import Language.Docopt.Value (Value(..), prettyPrintValue)
import Language.Docopt.OptionAlias 

type OptionObj =  { aliases    :: Aliases
                  , arg        :: Maybe OptionArgument
                  , env        :: Maybe String
                  , repeatable :: Boolean
                  }

newtype OptionArgument = OptionArgument { name     :: String
                                        , default  :: Maybe Value
                                        , optional :: Boolean
                                        }

unOptionArgument :: OptionArgument -> OptionArgumentObj
unOptionArgument (OptionArgument o) = o

instance showOptionArgument :: Show OptionArgument where
  show (OptionArgument arg) = "OptionArgument " <> showOptionArgumentObj arg

instance eqOptionArgument :: Eq OptionArgument where
  eq (OptionArgument a) (OptionArgument a') = eqOptionArgumentObj a a'

-- | The ord instance for option arguments uses the Tuple semantics for Ord
instance ordArgument :: Ord OptionArgument where
  compare (OptionArgument x) (OptionArgument x') = x `(compare `on` f)` x'
    where f x = x.optional /\ x.name /\ x.default

derive instance genericOptionArgument :: Generic OptionArgument

showOptionObj :: OptionObj -> String
showOptionObj o = "{ aliases: "    <> show o.aliases
               <> ", arg: "        <> show o.arg
               <> ", env: "        <> show o.env
               <> ", repeatable: " <> show o.repeatable
               <> "}"

eqOptionObj :: OptionObj -> OptionObj -> Boolean
eqOptionObj o o' = o.aliases    == o'.aliases
                && o.env        == o'.env
                && o.repeatable == o'.repeatable
                && o.arg        == o'.arg
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

hasDefault :: OptionObj -> Boolean
hasDefault { arg: Just (OptionArgument { default: Just _ }) } = true
hasDefault _                                                  = false

takesArgument :: OptionObj -> Boolean
takesArgument { arg: Just _ } = true
takesArgument _               = false

isFlag :: OptionObj -> Boolean
isFlag { arg: Just (OptionArgument { default: Just (BoolValue _) }) } = true
isFlag { arg: Nothing }                                               = true
isFlag _                                                              = false

prettyPrintOption :: OptionObj -> String
prettyPrintOption o = aliases <> arg' <> rep <> default <> env
  where
    prettyAliases = prettyPrintOptionAlias <$> (sort $ toAliasList o.aliases)
    aliases = intercalate "/" prettyAliases
    rep  = if o.repeatable then "..." else ""
    arg' = flip (maybe "") o.arg \(OptionArgument { name, optional }) ->
                (if optional then "[" else "")
                  <> "=" <> name
                  <> (if optional then "]" else "")
    default = flip (maybe "") o.arg \(OptionArgument { default }) ->
                flip (maybe "") default \v->
                  " [default: " <> (prettyPrintValue v) <>  "]"
    env = flip (maybe "") o.env \k -> " [env: " <> k <> "]"

prettyPrintOptionNaked :: OptionObj -> String
prettyPrintOptionNaked o =
     (if hasAlias then "(" else "")
  <> (aliases <> arg' <> rep)
  <> (if hasAlias then ")" else "")
  where
    hasAlias = length (NonEmpty.tail o.aliases) > 0
    prettyAliases = prettyPrintOptionAlias <$> (sort $ toAliasList o.aliases)
    aliases = intercalate "/" prettyAliases
    rep  = if o.repeatable then "..." else ""
    arg' = flip (maybe "") o.arg \(OptionArgument { name }) -> "="  <> name
