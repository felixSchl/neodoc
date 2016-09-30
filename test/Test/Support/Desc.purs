module Test.Support.Desc where

import Prelude
import Data.Maybe (Maybe(..))
import Data.List (List(Nil), (:))

import Neodoc.Value
import Neodoc.OptionAlias
import Neodoc.OptionAlias as OptionAlias
import Neodoc.Data.Description
import Neodoc.Data.OptionArgument

arg :: String -> Boolean -> OptionArgument
arg = argument

opt :: Aliases -> Maybe OptionArgument -> Maybe Value -> Description
opt as mArg mDef = OptionDescription as false mArg mDef Nothing

lname :: String -> Aliases
lname n = OptionAlias.Long n :| Nil

sname :: Char -> Aliases
sname f = OptionAlias.Short f :| Nil

fname :: Char -> String -> Aliases
fname f n = OptionAlias.Short f :| OptionAlias.Long n : Nil

argument :: String -> Boolean -> OptionArgument
argument = OptionArgument
