module Test.Support.Desc where

import Prelude
import Data.Maybe (Maybe(..))
import Data.List (List(Nil), (:))

import Language.Docopt.Value
import Language.Docopt.SpecParser.Desc
import Language.Docopt.OptionAlias
import Language.Docopt.OptionAlias (OptionAlias(..)) as OptionAlias

arg :: String -> Boolean -> Maybe Value -> OptionArgumentObj
arg = argument

opt :: Aliases -> Maybe OptionArgumentObj -> Desc
opt as a = OptionDesc { aliases:    as
                      , arg:        a
                      , env:        Nothing
                      , repeatable: false
                      }

lname :: String -> Aliases
lname n = OptionAlias.Long n :| Nil

sname :: Char -> Aliases
sname f = OptionAlias.Short f :| Nil

fname :: Char -> String -> Aliases
fname f n = OptionAlias.Short f :| OptionAlias.Long n : Nil

argument :: String -> Boolean -> Maybe Value -> OptionArgumentObj
argument n o d = { name: n, default: d, optional: o }
