module Test.Support.Desc where

import Prelude
import Data.Maybe (Maybe(..))

import Language.Docopt.Value
import Language.Docopt.Parser.Desc

arg :: String -> Boolean -> Maybe Value -> OptionArgumentObj
arg = argument

opt :: Name -> Maybe OptionArgumentObj -> Desc
opt n a = OptionDesc { name:       n
                     , arg:        a
                     , env:        Nothing
                     , repeatable: false
                     }

lname :: String -> Name
lname = Long

sname :: Char -> Name
sname = Flag

fname :: Char -> String -> Name
fname = Full

argument :: String -> Boolean -> Maybe Value -> OptionArgumentObj
argument n o d = { name: n, default: d, optional: o }
