module Test.Support.Desc where

import Prelude
import Data.Maybe (Maybe(..))

import qualified Docopt.Parser.Desc as Desc

arg :: String -> Maybe String -> Desc.Argument
arg = Desc.argument

opt :: Desc.Name -> Maybe Desc.Argument -> Desc.Desc
opt n a = Desc.OptionDesc $ Desc.Option { name: n, arg: a }

lname :: String -> Desc.Name
lname = Desc.Long

sname :: Char -> Desc.Name
sname = Desc.Flag

fname :: Char -> String -> Desc.Name
fname = Desc.Full
