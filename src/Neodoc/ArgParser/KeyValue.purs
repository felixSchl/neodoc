module Neodoc.ArgParser.KeyValue where

import Data.Tuple (Tuple)
import Data.Lazy (Lazy)
import Neodoc.ArgKey (ArgKey)
import Neodoc.Value.RichValue (RichValue)
import Neodoc.ArgParser.Arg

type KeyValue = Tuple Arg RichValue
type LazyKeyValue = Tuple Arg (Lazy RichValue)
