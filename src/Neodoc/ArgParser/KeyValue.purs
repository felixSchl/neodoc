module Neodoc.ArgParser.KeyValue where

import Data.Tuple (Tuple)
import Neodoc.ArgKey (ArgKey)
import Neodoc.Value.RichValue (RichValue)
import Neodoc.ArgParser.Arg

type KeyValue = Tuple Arg RichValue
