module Neodoc.ArgParser.KeyValue where

import Data.Tuple (Tuple)
import Neodoc.ArgKey (ArgKey)
import Neodoc.Value.RichValue (RichValue)

type KeyValue = Tuple ArgKey RichValue
