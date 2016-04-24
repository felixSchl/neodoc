module Language.Docopt.ParserGen.ValueMapping (
  ValueMapping ()
  ) where

import Data.Tuple (Tuple())
import Language.Docopt.Value (Value)
import Language.Docopt.Argument (Argument)

type ValueMapping = Tuple Argument Value
