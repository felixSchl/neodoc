module Language.Docopt.ParserGen.ValueMapping (
  ValueMapping ()
  ) where

import Data.Tuple (Tuple())
import qualified Language.Docopt.Errors   as D
import qualified Language.Docopt.Value    as D
import qualified Language.Docopt.Argument as D

type ValueMapping = Tuple D.Argument D.Value
