module Test.Support.Docopt where

import Prelude
import Data.Maybe (Maybe(..))
import Language.Docopt.Value
import Language.Docopt.Argument

-- short hand for values
array = ArrayValue
str   = StringValue
bool  = BoolValue
int   = IntValue
