module Test.Support.Docopt where

import Prelude
import Data.Maybe (Maybe(..))

import Docopt

-- short hand to create a command
co :: String -> Argument
co = Command

-- short hand to create a positional argument
po :: String -> Boolean -> Argument
po = Positional

-- short hand to an option argument
opt :: (Maybe Flag)
    -> (Maybe Name)
    -> (Maybe OptionArgument)
    -> IsRepeatable
    -> Argument
opt = Option
