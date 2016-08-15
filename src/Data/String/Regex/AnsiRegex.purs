module Data.String.Regex.AnsiRegex (regex) where

import Prelude
import Data.String.Regex (Regex)

foreign import getRegex :: Unit -> Regex

regex :: Regex
regex = getRegex unit
