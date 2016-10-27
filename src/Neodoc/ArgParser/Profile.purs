module Neodoc.ArgParser.Profile where

import Prelude
import Neodoc.ArgParser.Type
import Debug.Profile

profile :: ∀ a r. String -> (Unit -> ArgParser r a) -> ArgParser r a
profile msg f = Parser \a -> profileS msg \_-> unParser (f unit) a
