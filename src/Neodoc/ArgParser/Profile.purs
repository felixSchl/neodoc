module Neodoc.ArgParser.Profile where

import Prelude
import Neodoc.ArgParser.Type
import Debug.Profile

profile :: ∀ a r. String -> (Unit -> ArgParser r a) -> ArgParser r a
profile msg f = Parser \c s g i -> profileS msg \_-> unParser (f unit) c s g i
