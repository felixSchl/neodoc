module Neodoc.ArgParser.Profile where

import Prelude
import Debug.Profile
import Neodoc.Parsing.Parser
import Neodoc.ArgParser.Type

profile :: ∀ a r. String -> (Unit -> ArgParser r a) -> ArgParser r a
profile msg f = Parser \a -> profileS msg \_-> unParser (f unit) a
