module Neodoc.Value.Origin (
    Origin(..)
  , weight
  ) where

import Prelude
import Data.Function (on)

data Origin
  = Argv
  | Environment
  | Default
  | Empty

weight :: Origin -> Int
weight Argv        = 30000
weight Environment = 20000
weight Default     = 10000
weight Empty       = 0

instance showOrigin :: Show Origin where
  show Argv        = "Argv"
  show Environment = "Environment"
  show Default     = "Default"
  show Empty       = "Empty"

derive instance eqOrigin :: Eq Origin
derive instance ordOrigin :: Ord Origin
