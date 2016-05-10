module Language.Docopt.Origin (
  Origin(..)
  ) where

import Prelude
import Data.Function (on)

data Origin
  = Argv
  | Environment
  | Default
  | Empty

weight :: Origin -> Int
weight Argv        = 3
weight Environment = 2
weight Default     = 1
weight Empty       = 0

instance showOrigin :: Show Origin where
  show Argv        = "Argv"
  show Environment = "Environment"
  show Default     = "Default"
  show Empty       = "Empty"

instance eqOrigin :: Eq Origin where
  eq Argv        Argv        = true
  eq Environment Environment = true
  eq Default     Default     = true
  eq Empty       Empty       = true
  eq _           _           = false

instance ordOrigin :: Ord Origin where
  compare = compare `on` weight
