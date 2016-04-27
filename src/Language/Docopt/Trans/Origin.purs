module Language.Docopt.Trans.Origin (
  Origin(..)
  ) where

import Prelude

data Origin
  = Argv
  | Environment
  | Default

weight :: Origin -> Int
weight Argv        = 3
weight Environment = 2
weight Default     = 1

instance eqOrigin :: Eq Origin where
  eq Argv        Argv        = true
  eq Environment Environment = true
  eq Default     Default     = true
  eq _           _           = false

instance ordOrigin :: Ord Origin where
  compare a b = compare (weight a) (weight b)


