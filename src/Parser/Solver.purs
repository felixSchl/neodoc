-- |
-- | This module solves the usage specification into it's canonical form.
-- |

module Docopt.Parser.Solver where

import Prelude
import Data.Either
import Data.List (List(..), toList)
import Docopt.Parser.Usage

data UsageSpecification = UsageSpecification

instance showUsageSpecification :: Show UsageSpecification where
  show _ = "UsageSpecification"

solve :: List Usage -> Either String (List UsageSpecification)
solve usage = do
  pure $ toList []
