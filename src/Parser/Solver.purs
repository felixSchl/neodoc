-- |
-- | This module solves the usage specification into it's canonical form.
-- |

module Docopt.Parser.Solver where

import Prelude
import Data.Traversable (traverse)
import Data.Either
import Data.List (List(..), toList)
import Docopt.Parser.Usage

data UsageSpecification = UsageSpecification

instance showUsageSpecification :: Show UsageSpecification where
  show _ = "UsageSpecification"


-- | Canonicalize each usage and later the list of total usages.
solve :: List Usage -> Either String (List UsageSpecification)
solve usages = traverse solveEach usages
  where
    solveEach :: Usage -> Either String UsageSpecification
    solveEach usage = do
      pure $ UsageSpecification
