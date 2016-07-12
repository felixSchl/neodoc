module Language.Docopt.SpecParser.Usage.Usage (
    Usage (..)
  , prettyPrintUsage
  ) where

import Prelude
import Data.List (List())
import Data.Foldable (intercalate)

import Language.Docopt.SpecParser.Usage.Argument as U
type Usage = List U.Branch

prettyPrintUsage :: Usage -> String
prettyPrintUsage bs = intercalate " | " (U.prettyPrintBranch <$> bs)
