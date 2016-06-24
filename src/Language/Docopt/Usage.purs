module Language.Docopt.Usage (
    Usage (..)
  , prettyPrintUsage
  ) where

import Prelude
import Data.List (List())
import Data.Foldable (intercalate)
import Language.Docopt.Argument (Branch, prettyPrintBranch)

type Usage = List Branch

prettyPrintUsage :: Usage -> String
prettyPrintUsage xs
  = intercalate " | " (prettyPrintBranch <$> xs)
