module Neodoc.ArgParser.Result where

import Prelude
import Data.List (List)
import Data.Pretty (pretty, class Pretty)
import Neodoc.Data.SolvedLayout (SolvedLayoutArg)
import Neodoc.Data.Layout (Branch)
import Neodoc.ArgParser.KeyValue (KeyValue)

data ArgParseResult = ArgParseResult (Branch SolvedLayoutArg) (List KeyValue)

instance showArgParseResult :: Show ArgParseResult where
  show (ArgParseResult branch vs) = "ArgParseResult " <> show branch <> " " <> show vs

instance prettyArgParseResult :: Pretty ArgParseResult where
  pretty (ArgParseResult branch vs) = pretty branch <> " => (" <> pretty vs <> ")"
