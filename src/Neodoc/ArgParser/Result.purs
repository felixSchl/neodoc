module Neodoc.ArgParser.Result where

import Prelude
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Pretty (pretty, class Pretty)
import Neodoc.Data.SolvedLayout (SolvedLayoutArg)
import Neodoc.Data.Layout (Branch)
import Neodoc.ArgParser.KeyValue (KeyValue)

data ArgParseResult = ArgParseResult (Maybe (Branch SolvedLayoutArg)) (List KeyValue)

getResult :: ArgParseResult -> List KeyValue
getResult (ArgParseResult _ r) = r

instance showArgParseResult :: Show ArgParseResult where
  show (ArgParseResult branch vs) = "ArgParseResult " <> show branch <> " " <> show vs

instance prettyArgParseResult :: Pretty ArgParseResult where
  pretty (ArgParseResult Nothing _) = "<no output>"
  pretty (ArgParseResult (Just branch) vs) = pretty branch <> " => (" <> pretty vs <> ")"
