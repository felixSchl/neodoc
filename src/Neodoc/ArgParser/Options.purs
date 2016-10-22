module Neodoc.ArgParser.Options (
  Options()
) where

import Data.List (List)
import Neodoc.OptionAlias (OptionAlias)

-- | The options for generating a parser
type Options r = {
  optionsFirst      :: Boolean
, stopAt            :: Array String
, requireFlags      :: Boolean
, laxPlacement      :: Boolean
, repeatableOptions :: Boolean
, helpFlags         :: List OptionAlias
, versionFlags      :: List OptionAlias
  | r
}

