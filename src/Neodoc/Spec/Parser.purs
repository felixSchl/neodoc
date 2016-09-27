module Neodoc.Spec.Parser where

import Prelude
import Data.Bifunctor (lmap)
import Neodoc.Spec.Parser.Usage as Usage
import Neodoc.Spec.Parser.Description as Description
import Neodoc.Spec.Error

parseUsage = lmap SpecParseError <<< Usage.parse
parseDescription = lmap SpecParseError <<< Description.parse
