module Neodoc.Spec.Parser where

import Prelude
import Neodoc.Spec.Parser.Usage as Usage
import Neodoc.Spec.Parser.Description as Description
import Neodoc.Spec.Error
import Text.Parsing.Parser (parseErrorMessage) as P

parseUsage = Usage.parse
parseDescription = Description.parse
