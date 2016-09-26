module Neodoc.Spec.Parser where

import Neodoc.Spec.Parser.Usage as Usage
import Neodoc.Spec.Parser.Description as Description

parseUsage = Usage.parse
parseDescription = Description.parse
