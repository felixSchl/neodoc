module Neodoc.Spec.Parser where

import Prelude
import Data.Bifunctor (lmap)
import Neodoc.Spec.Parser.Usage as Usage
import Neodoc.Spec.Parser.Description as Description
import Neodoc.Spec.Error
import Text.Parsing.Parser (ParseError(..)) as P

parseUsage input = toSpecParseError $ Usage.parse input
parseDescription toks = toSpecParseError $ Description.parse toks

toSpecParseError = lmap (SpecParseError <<< getParseErrorMessage)

getParseErrorMessage :: P.ParseError -> String
getParseErrorMessage (P.ParseError m _ _) = m
