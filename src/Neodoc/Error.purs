module Neodoc.Error where

import Prelude
import Data.Generic
import Data.Maybe (Maybe (..))
import Data.Char as Char
import Data.Pretty (class Pretty, pretty)
import Data.String as String

data NeodocError
  = ScanError String
  | SpecLexError String
  | SpecParseError String
  | SpecSolveError String
  | ArgParserError String
  | VersionMissingError

derive instance genericNeodocError :: Generic NeodocError

isDeveloperError :: NeodocError -> Boolean
isDeveloperError (ArgParserError _) = false
isDeveloperError _ = true

instance showNeodocError :: Show NeodocError where
  show = gShow

instance prettyNeodocError :: Pretty NeodocError where
  pretty (ScanError msg) = "Failed to disect neodoc text:\n" <> msg
  pretty (SpecLexError msg) = "Failed to lex specification:\n" <> msg
  pretty (SpecParseError msg) = "Failed to parse specification:\n" <> msg
  pretty (SpecSolveError msg) = "Incoherent specification:\n" <> msg
  pretty (VersionMissingError) = "Package version could not be detected"
  pretty (ArgParserError msg) = msg
