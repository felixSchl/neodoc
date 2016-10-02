module Neodoc.Error where

import Prelude
import Data.Maybe (Maybe (..))
import Data.Char as Char
import Data.Pretty (class Pretty, pretty)
import Data.String as String
import Text.Parsing.Parser as P

data NeodocError
  = ScanError String
  | SpecLexError String
  | SpecParseError String
  | SpecSolveError String
  | ArgParserError String
  | VersionMissingError

isDeveloperError :: NeodocError -> Boolean
isDeveloperError (ArgParserError _) = false
isDeveloperError _ = true

instance showNeodocError :: Show NeodocError where
  show (ScanError e) = "ScanError " <> show e
  show (SpecLexError e) = "SpecLexError " <> show e
  show (SpecParseError e) = "SpecParseError " <> show e
  show (SpecSolveError e) = "SpecSolveError " <> show e
  show (ArgParserError e) = "ArgParserError " <> " " <> show e
  show (VersionMissingError) = "VersionMissingError"

instance prettyNeodocError :: Pretty NeodocError where
  pretty (ScanError msg) = "Failed to disect neodoc text:\n" <> msg
  pretty (SpecLexError msg) = "Failed to lex specification:\n" <> msg
  pretty (SpecParseError msg) = "Failed to parse specification:\n" <> msg
  pretty (SpecSolveError msg) = "Incoherent specification:\n" <> msg
  pretty (VersionMissingError) = "Package version could not be detected"
  pretty (ArgParserError msg) = msg
