module Neodoc.Error where

import Prelude
import Data.Pretty (class Pretty, pretty)
import Text.Parsing.Parser as P

data NeodocError
  = ScanError String
  | SpecLexError String
  | SpecParseError String
  | SpecSolveError String
  | GenericError String

instance showNeodocError :: Show NeodocError where
  show (ScanError e) = "ScanError " <> show e
  show (SpecLexError e) = "SpecLexError " <> show e
  show (SpecParseError e) = "SpecParseError " <> show e
  show (SpecSolveError e) = "SpecSolveError " <> show e
  show (GenericError e) = "GenericError " <> show e

instance prettyNeodocError :: Pretty NeodocError where
  pretty (ScanError m) = "invalid neodoc document: " <> m
  pretty (SpecLexError m) = "failed to lex section: " <> m
  pretty (SpecParseError m) = "failed to parse section: " <> m
  pretty (SpecSolveError e) = "failed to solve spec: " <> e
  pretty (GenericError e) = e
