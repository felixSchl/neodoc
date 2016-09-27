module Neodoc.Error where

import Prelude
import Data.Pretty (class Pretty, pretty)
import Text.Parsing.Parser as P

data NeodocError
  = ScanError P.ParseError
  | SpecLexError P.ParseError
  | SpecParseError P.ParseError
  | SpecSolveError String

instance showNeodocError :: Show NeodocError where
  show (ScanError e) = "ScanError " <> show e
  show (SpecLexError e) = "SpecLexError " <> show e
  show (SpecParseError e) = "SpecParseError " <> show e
  show (SpecSolveError e) = "SpecSolveError " <> e

instance prettyNeodocError :: Pretty NeodocError where
  pretty (ScanError (P.ParseError m _ _)) = "invalid neodoc document: " <> m
  pretty (SpecLexError (P.ParseError m _ _)) = "failed to lex section: " <> m
  pretty (SpecParseError (P.ParseError m _ _)) = "failed to parse section: " <> m
  pretty (SpecSolveError e) = "failed to solve spec: " <> e
