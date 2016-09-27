module Neodoc.Spec.Error where

import Neodoc.Error (NeodocError(..)) as Neodoc
import Neodoc.Error.Class (class ToNeodocError)
import Text.Parsing.Parser as P

newtype SpecParseError = SpecParseError P.ParseError

instance toNeodocErrorSpecParseError :: ToNeodocError SpecParseError where
  toNeodocError (SpecParseError e) = Neodoc.SpecParseError e

newtype SpecLexError = SpecLexError P.ParseError

instance toNeodocErrorSpecLexError :: ToNeodocError SpecLexError where
  toNeodocError (SpecLexError e) = Neodoc.SpecLexError e
