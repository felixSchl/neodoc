module Neodoc.Spec.Error where

import Neodoc.Error (NeodocError(..)) as Neodoc
import Neodoc.Error.Class (class ToNeodocError)
import Text.Parsing.Parser as P

newtype SpecParseError = SpecParseError P.ParseError

instance toNeodocErrorSpecParseError :: ToNeodocError SpecParseError where
  toNeodocError (SpecParseError (P.ParseError m _ _)) = Neodoc.SpecParseError m

newtype SpecLexError = SpecLexError P.ParseError

instance toNeodocErrorSpecLexError :: ToNeodocError SpecLexError where
  toNeodocError (SpecLexError (P.ParseError m _ _)) = Neodoc.SpecLexError m
