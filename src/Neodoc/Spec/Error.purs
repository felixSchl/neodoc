module Neodoc.Spec.Error where

import Prelude
import Data.Generic
import Data.Pretty
import Neodoc.Error (NeodocError(..)) as Neodoc
import Neodoc.Error.Class (class ToNeodocError)
import Text.Parsing.Parser as P

newtype SpecParseError = SpecParseError String

derive instance eqSpecParseError :: Eq SpecParseError
derive instance deriveSpecParseError :: Generic SpecParseError

instance showSpecParseError :: Show SpecParseError where
  show = gShow

instance prettySpecParseError :: Pretty SpecParseError where
  pretty (SpecParseError s) = s

instance toNeodocErrorSpecParseError :: ToNeodocError SpecParseError where
  toNeodocError (SpecParseError m) = Neodoc.SpecParseError m
