module Neodoc.Spec.Error where

import Prelude
import Data.Generic.Rep
import Data.Generic.Rep.Show (genericShow)
import Data.Pretty
import Neodoc.Error (NeodocError(..)) as Neodoc
import Neodoc.Error.Class (class ToNeodocError)

newtype SpecParseError = SpecParseError String

derive instance eqSpecParseError :: Eq SpecParseError
derive instance deriveSpecParseError :: Generic SpecParseError _

instance showSpecParseError :: Show SpecParseError where
  show = genericShow

instance prettySpecParseError :: Pretty SpecParseError where
  pretty (SpecParseError s) = s

instance toNeodocErrorSpecParseError :: ToNeodocError SpecParseError where
  toNeodocError (SpecParseError m) = Neodoc.SpecParseError m
