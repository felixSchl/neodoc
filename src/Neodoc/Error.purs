module Neodoc.Error where

import Prelude

import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Pretty (class Pretty)


data NeodocError
  = ScanError String
  | SpecLexError String
  | SpecParseError String
  | SpecSolveError String
  | ArgParserError String
  | VersionMissingError

derive instance genericNeodocError :: Generic NeodocError _

instance decodeJsonNeodocError :: DecodeJson NeodocError where
  decodeJson = genericDecodeJson

instance encodeJsonNeodocError :: EncodeJson NeodocError where
  encodeJson = genericEncodeJson

instance showNeodocError :: Show NeodocError where
  show = genericShow

instance prettyNeodocError :: Pretty NeodocError where
  pretty (ScanError msg) = "Failed to disect neodoc text:\n" <> msg
  pretty (SpecLexError msg) = "Failed to lex specification:\n" <> msg
  pretty (SpecParseError msg) = "Failed to parse specification:\n" <> msg
  pretty (SpecSolveError msg) = "Incoherent specification:\n" <> msg
  pretty (VersionMissingError) = "Package version could not be detected"
  pretty (ArgParserError msg) = msg


isDeveloperError :: NeodocError -> Boolean
isDeveloperError (ArgParserError _) = false
isDeveloperError _ = true
