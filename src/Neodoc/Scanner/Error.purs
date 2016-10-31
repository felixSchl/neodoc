module Neodoc.Scanner.Error where

import Prelude
import Data.Pretty (class Pretty, pretty)
import Neodoc.Error (NeodocError(..)) as Neodoc
import Neodoc.Error.Class (class ToNeodocError)

newtype ScanError = ScanError String

instance toNeodocErrorScanError :: ToNeodocError ScanError where
  toNeodocError (ScanError msg) = Neodoc.ScanError msg

instance prettyScanError :: Pretty ScanError where
  pretty (ScanError msg) = msg
