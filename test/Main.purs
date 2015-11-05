module Test.Main where

import Prelude

import Test.Spec.Runner (run)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.ScannerSpec (scannerSpec)
import Test.Spec.UsageParserSpec (usageParserSpec)
import Test.Spec.OptionsParserSpec (optionsParserSpec)

main = run [consoleReporter] do
  scannerSpec
  usageParserSpec
  optionsParserSpec
