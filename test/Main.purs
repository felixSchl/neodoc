module Test.Main where

import Prelude

import Test.Spec.Runner (run)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.ScannerSpec (scannerSpec)
import Test.Spec.UsageParserSpec (usageParserSpec)
import Test.Spec.OptionsParserSpec (optionsParserSpec)
import Test.Spec.GeneratorSpec (generatorSpec)

main = run [consoleReporter] do
  generatorSpec
  scannerSpec
  usageParserSpec
  optionsParserSpec
