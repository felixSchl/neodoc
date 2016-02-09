module Test.Main where

import Prelude

import Test.Spec.Runner (run)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.ScannerSpec (scannerSpec)
import Test.Spec.UsageParserSpec (usageParserSpec)
import Test.Spec.DescParserSpec (descParserSpec)
import Test.Spec.GeneratorSpec (generatorSpec)
import Test.Spec.SolverSpec (solverSpec)

main = run [consoleReporter] do
  -- scannerSpec
  -- usageParserSpec
  -- descParserSpec
  solverSpec
  -- generatorSpec
