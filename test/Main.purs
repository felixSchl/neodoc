module Test.Main where

import Prelude

import Test.Spec.Runner (run)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.ScannerSpec (scannerSpec)
import Test.Spec.UsageParserSpec (usageParserSpec)
import Test.Spec.DescParserSpec (descParserSpec)
import Test.Spec.GenSpec (genSpec)
import Test.Spec.GenTransSpec (genTransSpec)
import Test.Spec.SolverSpec (solverSpec)
import Test.Spec.DocoptSpec (docoptSpec)

main = run [consoleReporter] do
  scannerSpec     unit
  usageParserSpec unit
  descParserSpec  unit
  solverSpec      unit
  genSpec         unit
  genTransSpec    unit
  docoptSpec      unit
