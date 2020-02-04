module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec.Runner (runSpec)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.ScannerSpec (scannerSpec)
import Test.Spec.UsageParserSpec (usageParserSpec)
import Test.Spec.DescParserSpec (descParserSpec)
import Test.Spec.ArgParserSpec (argParserSpec)
import Test.Spec.SolveSpec (solveSpec)
import Test.Support.CompatParser (readTests)


main :: Effect Unit
main = do
  tests <- readTests "testcases.docopt"
  launchAff_ $ runSpec [consoleReporter] do
    scannerSpec     unit
    scannerSpec     unit
    usageParserSpec unit
    descParserSpec  unit
    solveSpec       unit
    argParserSpec   unit
