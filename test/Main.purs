module Test.Main where

import Prelude

import Test.Spec.Runner (run)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.ScannerSpec (scannerSpec)
import Test.Spec.UsageParserSpec (usageParserSpec)
import Test.Spec.DescParserSpec (descParserSpec)
import Test.Spec.ArgParserSpec (argParserSpec)
import Test.Spec.ForeignSpec (foreignSpec)
import Test.Spec.SolveSpec (solveSpec)
import Test.Spec.CompatSpec (compatSpec)
import Test.Support.CompatParser

-- Somehow, purescript needs this:
_liftEff :: ∀ e a. Effect e a -> Aff e a
_liftEff = liftEff


main = launchAff do
  tests <- _liftEff $ readTests "testcases.docopt"
  liftEff $ run [consoleReporter] do
    scannerSpec     unit
    usageParserSpec unit
    descParserSpec  unit
    solveSpec       unit
    argParserSpec   unit
    foreignSpec     tests
    compatSpec      tests
