module Test.Main where

import Prelude

import Control.Monad.Aff (launchAff)
import Test.Spec.Runner (run)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.ScannerSpec (scannerSpec)
import Test.Spec.UsageParserSpec (usageParserSpec)
import Test.Spec.DescParserSpec (descParserSpec)
import Test.Spec.ParserGenSpec (parserGenSpec)
import Test.Spec.TransSpec (transSpec)
import Test.Spec.SolverSpec (solverSpec)
import Test.Spec.DocoptSpec (genDocoptSpec)
import Control.Monad.Eff.Class (liftEff)

main = launchAff do
  docoptSpec <- genDocoptSpec
  liftEff $ run [consoleReporter] do
    scannerSpec     unit
    -- usageParserSpec unit
    -- descParserSpec  unit
    -- solverSpec      unit
    -- parserGenSpec   unit
    -- transSpec       unit
    -- docoptSpec      unit
