module Test.Main where

import Prelude

import Control.Monad.Aff (Aff(), launchAff)
import Test.Spec.Runner (run)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.ScannerSpec (scannerSpec)
import Test.Spec.UsageParserSpec (usageParserSpec)
import Test.Spec.DescParserSpec (descParserSpec)
import Test.Spec.ParserGenSpec (parserGenSpec)
import Test.Spec.TransSpec (transSpec)
import Test.Spec.SolverSpec (solverSpec)
import Test.Spec.CompatSpec (genCompatSpec)
import Test.Spec.DocoptSpec (docoptSpec)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff (Eff())
import Node.FS (FS())
import Control.Monad.Eff.Exception (EXCEPTION())
import Control.Monad.Eff.Console(log, CONSOLE())
import Node.Process (PROCESS())
import Node.Process as Process
import Control.Bind((=<<))

import Docopt (fromREADME_)

main = do
  fromREADME_ "./test/Test/Spec/fixtures/README.md"


-- main = launchAff do
-- --   compatSpec <- genCompatSpec
--   liftEffA $ run [consoleReporter] do
--     -- scannerSpec     unit
--     -- usageParserSpec unit
--     -- descParserSpec  unit
--     -- solverSpec      unit
--     parserGenSpec   unit
--     -- transSpec       unit
--     -- compatSpec      unit
--     -- docoptSpec      unit
