module Test.Main where

import Prelude

import Control.Monad.Aff (launchAff)
import Test.Spec.Runner (run)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.ScannerSpec (scannerSpec)
import Test.Spec.UsageParserSpec (usageParserSpec)
import Test.Spec.DescParserSpec (descParserSpec)
import Test.Spec.ArgParserSpec (parserGenSpec)
import Test.Spec.SolverSpec (solverSpec)
import Test.Spec.CompatSpec (genCompatSpec)
import Test.Spec.DocoptSpec (docoptSpec)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff (Eff())
import Node.FS (FS())
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Console (CONSOLE)
import Node.Process (PROCESS)
import Test.Assert (ASSERT)


main :: Eff ( err     :: EXCEPTION
            , process :: PROCESS
            , fs      :: FS
            , console :: CONSOLE
            , assert  :: ASSERT
            ) Unit
main = launchAff do
  compatSpec <- genCompatSpec
  liftEff $ run [consoleReporter] do
    scannerSpec     unit
    usageParserSpec unit
    descParserSpec  unit
    solverSpec      unit
    parserGenSpec   unit
    docoptSpec      unit
    compatSpec      unit
