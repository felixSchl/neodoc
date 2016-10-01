module Test.Main where

import Prelude

import Control.Monad.Aff (Aff, launchAff)
import Test.Spec.Runner (run)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.ScannerSpec (scannerSpec)
import Test.Spec.UsageParserSpec (usageParserSpec)
import Test.Spec.DescParserSpec (descParserSpec)
import Test.Spec.ArgParserSpec (argParserSpec)
import Test.Spec.ForeignSpec (foreignSpec)
import Test.Spec.SolveSpec (solveSpec)
import Test.Spec.CompatSpec (compatSpec)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff (Eff())
import Node.FS (FS())
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Console (CONSOLE)
import Node.Process (PROCESS)
import Test.Assert (ASSERT)
import Test.Support.CompatParser

-- Somehow, purescript needs this:
_liftEff :: ∀ e a. Eff e a -> Aff e a
_liftEff = liftEff

main :: Eff ( err     :: EXCEPTION
            , process :: PROCESS
            , fs      :: FS
            , console :: CONSOLE
            , assert  :: ASSERT
            | _
            ) _
main = launchAff do
  tests <- _liftEff $ readTests "testcases.docopt"
  liftEff $ run [consoleReporter] do
    -- scannerSpec     unit
    -- usageParserSpec unit
    -- descParserSpec  unit
    -- solveSpec       unit
    -- argParserSpec   unit
    -- foreignSpec     tests
    compatSpec      tests
