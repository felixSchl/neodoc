module Test.Spec.DocoptSpec (docoptSpec) where

import Prelude
import Debug.Trace
import Data.Maybe (Maybe(..))
import Test.Spec (Spec(), describe, it)
import Control.Monad.Eff.Exception (EXCEPTION())
import Control.Monad.Eff.Console (CONSOLE())
import Node.FS (FS())
import Node.Process (PROCESS())
import Language.Docopt.Env as Env

docoptSpec = \_ -> do
  describe "Docopt" do
    it "should work..." do
      pure unit
