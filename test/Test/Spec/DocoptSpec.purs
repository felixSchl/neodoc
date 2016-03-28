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

import Docopt (fromREADME, defaultOptions)

concatPaths :: forall a. (Semigroup a) => a -> a -> a
concatPaths a b = a ++ b
infixl 9 concatPaths as </>

fixtures = "./test/Test/Spec/fixtures/"

docoptSpec = \_ -> do
  describe "Docopt" do
    it "should work..." do
      v <- fromREADME
              (defaultOptions {
                argv = Just []
              , env  = Just Env.empty
              })
        $ fixtures </> "README.md"
      traceShowA v
      pure unit
