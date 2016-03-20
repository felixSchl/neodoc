module Test.Spec.DocoptSpec (docoptSpec) where

import Prelude
import Debug.Trace
import Test.Spec (describe, it)
import Docopt (fromREADME_)

fixture = ("./test/Test/Spec/fixtures/" ++)

docoptSpec = \_ -> do
  describe "foo" do
    it "bar" do
      fromREADME_ $ fixture "README.md"
      pure unit
