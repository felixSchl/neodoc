module Test.Spec.DocoptSpec (docoptSpec) where

import Prelude
import Debug.Trace
import Test.Spec (describe, it)

docoptSpec = \_ -> do
  describe "foo" do
    it "bar" do
      pure unit
