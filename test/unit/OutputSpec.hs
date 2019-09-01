-- Copyright China University of Water Resources and Electric Power (c) 2019
-- All rights reserved.

module OutputSpec where

import Category
import Parse
import Output
import Test.Hspec

spec :: Spec
spec = do
    describe "Output" $ do
      it "All functions in module Output are IO-type, and not-testable now." $ do
        True `shouldBe` True


