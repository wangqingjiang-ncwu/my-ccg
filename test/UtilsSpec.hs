-- Copyright (c) 2019-2025 China University of Water Resources and Electric Power
-- All rights reserved.

module UtilsSpec where

import Utils
import Phrase
import Test.Hspec

spec :: Spec
spec = do
  describe "Utils" $ do
    it "The result of show Empty is ()" $ do
      show (Empty :: BiTree PhraCate) `shouldBe`  "()"
    it "The result of show (Node 1 (Node 2 Empty Empty) Empty) is (1,(2,(),()),())" $ do
      show (Node 1 (Node 2 Empty Empty) Empty) `shouldBe` "(1,(2,(),()),())"
    it "The result of Empty < (Node 1 Empty Empty) is True" $ do
      Empty < (Node 1 Empty Empty) `shouldBe` True
    it "The result of getLeftSub (Node 1 Empty Empty) is Empty" $ do
      getLeftSub (Node 1 Empty Empty) `shouldBe` Empty
    it "The result of getLeftSub (Node 1 (Node 2 Empty Empty) Empty) is (Node 2 Empty Empty)" $ do
      getLeftSub (Node 1 (Node 2 Empty Empty) Empty) `shouldBe` (Node 2 Empty Empty)
    it "The result of setRightSub Empty (Node 1 Empty (Node 2 Empty Empty)) is (Node 1 Empty Empty)" $ do
      setRightSub Empty (Node 1 Empty (Node 2 Empty Empty)) `shouldBe` (Node 1 Empty Empty)
