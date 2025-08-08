-- Copyright (c) 2019-2025 China University of Water Resources and Electric Power
-- All rights reserved.

module MyTestSpec where

import MyTest
import Database
import Database.MySQL.Base
import Data.Time.Clock
import Test.Hspec

spec :: Spec
spec = do
  describe "Test" $ do
    it "The result of testClockTime is OK." $ do
      testClockTime
      0 `shouldBe` 0    
    it "The result of testMySQLRead 1 100 1 1 is 0." $ do
      res <- testMySQLRead 1 100 1 1
      res `shouldBe` (0 :: Int)
    it "The result of testMySQLRead 1 200 1 1 is 0." $ do
      res <- testMySQLRead 1 200 1 1
      res `shouldBe` (0 :: Int)
    it "The result of testMySQLRead 1 300 1 1 is 0." $ do
      res <- testMySQLRead 1 300 1 1
      res `shouldBe` (0 :: Int)
    it "The result of testMySQLRead 1 400 1 1 is 0." $ do
      res <- testMySQLRead 1 400 1 1
      res `shouldBe` (0 :: Int)
{-
    it "The result of testMySQLRead 1 500 1 1 is 0." $ do
      res <- testMySQLRead 1 500 1 1
      res `shouldBe` (0 :: Int)
    it "The result of testMySQLRead 1 600 1 1 is 0." $ do
      res <- testMySQLRead 1 600 1 1
      res `shouldBe` (0 :: Int)
    it "The result of testMySQLRead 1 700 1 1 is 0." $ do
      res <- testMySQLRead 1 700 1 1
      res `shouldBe` (0 :: Int)
    it "The result of testMySQLRead 1 800 1 1 is 0." $ do
      res <- testMySQLRead 1 800 1 1
      res `shouldBe` (0 :: Int)
    it "The result of testMySQLRead 1 900 1 1 is 0." $ do
      res <- testMySQLRead 1 900 1 1
      res `shouldBe` (0 :: Int)
    it "The result of testMySQLRead 1 1000 1 1 is 0." $ do
      res <- testMySQLRead 1 1000 1 1
      res `shouldBe` (0 :: Int)
 -}
