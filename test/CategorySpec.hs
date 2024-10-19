-- Copyright China University of Water Resources and Electric Power (c) 2019
-- All rights reserved.

module CategorySpec where

import Category
import Test.Hspec

spec :: Spec
spec = do
  describe "Category" $ do
    it "returns the result of cateEqual s np is False" $ do
      cateEqual sCate npCate `shouldBe` (False :: Bool)
    it "returns the result of cateEqual s\\.np s\\*np is True" $ do
      cateEqual (getCateFromString "s\\.np") (getCateFromString "s\\*np") `shouldBe` (True :: Bool)
    it "returns the result of cateEqual (s\\.np)/.np (s\\.np)/xnp is True" $ do
      cateEqual (getCateFromString "(s\\.np)/.np") (getCateFromString "(s\\.np)/xnp") `shouldBe` True
    it "returns the first element of slashes" $ do
      head slashes `shouldBe` ("/." :: String)
    it "returns the first element of primitives" $ do
      head primitives `shouldBe` ("s" :: String)
    it "nilCate is the category Nil" $ do
      nilCate `shouldBe` nilCate
    it "sCate is the category s" $ do
      sCate `shouldBe` sCate
    it "npCate is the category np" $ do
      npCate `shouldBe` npCate
    it "Result of veriStrForCate \"s\\.np\" is True" $ do
      veriStrForCate "s\\.np" `shouldBe` (True :: Bool)
    it "Result of getCateFromString  \"s\\.np\" is category s\\.np" $ do
      show (getCateFromString "s\\.np") `shouldBe` "s\\.np"
    it "Result of getCateFromString  \"(s\\.np)/.np\" is category (s\\.np)/.np" $ do
      show (getCateFromString "(s\\.np)/.np") `shouldBe` "(s\\.np)/.np"
    it "Result of indexOfSlash 0 0 \"(s\\*np)/#(s\\*np)\" is 7" $ do
      indexOfSlash 0 0 "(s\\*np)/#(s\\*np)" `shouldBe` 7
    it "Result of leftStr (s\\*np)/#(s\\.np) is s\\*np" $ do
      leftStr "(s\\*np)/#(s\\.np)" `shouldBe` "s\\*np"
    it "Result of rightStr (s\\*np)/#(s\\.np) is s\\.np" $ do
      rightStr "(s\\*np)/#(s\\.np)" `shouldBe` "s\\.np"
    it "Result of midSlashStr (s\\*np)/#(s\\.np) is /#" $ do
      midSlashStr "(s\\*np)/#(s\\.np)" `shouldBe` "/#"
    it "Result isNil Nil is True" $ do
      isNil (getCateFromString "Nil") `shouldBe` (True :: Bool)
    it "Result isPrimitive (Primitive \"s\") is True" $ do
      isPrimitive (getCateFromString "s") `shouldBe` (True :: Bool)
    it "Result isDerivative (getCateFromString \"(s\\.np)/#(s\\.np)\") is True" $ do
      isDerivative (getCateFromString "(s\\.np)/#(s\\.np)") `shouldBe` (True :: Bool)
    it "Result (<) npCate objectExtractionCate is True" $ do
      (<) npCate objectExtractionCate `shouldBe` True
    it "Result (<) sCate adjCate is True" $ do
      (<) sCate adjCate `shouldBe` True
    it "Result (>) predCate objectExtractionCate is True" $ do
      (>) predCate objectExtractionCate `shouldBe` True
