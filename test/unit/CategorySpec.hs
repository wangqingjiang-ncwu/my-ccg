-- Copyright China University of Water Resources and Electric Power (c) 2019
-- All rights reserved.

module CategorySpec where

import Category
import Test.Hspec

spec :: Spec
spec = do
  describe "Category" $ do
    it "returns the first element of slashes" $ do
      head slashes `shouldBe` ("/." :: String)
    it "returns the first element of primitives" $ do
      head primitives `shouldBe` ("s" :: String)
    it "nilCate is the category Nil" $ do
      nilCate `shouldBe` nilCate
    it "Result of veriStrForCate \"s\\.np\" is True" $ do
      veriStrForCate "s\\.np" `shouldBe` (True :: Bool)
    it "Result of getCateFromString  \"s\\.np\" is category s\\.np" $ do
      show (getCateFromString "s\\.np") `shouldBe` "s\\.np"
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


