-- Copyright (c) 2019-2025 China University of Water Resources and Electric Power,
-- All rights reserved.

module CLSpec where

import CL
import Test.Hspec

spec :: Spec
spec = do
  describe "CL" $ do
    it "The result of isNullTerm nullTerm is True" $ do
      isNullTerm nullTerm `shouldBe` True
    it "The result of isPrimTerm (ConstTerm \"x\") is True" $ do
      isPrimTerm (ConstTerm "x") `shouldBe` True
    it "The result of isPrimTerm (JuxTerm (ConstTerm \"x\") (VarTerm \"y\")) is True" $ do
      isPrimTerm (JuxTerm (ConstTerm "x") (VarTerm "y")) `shouldBe` False
    it "The result of isCompoundTerm (JuxTerm (ConstTerm \"x\") (VarTerm \"y\")) is True" $ do
      isCompoundTerm (JuxTerm (ConstTerm "x") (VarTerm "y")) `shouldBe` True
    it "The result of isJuxTerm (JuxTerm (ConstTerm \"x\") (VarTerm \"y\")) is True" $ do
      isJuxTerm (JuxTerm (ConstTerm "x") (VarTerm "y")) `shouldBe` True
    it "The result of fstTerm (JuxTerm (ConstTerm \"x\") (VarTerm \"y\")) is ConstTerm \"x\"" $ do
      fstTerm (JuxTerm (ConstTerm "x") (VarTerm "y")) `shouldBe` Just (ConstTerm "x")
    it "The result of sndTerm (JuxTerm (ConstTerm \"x\") (VarTerm \"y\")) is ConstTerm \"x\"" $ do
      sndTerm (JuxTerm (ConstTerm "x") (VarTerm "y")) `shouldBe` Just (VarTerm "y")
    it "The result of fvOfTerm (JuxTerm (VarTerm \"x\") (VarTerm \"y\")) is [VarTerm \"x\", VarTerm \"y\"]" $ do
      fvOfTerm (JuxTerm (VarTerm "x") (VarTerm "y")) `shouldBe` [VarTerm "x", VarTerm "y"]
    it "The result of subTermOfTerm (JuxTerm (VarTerm \"x\") (VarTerm \"y\")) is [JuxTerm (VarTerm \"x\") (VarTerm \"y\"), VarTerm \"x\", VarTerm \"y\"]" $ do
      subTermOfTerm (JuxTerm (VarTerm "x") (VarTerm "y")) `shouldBe` [JuxTerm (VarTerm "x") (VarTerm "y"), VarTerm "x", VarTerm "y"]
    it "The result of app (ConstTerm \"x\") (VarTerm \"y\")) is JuxTerm (ConstTerm \"x\") (VarTerm \"y\")" $ do
      app (ConstTerm "x") (VarTerm "y") `shouldBe` JuxTerm (ConstTerm "x") (VarTerm "y")
    it "The result of aAxiom (JuxTerm (JuxTerm (ConstTerm \"A\") (ConstTerm \"x\")) (VarTerm \"y\")) is JuxTerm (ConstTerm \"x\") (VarTerm \"y\")" $ do
      aAxiom (JuxTerm (JuxTerm (ConstTerm "A") (ConstTerm "x")) (VarTerm "y")) `shouldBe` JuxTerm (ConstTerm "x") (VarTerm "y")
    it "The result of bAxiom (JuxTerm (JuxTerm (JuxTerm (ConstTerm \"B\") (ConstTerm \"x\")) (VarTerm \"y\")) (VarTerm \"z\")) is JuxTerm (ConstTerm \"x\") (JuxTerm (VarTerm \"y\") (VarTerm \"z\"))" $ do
      bAxiom (JuxTerm (JuxTerm (JuxTerm (ConstTerm "B") (ConstTerm "x")) (VarTerm "y")) (VarTerm "z")) `shouldBe` JuxTerm (ConstTerm "x") (JuxTerm (VarTerm "y") (VarTerm "z"))
    it "The result of termSeq2Term [JuxTerm (ConstTerm \"A\") (ConstTerm \"x\"), (VarTerm \"y\")] is JuxTerm (JuxTerm (ConstTerm \"A\") (ConstTerm \"x\") (VarTerm \"y\")" $ do
      termSeq2Term [JuxTerm (ConstTerm "A") (ConstTerm "x"), (VarTerm "y")] `shouldBe` JuxTerm (JuxTerm (ConstTerm "A") (ConstTerm "x")) (VarTerm "y")
    it "The result of getTermFromStr \"\" is nullTerm" $ do
        getTermFromStr "" `shouldBe` nullTerm
    it "The result of getTermFromStr \"a\" is ConstTerm \"a\"" $ do
        getTermFromStr "a" `shouldBe` ConstTerm "a"
    it "The result of getTermFromStr \"(M N)\" is JuxTerm (ConstTerm \"M\") (ConstTerm \"N\")" $ do
        getTermFromStr "(M N)" `shouldBe` JuxTerm (ConstTerm "M") (ConstTerm "N")
    it "The result of sortTerms [JuxTerm (ConstTerm \"A\") (ConstTerm \"x\"), (VarTerm \"y\")] is [VarTerm \"y\", JuxTerm (ConstTerm \"A\") (ConstTerm \"x\")]" $ do
      sortTerms [JuxTerm (ConstTerm "A") (ConstTerm "x"), (VarTerm "y")] `shouldBe` [VarTerm "y", JuxTerm (ConstTerm "A") (ConstTerm "x")]
    it "The result of doCombAxiom (JuxTerm (JuxTerm (ConstTerm \"A\") (ConstTerm \"x\")) (VarTerm \"y\")) is (JuxTerm (ConstTerm \"x\") (VarTerm \"y\"), True)" $ do
      doCombAxiom (JuxTerm (JuxTerm (ConstTerm "A") (ConstTerm "x")) (VarTerm "y")) `shouldBe` (JuxTerm (ConstTerm "x") (VarTerm "y"), True)
    it "The result of doCombAxiom (JuxTerm (JuxTerm (JuxTerm (ConstTerm \"B\") (ConstTerm \"x\")) (VarTerm \"y\")) (VarTerm \"z\")) is (JuxTerm (ConstTerm \"x\") (JuxTerm (VarTerm \"y\") (VarTerm \"z\")), True)" $ do
      doCombAxiom (JuxTerm (JuxTerm (JuxTerm (ConstTerm "B") (ConstTerm "x")) (VarTerm "y")) (VarTerm "z")) `shouldBe` (JuxTerm (ConstTerm "x") (JuxTerm (VarTerm "y") (VarTerm "z")), True)
    it "The result of reduct 0 5 (JuxTerm (JuxTerm (JuxTerm (ConstTerm \"B\") (ConstTerm \"x\")) (VarTerm \"y\")) (VarTerm \"z\")) is JuxTerm (ConstTerm \"x\") (JuxTerm (VarTerm \"y\") (VarTerm \"z\"))" $ do
      reduct 0 5 (JuxTerm (JuxTerm (JuxTerm (ConstTerm "B") (ConstTerm "x")) (VarTerm "y")) (VarTerm "z")) `shouldBe` JuxTerm (ConstTerm "x") (JuxTerm (VarTerm "y") (VarTerm "z"))
