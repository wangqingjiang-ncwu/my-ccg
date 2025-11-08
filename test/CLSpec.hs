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

    it "The result of isBasicType (Basic \"s\") is True" $ do
      isBasicType (Basic "s") `shouldBe` True
    it "The result of isBasicType (Implicational (Basic x) (Basic y)) is True" $ do
      isBasicType (Implicational (Basic "x") (Basic "y")) `shouldBe` False
    it "The result of isImplicationaType (Implicational (Basic x) (Basic y)) is True" $ do
      isImplicationalType (Implicational (Basic "x") (Basic "y")) `shouldBe` True
    it "The result of show (Implicational (Basic x) (Basic y)) is (x -> y)" $ do
      show (Implicational (Basic "x") (Basic "y")) `shouldBe` "(x -> y)"
    it "The result of allAntes (Implicational (Basic x) (Implicational (Basic y) (Basic z))) is [Basic x, Basic y]" $ do
      allAntes (Implicational (Basic "x") (Implicational (Basic "y") (Basic "z"))) `shouldBe` [Basic "x", Basic "y"]
    it "The result of finalCons (Implicational (Basic x) (Implicational (Basic y) (Basic z))) is (Basic z)" $ do
      finalCons (Implicational (Basic "x") (Implicational (Basic "y") (Basic "z"))) `shouldBe` (Basic "z")
    it "The result of getSimpleTypeFromStr \"A\" is Basic \"A\"." $ do
      getSimpleTypeFromStr "A" `shouldBe` (Basic "A" :: SimpleType)
    it "The result of getSimpleTypeFromStr \"(A -> B)\" is Implicational (Basic \"A\") (Basic \"B\")." $ do
      getSimpleTypeFromStr "(A -> B)" `shouldBe` (Implicational (Basic "A") (Basic "B") :: SimpleType)

    it "The result of isStrOfSimpleType \"A\" is True." $ do
      isStrOfSimpleType "A" `shouldBe` True
    it "The result of isStrOfSimpleType \"(A -> B)\")) is True." $ do
      isStrOfSimpleType "(A -> B)" `shouldBe` True
    it "The result of isStrOfSimpleType \"1A\" is False." $ do
      isStrOfSimpleType "1A" `shouldBe` False
    it "The result of isStrOfSimpleType \"(A -> )\" is False." $ do
      isStrOfSimpleType "(A -> )" `shouldBe` False

    it "The result of isLVarTerm (Var \"A\") is True." $ do
      isLVarTerm (Var "A") `shouldBe` True
    it "The result of isLVarTerm (Lambda \"A\" (Var \"B\")) is True." $ do
      isLVarTerm (Lambda "A" (Var "B")) `shouldBe` False
    it "The result of isAbstactTerm (Var \"A\") is False." $ do
      isAbstactTerm (Var "A") `shouldBe` False
    it "The result of isAbstactTerm (Lambda \"A\" (Var \"B\")) is True." $ do
      isAbstactTerm (Lambda "A" (Var "B")) `shouldBe` True
    it "The result of isApplyTerm (Var \"A\") is False." $ do
      isApplyTerm (Var "A") `shouldBe` False
    it "The result of isApplyTerm (Lambda \"A\" (Var \"B\"), Var \"C\") is True." $ do
      isApplyTerm (Apply (Lambda "A" (Var "B")) (Var "C")) `shouldBe` True
    it "The result of getVarName (Var \"A\") is Just \"A\"." $ do
      getVarName (Var "A" ) `shouldBe` Just "A"
    it "The result of getVarName (Lambda \"A\" (Var \"B\")) is Nothing." $ do
      getVarName (Lambda "A" (Var "B")) `shouldBe` Nothing
    it "The result of getAbstractedVar (Lambda \"A\" (Var \"B\")) is Just (Var \"A\")." $ do
      getAbstractedVar (Lambda "A" (Var "B")) `shouldBe` Just (Var "A")
    it "The result of getAbstractedTerm (Lambda \"A\" (Var \"B\")) is Just (Var \"B\")." $ do
      getAbstractedTerm (Lambda "A" (Var "B")) `shouldBe` Just (Var "B")
    it "The result of getFuncTerm (Apply (Var \"A\") (Var \"B\")) is Just (Var \"A\")." $ do
      getFuncTerm (Apply (Var "A") (Var "B")) `shouldBe` Just (Var "A")
    it "The result of getParaTerm (Apply (Var \"A\") (Var \"B\")) is Just (Var \"B\")." $ do
      getParaTerm (Apply (Var "A") (Var "B")) `shouldBe` Just (Var "B")
    it "The result of fvOfLTerm (Var \"A\") is [Var \"A\"]." $ do
      fvOfLTerm (Var "A") `shouldBe` [Var "A"]
    it "The result of fvOfLTerm (Apply (Var \"A\") (Var \"B\")) is [Var \"A\", Var \"B\"]." $ do
      fvOfLTerm (Apply (Var "A") (Var "B")) `shouldBe` [Var "A", Var "B"]
    it "The result of fvOfLTerm (Lambda \"x\" (Var \"y\")) is [Var \"y\"]." $ do
      fvOfLTerm (Lambda "x" (Var "y")) `shouldBe` [Var "y"]
    it "The result of fvOfLTerm (Lambda \"x\" (Lambda \"y\" (Apply (Var \"x\") (Var \"z\")))) is [Var \"z\"]." $ do
      fvOfLTerm (Lambda "x" (Lambda "y" (Apply (Var "x") (Var "z")))) `shouldBe` [Var "z"]
    it "The result of getLTermFromStr \"a\" is a." $ do
      getLTermFromStr "a" `shouldBe` (Var "a")
    it "The result of getLTermFromStr \"(\\x. M)\" is Lambda x M" $ do
      getLTermFromStr "(\\x. M)" `shouldBe` (Lambda "x" (Var "M"))
{-
    it "The result of isStrOfLambdaTerm \"(\\x.M)\" is False" $ do
      isStrOfLambdaTerm "(\\x.M)" `shouldBe` False
    it "The result of isStrOfLambdaTerm \"(\\x. M)\" is True" $ do
      isStrOfLambdaTerm "(\\x. M)" `shouldBe` True
 -}

    it "The result of cLTerm2LambdaTerm (VarTerm \"x\" ) is (Var \"x\")." $ do
      cLTerm2LambdaTerm (VarTerm "x") `shouldBe` (Var "x")
    it "The result of cLTerm2LambdaTerm (ConstTerm \"a\") is (Var \"a\")." $ do
      cLTerm2LambdaTerm (ConstTerm "a") `shouldBe` (Var "a")
    it "The result of cLTerm2LambdaTerm (JuxTerm (VarTerm \"b\") (ConstTerm \"c\")) is (Apply (Var \"b\") (Var \"c\"))" $ do
      cLTerm2LambdaTerm (JuxTerm (VarTerm "b") (ConstTerm "c")) `shouldBe` (Apply (Var "b") (Var "c"))
