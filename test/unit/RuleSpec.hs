-- Copyright China University of Water Resources and Electric Power (c) 2019
-- All rights reserved.

module RuleSpec where

import Category
import Rule
import Test.Hspec

spec :: Spec
spec = do
  describe "Rule" $ do
    it "The result of forwardly application s/.np to np is (s,\">\")" $ do
      appF (getCateFromString "s/.np") npCate `shouldBe`  (sCate, ">")

    it "The result of forwardly application s to np is (Nil, \">\")" $ do
      appF sCate npCate `shouldBe` (nilCate,">")

    it "The result of backwardly application np to s\\.np is (s,\"<\")" $ do
      appB npCate (getCateFromString "s\\.np") `shouldBe` (sCate,"<")

    it "The result of forward harmonic composition (s\\.np)/#(s\\.np) with (s\\.np)/.np is ((s\\.np)/.np, \">B\")" $ do
      comFh (getCateFromString "(s\\.np)/#(s\\.np)") (getCateFromString "(s\\.np)/.np") `shouldBe` (getCateFromString "(s\\.np)/.np", ">B")

    it "The result of forward harmonic composition (s\\.np)/*(s\\.np) with (s\\.np)/.np is (Nil,\">B\")" $ do
      comFh (getCateFromString "(s\\.np)/*(s\\.np)") (getCateFromString "(s\\.np)/.np") `shouldBe` (getCateFromString "Nil", ">B")

    it "The result of forward harmonic composition^2 (s\\.np)/#(s\\.np) with ((s\\.np)/#np)/.np) is (((s\\.np)/#np/).np, \">B2\")" $ do
      comFh2 (getCateFromString "(s\\.np)/#(s\\.np)") (getCateFromString "((s\\.np)/#np)/.np") `shouldBe` (getCateFromString "((s\\.np)/#np)/.np", ">B2")

    it "The result of backward harmonic composition (s\\.np)\\.np with (s\\.np)\\#(s\\.np) is ((s\\.np)\\.np, \"<B\")" $ do
      comBh (getCateFromString "(s\\.np)\\.np") (getCateFromString "(s\\.np)\\#(s\\.np)") `shouldBe` (getCateFromString "(s\\.np)\\.np", "<B")

    it "The result of forward crossing composition (s\\.np)/x(s\\.np) with (s\\.np)\\xnp is ((s\\.np)\\xnp, \">Bx\")" $ do
      comFc (getCateFromString "(s\\.np)/x(s\\.np)") (getCateFromString "(s\\.np)\\xnp") `shouldBe` (getCateFromString "(s\\.np)\\xnp", ">Bx")

    it "The result of backward crossing composition (s\\.np)/.np with (s\\.np)\\x(s\\.np) is ((s\\.np)/.np, \"<Bx\")" $ do
      comBc (getCateFromString "(s\\.np)/.np") (getCateFromString "(s\\.np)\\x(s\\.np)") `shouldBe` (getCateFromString "(s\\.np)/.np", "<Bx")

    it "The result of forward raising and harmonic composition np with (s\\.np)/.np is (s/.np, \">T->B\")" $ do
      raiFh npCate (getCateFromString "(s\\.np)/.np") `shouldBe` (getCateFromString "s/.np", ">T->B")

    it "The result of forward raising and harmonic composition np with s\\.np is (Nil, \">T->B\")" $ do
      raiFh npCate (getCateFromString "s\\.np") `shouldBe` (nilCate, ">T->B")

    it "The result of forward raising and harmonic composition np with (s\\.np)/.np is (s/.np, \">T->B\")" $ do
      raiFh npCate (getCateFromString "(s\\.np)/.np") `shouldBe` (getCateFromString "s/.np", ">T->B")

    it "The result of fordward raising and crossing composition np with (s\\.np)\\xnp is (s\\xnp, \">T->Bx\")" $ do
      raiFc npCate (getCateFromString "(s\\.np)\\xnp") `shouldBe` (getCateFromString "s\\xnp", ">T->Bx")

    it "The result of backward raising and harmonic composition (s/.np)\\#np with np is (s\\#np, \"<T-<B\")" $ do
      raiBh (getCateFromString "(s/.np)\\#np") npCate `shouldBe` (getCateFromString "s\\#np", "<T-<B")

    it "The result of backward raising and crossing composition (s/.np)/.s with np is (s/.s, \"<T-<Bx\")" $ do
      raiBc (getCateFromString "(s/.np)/.s") npCate `shouldBe` (getCateFromString "s/.s", "<T-<Bx")

    it "The result of backward raising and crossing composition (s/.np)/#s with np is (Nil, \"<T-<Bx\")" $ do
      raiBc (getCateFromString "(s/.np)/#s") npCate `shouldBe` (getCateFromString "Nil", "<T-<Bx")



