-- Copyright China University of Water Resources and Electric Power (c) 2019
-- All rights reserved.

module RuleSpec where

import Category
import Rule
import Test.Hspec

spec :: Spec
spec = do
  describe "Rule" $ do
    it "The result of forwardly application s/.np to np is s" $ do
      appF (getCateFromString "s/.np") (getCateFromString "np") `shouldBe` (getCateFromString "s")

    it "The result of forwardly application s to np is Nil" $ do
      appF (getCateFromString "s") (getCateFromString "np") `shouldBe` (nilCate)

    it "The result of backwardly application np to s\\.np is s" $ do
      appB (getCateFromString "np") (getCateFromString "s\\.np") `shouldBe` (getCateFromString "s")

    it "The result of forward harmonic composition (s\\.np)/#(s\\.np) with (s\\.np)/.np is (s\\.np)/.np" $ do
      comFh (getCateFromString "(s\\.np)/#(s\\.np)") (getCateFromString "(s\\.np)/.np") `shouldBe` (getCateFromString "(s\\.np)/.np")

    it "The result of forward harmonic composition (s\\.np)/*(s\\.np) with (s\\.np)/.np is Nil" $ do
      comFh (getCateFromString "(s\\.np)/*(s\\.np)") (getCateFromString "(s\\.np)/.np") `shouldBe` (getCateFromString "Nil")

    it "The result of forward harmonic composition^2 (s\\.np)/#(s\\.np) with ((s\\.np)/#np)/.np) is ((s\\.np)/#np/).np" $ do
      comFh2 (getCateFromString "(s\\.np)/#(s\\.np)") (getCateFromString "((s\\.np)/#np)/.np") `shouldBe` (getCateFromString "((s\\.np)/#np)/.np")

    it "The result of backward harmonic composition (s\\.np)\\.np with (s\\.np)\\#(s\\.np) is (s\\.np)\\.np" $ do
      comBh (getCateFromString "(s\\.np)\\.np") (getCateFromString "(s\\.np)\\#(s\\.np)") `shouldBe` (getCateFromString "(s\\.np)\\.np")

    it "The result of forward crossing composition (s\\.np)/x(s\\.np) with (s\\.np)\\xnp is (s\\.np)\\xnp" $ do
      comFc (getCateFromString "(s\\.np)/x(s\\.np)") (getCateFromString "(s\\.np)\\xnp") `shouldBe` (getCateFromString "(s\\.np)\\xnp")

    it "The result of backward crossing composition (s\\.np)/.np with (s\\.np)\\x(s\\.np) is (s\\.np)/.np" $ do
      comBc (getCateFromString "(s\\.np)/.np") (getCateFromString "(s\\.np)\\x(s\\.np)") `shouldBe` (getCateFromString "(s\\.np)/.np")

    it "The result of forward raising and harmonic composition np with (s\\.np)/.np is s/.np" $ do
      raiFh (getCateFromString "np") (getCateFromString "(s\\.np)/.np") `shouldBe` (getCateFromString "s/.np")

    it "The result of forward raising and harmonic composition np with s\\.np is Nil" $ do
      raiFh (getCateFromString "np") (getCateFromString "s\\.np") `shouldBe` nilCate

    it "The result of forward raising and harmonic composition np with (s\\.np)/.np is s/.np" $ do
      raiFh (getCateFromString "np") (getCateFromString "(s\\.np)/.np") `shouldBe` (getCateFromString "s/.np")

    it "The result of fordward raising and crossing composition np with (s\\.np)\\xnp is s\\xnp" $ do
      raiFc (getCateFromString "np") (getCateFromString "(s\\.np)\\xnp") `shouldBe` (getCateFromString "s\\xnp")

    it "The result of backward raising and harmonic composition (s/.np)\\#np with np is s\\#np" $ do
      raiBh (getCateFromString "(s/.np)\\#np") (getCateFromString "np") `shouldBe` (getCateFromString "s\\#np")

    it "The result of backward raising and crossing composition (s/.np)/.s with np is s/.s" $ do
      raiBc (getCateFromString "(s/.np)/.s") (getCateFromString "np") `shouldBe` (getCateFromString "s/.s")

    it "The result of backward raising and crossing composition (s/.np)/#s with np is Nil" $ do
      raiBc (getCateFromString "(s/.np)/#s") (getCateFromString "np") `shouldBe` (getCateFromString "Nil")











