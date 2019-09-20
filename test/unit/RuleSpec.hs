-- Copyright China University of Water Resources and Electric Power (c) 2019
-- All rights reserved.

module RuleSpec where

import Category
import Rule
import Test.Hspec

spec :: Spec
spec = do
  describe "Rule" $ do
    it "The result of forwardly application (s/.np, \"eat'\") to (np, \"food'\") is (s,\">\", \"eat' food'\")" $ do
      appF (getCateFromString "s/.np", "eat'") (npCate, "food'") `shouldBe`  (sCate, ">", "eat' food'")

    it "The result of forwardly application (s, \"(eats' food') Mary'\" to (np, \"Frank'\") is (Nil, \">\", \"\")" $ do
      appF (sCate, "(eats' food') Mary'") (npCate, "Frank'") `shouldBe` (nilCate, ">", "")

    it "The result of backwardly application (np, \"Frank'\") to (s\\.np, \"eats' food'\") is (s,\"<\", \"(eats' food') Frank'\")" $ do
      appB (npCate, "Frank'") (getCateFromString "s\\.np", "eats' food'") `shouldBe` (sCate,"<","(eats' food') Frank'")

    it "The result of forward harmonic composition ((s\\.np)/#(s\\.np), \"very'\") with ((s\\.np)/.np,\"hates'\") is ((s\\.np)/.np, \">B\", \"very' hates'\")" $ do
      comFh (getCateFromString "(s\\.np)/#(s\\.np)", "very'") (getCateFromString "(s\\.np)/.np", "hates'") `shouldBe` (getCateFromString "(s\\.np)/.np", ">B", "very' hates'")

    it "The result of forward harmonic composition ((s\\.np)/*(s\\.np), \"very'\") with ((s\\.np)/.np, \"hates'\") is (Nil, \">B\", \"\")" $ do
      comFh (getCateFromString "(s\\.np)/*(s\\.np)", "very'") (getCateFromString "(s\\.np)/.np", "hates'") `shouldBe` (getCateFromString "Nil", ">B", "")

    it "The result of forward harmonic composition^2 ((s\\.np)/#(s\\.np) \"quickly'\") with (((s\\.np)/#np)/.np), \"give'\") is (((s\\.np)/#np/).np, \">B2\", \"quickly' give'\")" $ do
      comFh2 (getCateFromString "(s\\.np)/#(s\\.np)", "quickly'") (getCateFromString "((s\\.np)/#np)/.np", "give'") `shouldBe` (getCateFromString "((s\\.np)/#np)/.np", ">B2", "quickly' give'")

    it "The result of backward harmonic composition ((s\\.np)\\.np \"doony'\") with ((s\\.np)\\#(s\\.np), \"well'\") is ((s\\.np)\\.np, \"<B\", \"well' doony'\")" $ do
      comBh (getCateFromString "(s\\.np)\\.np", "doony'") (getCateFromString "(s\\.np)\\#(s\\.np)", "well'") `shouldBe` (getCateFromString "(s\\.np)\\.np", "<B", "well' doony'")

    it "The result of forward crossing composition ((s\\.np)/x(s\\.np), \"very'\") with ((s\\.np)\\xnp, \"doony'\") is ((s\\.np)\\xnp, \">Bx\", \"very' doony'\")" $ do
      comFc (getCateFromString "(s\\.np)/x(s\\.np)", "very'") (getCateFromString "(s\\.np)\\xnp", "doony'") `shouldBe` (getCateFromString "(s\\.np)\\xnp", ">Bx", "very' doony'")

    it "The result of backward crossing composition ((s\\.np)/.np, \"do'\") with ((s\\.np)\\x(s\\.np), \"well'\") is ((s\\.np)/.np, \"<Bx\", \"well' do'\")" $ do
      comBc (getCateFromString "(s\\.np)/.np", "do'") (getCateFromString "(s\\.np)\\x(s\\.np)", "well'") `shouldBe` (getCateFromString "(s\\.np)/.np", "<Bx", "well' do'")

    it "The result of forward raising and harmonic composition (np, \"I'\") with ((s\\.np)/.np, \"eat'\") is (s/.np, \">T->B\", \"I' eat'\")" $ do
      raiFh (npCate, "I'") (getCateFromString "(s\\.np)/.np", "eat'") `shouldBe` (getCateFromString "s/.np", ">T->B", "I' eat'")

    it "The result of forward raising and harmonic composition (np, \"I'\") with (s\\.np, \"watch' TV'\") is (Nil, \">T->B\", \"\")" $ do
      raiFh (npCate, "I'") (getCateFromString "s\\.np", "watch' TV'") `shouldBe` (nilCate, ">T->B", "")

    it "The result of forward raising and harmonic composition (np, \"Frank'\") with ((s\\.np)/.np, \"give'\") is ((s/.np, \">T->B\", \"Frank' give'\")" $ do
      raiFh (npCate, "Frank'") (getCateFromString "(s\\.np)/.np", "give'") `shouldBe` (getCateFromString "s/.np", ">T->B", "Frank' give'")

    it "The result of fordward raising and crossing composition (np, \"Frank'\") with ((s\\.np)\\xnp, \"doony'\") is (s\\xnp, \">T->Bx\", \"Frank' doony'\")" $ do
      raiFc (npCate, "Frank'") (getCateFromString "(s\\.np)\\xnp", "doony'") `shouldBe` (getCateFromString "s\\xnp", ">T->Bx", "Frank' doony'")

    it "The result of backward raising and harmonic composition ((s/.np)\\#np, \"give'\") with (np, \"Frank'\") is (s\\#np, \"<T-<B\", \"Frank' give'\")" $ do
      raiBh (getCateFromString "(s/.np)\\#np", "give'") (npCate, "Frank'") `shouldBe` (getCateFromString "s\\#np", "<T-<B", "Frank' give'")

    it "The result of backward raising and crossing composition ((s/.np)/.s, \"let'\") with (np, \"him'\") is (s/.s, \"<T-<Bx\", \"him' let'\")" $ do
      raiBc (getCateFromString "(s/.np)/.s", "let'") (npCate, "him'") `shouldBe` (getCateFromString "s/.s", "<T-<Bx", "him' let'")

    it "The result of backward raising and crossing composition ((s/.np)/#s, \"let'\") with (np, \"him'\") is (Nil, \"<T-<Bx\",\"\")" $ do
      raiBc (getCateFromString "(s/.np)/#s", "let'") (npCate, "him'") `shouldBe` (getCateFromString "Nil", "<T-<Bx", "")



