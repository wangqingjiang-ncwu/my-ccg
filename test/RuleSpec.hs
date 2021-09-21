-- Copyright China University of Water Resources and Electric Power (c) 2019
-- All rights reserved.

module RuleSpec where

import Category
import Rule
import Test.Hspec

spec :: Spec
spec = do
  describe "Rule" $ do
    it "The result of forwardly application (s/.np, \"eat'\",\"DE\") to (np, \"food'\",\"DE\") is (s,\">\", \"eat' food'\",\"NR\",True)" $ do
      appF (getCateFromString "s/.np", "eat'","DE") (npCate, "food'","DE") `shouldBe`  (sCate, ">", "eat' food'", "NR", True)

    it "The result of forwardly application (s, \"(eats' food') Mary'\", \"DE\") to (np, \"Frank'\",\"DE\") is (Nil, \">\", \"\", \"\", False)" $ do
      appF (sCate, "(eats' food') Mary'","DE") (npCate, "Frank'","DE") `shouldBe` (nilCate, ">", "", "", False)

    it "The result of backwardly application (np, \"Frank'\",\"DE\") to (s\\.np, \"eats' food'\",\"DE\") is (s,\"<\", \"(eats' food') Frank'\", \"SP\", True)" $ do
      appB (npCate, "Frank'","DE") (getCateFromString "s\\.np", "eats' food'","DE") `shouldBe` (sCate,"<","(eats' food') Frank'", "SP", True)

    it "The result of forward harmonic composition ((s\\.np)/#(s\\.np), \"very'\",\"DE\") with ((s\\.np)/.np,\"hates'\",\"DE\") is ((s\\.np)/.np, \">B\", \"very' hates'\", \"DHv\",True)" $ do
      comFh (getCateFromString "(s\\.np)/#(s\\.np)", "very'","DE") (getCateFromString "(s\\.np)/.np", "hates'","DE") `shouldBe` (getCateFromString "(s\\.np)/.np", ">B", "very' hates'", "DHv", True)

    it "The result of forward harmonic composition ((s\\.np)/*(s\\.np), \"very'\",\"DE\") with ((s\\.np)/.np, \"hates'\",\"DE\") is (Nil, \">B\", \"\", \"\", False)" $ do
      comFh (getCateFromString "(s\\.np)/*(s\\.np)", "very'","DE") (getCateFromString "(s\\.np)/.np", "hates'","DE") `shouldBe` (getCateFromString "Nil", ">B", "", "", False)

    it "The result of forward harmonic composition^2 ((s\\.np)/#(s\\.np) \"quickly'\",\"DE\") with (((s\\.np)/#np)/.np), \"give'\",\"DE\") is (((s\\.np)/#np/).np, \">B2\", \"quickly' give'\", \"DHv\", True)" $ do
      comFh2 (getCateFromString "(s\\.np)/#(s\\.np)", "quickly'","DE") (getCateFromString "((s\\.np)/#np)/.np", "give'","DE") `shouldBe` (getCateFromString "((s\\.np)/#np)/.np", ">B2", "quickly' give'", "DHv", True)

    it "The result of backward harmonic composition ((s\\.np)\\.np \"doony'\",\"DE\") with ((s\\.np)\\#(s\\.np), \"well'\",\"DE\") is ((s\\.np)\\.np, \"<B\", \"well' doony'\", \"NR\", True)" $ do
      comBh (getCateFromString "(s\\.np)\\.np", "doony'","DE") (getCateFromString "(s\\.np)\\#(s\\.np)", "well'","DE") `shouldBe` (getCateFromString "(s\\.np)\\.np", "<B", "well' doony'", "NR", True)

    it "The result of forward crossing composition ((s\\.np)/x(s\\.np), \"very'\",\"DE\") with ((s\\.np)\\xnp, \"doony'\",\"DE\") is ((s\\.np)\\xnp, \">Bx\", \"very' doony'\", \"NR\", True)" $ do
      comFc (getCateFromString "(s\\.np)/x(s\\.np)", "very'","DE") (getCateFromString "(s\\.np)\\xnp", "doony'","DE") `shouldBe` (getCateFromString "(s\\.np)\\xnp", ">Bx", "very' doony'", "NR", True)

    it "The result of backward crossing composition ((s\\.np)/.np, \"do'\",\"DE\") with ((s\\.np)\\x(s\\.np), \"well'\",\"DE\") is ((s\\.np)/.np, \"<Bx\", \"well' do'\", \"HvC\", True)" $ do
      comBc (getCateFromString "(s\\.np)/.np", "do'","DE") (getCateFromString "(s\\.np)\\x(s\\.np)", "well'","DE") `shouldBe` (getCateFromString "(s\\.np)/.np", "<Bx", "well' do'", "HvC", True)

    it "The result of forward raising and harmonic composition (np, \"I'\",\"DE\") with ((s\\.np)/.np, \"eat'\",\"DE\") is (s/.np, \">T->B\", \"eat' I'\", \"OE\", True)" $ do
      raiFh (npCate, "I'","DE") (getCateFromString "(s\\.np)/.np", "eat'","DE") `shouldBe` (getCateFromString "s/.np", ">T->B", "eat' I'", "OE", True)

    it "The result of forward raising and harmonic composition (np, \"I'\",\"DE\") with (s\\.np, \"watch' TV'\",\"DE\") is (Nil, \">T->B\", \"\", \"\", False)" $ do
      raiFh (npCate, "I'","DE") (getCateFromString "s\\.np", "watch' TV'","DE") `shouldBe` (nilCate, ">T->B", "", "", False)

    it "The result of forward raising and harmonic composition (np, \"Frank'\",\"DE\") with ((s\\.np)/.np, \"give'\",\"DE\") is ((s/.np, \">T->B\", \"give' Frank'\", \"OE\", True)" $ do
      raiFh (npCate, "Frank'","DE") (getCateFromString "(s\\.np)/.np", "give'","DE") `shouldBe` (getCateFromString "s/.np", ">T->B", "give' Frank'","OE",True)

    it "The result of fordward raising and crossing composition (np, \"Frank'\",\"DE\") with ((s\\.np)\\xnp, \"doony'\",\"DE\") is (s\\xnp, \">T->Bx\", \"doony' Frank'\", \"NR\", True)" $ do
      raiFc (npCate, "Frank'","DE") (getCateFromString "(s\\.np)\\xnp", "doony'","DE") `shouldBe` (getCateFromString "s\\xnp", ">T->Bx", "doony' Frank'","NR",True)

    it "The result of backward raising and harmonic composition ((s/.np)\\#np, \"give'\",\"DE\") with (np, \"Frank'\",\"DE\") is (s\\#np, \"<T-<B\", \"give' Frank'\", \"NR\", True)" $ do
      raiBh (getCateFromString "(s/.np)\\#np", "give'","DE") (npCate, "Frank'","DE") `shouldBe` (getCateFromString "s\\#np", "<T-<B", "give' Frank'","NR",True)

    it "The result of backward raising and crossing composition ((s/.np)/.s, \"let'\",\"DE\") with (np, \"him'\",\"DE\") is (s/.s, \"<T-<Bx\", \"let' him'\", \"NR\",True)" $ do
      raiBc (getCateFromString "(s/.np)/.s", "let'","DE") (npCate, "him'","DE") `shouldBe` (getCateFromString "s/.s", "<T-<Bx", "let' him'", "NR", True)

    it "The result of backward raising and crossing composition ((s/.np)/#s, \"let'\",\"DE\") with (np, \"him'\",\"DE\") is (Nil, \"<T-<Bx\",\"\", \"\", False)" $ do
      raiBc (getCateFromString "(s/.np)/#s", "let'","DE") (npCate, "him'","DE") `shouldBe` (nilCate, "<T-<Bx", "", "", False)
