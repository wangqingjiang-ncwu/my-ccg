-- Copyright China University of Water Resources and Electric Power (c) 2019-2024
-- All rights reserved.

module AmbiResolSpec where

import Category
import Phrase
import AmbiResol
import Test.Hspec

spec :: Spec
spec = do
  describe "AmbiResol" $ do
    it "The result of equal4OverPair (((0,1),[(\"s/.np\", \">T->B\", \"Frank' loves'\", \"OE\", True)],1), ((1,1),[(\"s\\.np\", \">\", \"loves' Mary'\", \"VO\", True)],1), Rp)  (((0,1),[(\"s/.np\", \">T->B\", \"Frank' loves'\", \"OE\", False)],1), ((1,1),[(\"s\\.np\", \">\", \"loves' Mary'\", \"VO\", True)],1), Rp) is True." $ do
      let c1 = getCateFromString "s/.np"
      let c2 = getCateFromString "s\\.np"
      let pc1 = createPhraCate 0 1 [(c1,">T->B","Frank' loves'","OE",True)] 1
      let pc2 = createPhraCate 1 1 [(c2,">","loves' Mary'","VO",True)] 2
      let prior1 = read "Rp" :: Prior
      let op1 = (pc1, pc2, prior1)
      let c3 = getCateFromString "s/.np"
      let c4 = getCateFromString "s\\.np"
      let pc3 = createPhraCate 0 1 [(c3,">T->B","Frank' loves'","OE",False)] 1
      let pc4 = createPhraCate 1 1 [(c4,">","loves' Mary'","VO",True)] 2
      let prior2 = read "Rp" :: Prior
      let op2 = (pc3, pc4, prior2)
      equal4OverPair op1 op2 `shouldBe` True

    it "The result of elem4OverPair (((0,1),[(\"s/.np\", \">T->B\", \"Frank' loves'\", \"OE\", True)],1), ((1,1),[(\"s\\.np\", \">\", \"loves' Mary'\", \"VO\", True)],1), Rp) [(((0,1),[(\"s/.np\", \">T->B\", \"Frank' loves'\", \"OE\", True)],1), ((1,1),[(\"s\\.np\", \">\", \"loves' Mary'\", \"VO\", True)],1), Rp), (((0,1),[(\"s/.np\", \">T->B\", \"Frank' loves'\", \"OE\", False)],1), ((1,1),[(\"s\\.np\", \">\", \"loves' Mary'\", \"VO\", True)],1), Rp)] is True." $ do
      let c1 = getCateFromString "s/.np"
      let c2 = getCateFromString "s\\.np"
      let pc1 = createPhraCate 0 1 [(c1,">T->B","Frank' loves'","OE",True)] 1
      let pc2 = createPhraCate 1 1 [(c2,">","loves' Mary'","VO",True)] 2
      let prior1 = read "Rp" :: Prior
      let op1 = (pc1, pc2, prior1)
      let c3 = getCateFromString "s/.np"
      let c4 = getCateFromString "s\\.np"
      let pc3 = createPhraCate 0 1 [(c3,">T->B","Frank' loves'","OE",False)] 1
      let pc4 = createPhraCate 1 1 [(c4,">","loves' Mary'","VO",True)] 2
      let prior2 = read "Rp" :: Prior
      let op2 = (pc3, pc4, prior2)
      let opClo = [op1, op2]
      elem4OverPair op1 opClo `shouldBe` True

    it "The result of removeDup4OverPair [(((0,1),[(\"s/.np\", \">T->B\", \"Frank' loves'\", \"OE\", True)],1), ((1,1),[(\"s\\.np\", \">\", \"loves' Mary'\", \"VO\", True)],1), Rp), (((0,1),[(\"s/.np\", \">T->B\", \"Frank' loves'\", \"OE\", False)],1), ((1,1),[(\"s\\.np\", \">\", \"loves' Mary'\", \"VO\", True)],1), Rp)] is [(((0,1),[(\"s/.np\", \">T->B\", \"Frank' loves'\", \"OE\", False)],1), ((1,1),[(\"s\\.np\", \">\", \"loves' Mary'\", \"VO\", True)],1), Rp)]." $ do
      let c1 = getCateFromString "s/.np"
      let c2 = getCateFromString "s\\.np"
      let pc1 = createPhraCate 0 1 [(c1,">T->B","Frank' loves'","OE",True)] 1
      let pc2 = createPhraCate 1 1 [(c2,">","loves' Mary'","VO",True)] 2
      let prior1 = read "Rp" :: Prior
      let op1 = (pc1, pc2, prior1)
      let c3 = getCateFromString "s/.np"
      let c4 = getCateFromString "s\\.np"
      let pc3 = createPhraCate 0 1 [(c3,">T->B","Frank' loves'","OE",False)] 1
      let pc4 = createPhraCate 1 1 [(c4,">","loves' Mary'","VO",True)] 2
      let prior2 = read "Rp" :: Prior
      let op2 = (pc3, pc4, prior2)
      let opClo = [op1, op2]
      removeDup4OverPair opClo `shouldBe` [op2]

    it "The result of hasDup4OverPair [(((0,1),[(\"s/.np\", \">T->B\", \"Frank' loves'\", \"OE\", True)],1), ((1,1),[(\"s\\.np\", \">\", \"loves' Mary'\", \"VO\", True)],1), Rp), (((0,1),[(\"s/.np\", \">T->B\", \"Frank' loves'\", \"OE\", True)],1), ((1,1),[(\"s\\.np\", \">\", \"loves' Mary'\", \"VO\", True)],1), Rp)] is True." $ do
      let c1 = getCateFromString "s/.np"
      let c2 = getCateFromString "s\\.np"
      let pc1 = createPhraCate 0 1 [(c1,">T->B","Frank' loves'","OE",True)] 1
      let pc2 = createPhraCate 1 1 [(c2,">","loves' Mary'","VO",True)] 2
      let prior1 = read "Rp" :: Prior
      let op1 = (pc1, pc2, prior1)
      let c3 = getCateFromString "s/.np"
      let c4 = getCateFromString "s\\.np"
      let pc3 = createPhraCate 0 1 [(c3,">T->B","Frank' loves'","OE",False)] 1
      let pc4 = createPhraCate 1 1 [(c4,">","loves' Mary'","VO",True)] 2
      let prior2 = read "Rp" :: Prior
      let op2 = (pc3, pc4, prior2)
      let opClo = [op1, op2]
      hasDup4OverPair opClo `shouldBe` True

    it "The result of (==) (np, \">\", \"AHn\") (np, \">\", \"AHn\") is True." $ do
      let phraSyn1 = (npCate, ">", "AHn")
      (==) phraSyn1 phraSyn1 `shouldBe` True
