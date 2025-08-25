-- Copyright (c) 2019-2025 China University of Water Resources and Electric Power
-- All rights reserved.

module AmbiResolSpec where

import Category
import Phrase
import AmbiResol
import Utils
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

    it "The result of (<) (np, \">\", \"AHn\") (s/.np, \">\", \"AHn\") is True." $ do
      let phraSyn1 = (npCate, ">", "AHn")
      let phraSyn2 = (objectExtractionCate, ">", "AHn")
      (<) phraSyn1 phraSyn2 `shouldBe` True

    it "The result of (<) (s, \">\", \"AHn\") (np/.np, \">\", \"AHn\") is True." $ do
      let phraSyn1 = (sCate, ">", "AHn")
      let phraSyn2 = (adjCate, ">", "AHn")
      (<) phraSyn1 phraSyn2 `shouldBe` True

    it "The result of (>) (np, \">\", \"AHn\") (np, \"<\", \"AHn\") is True." $ do
      let phraSyn1 = (npCate, ">", "AHn")
      let phraSyn2 = (npCate, "<", "AHn")
      (>) phraSyn1 phraSyn2 `shouldBe` True

    it "The result of phraCateTree2PhraSynTree Empty is Empty." $ do
      phraCateTree2PhraSynTree Empty `shouldBe` Empty

    it "The result of phraCateTree2PhraSynTree (Node ((0,2),[(s,<,((loves' Mary') Frank'),SP,True),1) (Node ((0,0),[(np,Desig,Frank',DE,False)],0) (Node ((1,1),[(s\\.np,>,(loves' Mary'),VO,False)],2) Empty Empty) is (Node (s,>,SP,2) (Node (np,Desig,DE,0) Empty Empty) (Node (s\\.np,>,VO,1) Empty Empty))." $ do
      let c0 = npCate
      let c1 = getCateFromString "s/.np"
      let c2 = getCateFromString "s\\.np"
      let pc00 = createPhraCate 0 0 [(c0,"Desig","Frank'","DE",False)] 0
      let pc01 = createPhraCate 0 1 [(c1,">T->B","(Frank' loves')","OE",False)] 1
      let pc11 = createPhraCate 1 1 [(c2,">","(loves' Mary')","VO",False)] 2
      let pc021 = createPhraCate 0 2 [(sCate,"<","((loves' Mary') Frank')","SP",True)] 1
      let pc022 = createPhraCate 0 2 [(sCate,"<","((Frank' loves') Mary'))","NR",True)] 2
      let phraSyn0 = (c0, "Desig", "DE", 0)
      let phraSyn01 = (c1, ">T->B", "OE", 1)
      let phraSyn11 = (c2, ">", "VO", 1)
      let phraSyn021 = (sCate, "<", "SP", 2)
      let phraSyn022 = (sCate, ">", "NR", 2)
      phraCateTree2PhraSynTree (Node pc021 (Node pc00 Empty Empty) (Node pc11 Empty Empty)) `shouldBe` (Node phraSyn021 (Node phraSyn0 Empty Empty) (Node phraSyn11 Empty Empty))
