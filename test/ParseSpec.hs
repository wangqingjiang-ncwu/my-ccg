-- Copyright China University of Water Resources and Electric Power (c) 2019
-- All rights reserved.

module ParseSpec where

import Category
import Rule
import Parse
import Test.Hspec

spec :: Spec
spec = do
  describe "Parse" $ do
    it "The result of initPhraCate [] is []" $ do
      initPhraCate [] `shouldBe` ([] :: [PhraCate])

    it "The result of initPhraCate [s] is [((0,0),[(s,\"Desig\",\"Frank'\")],0)]" $ do
      initPhraCate [(sCate, "Frank'")] `shouldBe` ([((0,0),[(sCate,"Desig","Frank'")],0)] :: [PhraCate])

    it "The result of initPhraCate [np, (s\\.np)/.np, np] is [((0,0),[(np,\"Desig\",\"Frank'\")],0),((1,0),[((s\\.np)/.np,\"Desig\",\"loves'\")],1),((2,0),[(np,\"Desig\",\"Mary'\")],2)]" $ do
      let c1 = npCate
      let c2 = getCateFromString "(s\\.np)/.np"
      let c3 = npCate
      initPhraCate [(c1,"Frank'"),(c2,"loves'"),(c3,"Mary'")] `shouldBe` ([((0,0),[(c1,"Desig","Frank'")],0),((1,0),[(c2,"Desig","loves'")],1),((2,0),[(c3,"Desig","Mary'")],2)] :: [PhraCate])

    it "The result of createPhraCate 0 1 s \"Np/s\" \"smiles' Frank'\" 1 is ((0,1),[(s, \"Np/s\", \"smiles' Frank'\")],1)" $ do
      let c = sCate
      createPhraCate 0 1 c "Np/s" "smiles' Frank'" 1 `shouldBe` (((0,1),[(c,"Np/s","smiles' Frank'")],1)::PhraCate)

    it "The result of applying func pclt to ((0,0),[(np,\"Desig\",\"Frank'\")],0) and ((1,0),[((s\\.np)/.np,\"Desig\",\"loves'\")],1) is True" $ do
      let c1 = npCate
      let c2 = getCateFromString "(s\\.np)/.np"
      let pcs = initPhraCate [(c1,"Frank'"), (c2,"smile'")]
      let pc1 = head pcs
      let pc2 = last pcs
      pclt pc1 pc2 `shouldBe` (True :: Bool)

    it "The result of applying func pclt to ((0,0),[(np,\"Desig\",\"Frank'\")],0) and ((0,0),[(s/.np,\"Desig\",\"loves' Mary'\")],0) is False" $ do
      let c1 = npCate
      let c2 = getCateFromString "s/.np"
      let pcs1 = initPhraCate [(c1,"Frank'")]
      let pc1 = head pcs1
      let pcs2 = initPhraCate [(c2,"loves' Mary'")]
      let pc2 = head pcs2
      pclt pc1 pc2 `shouldBe` (False :: Bool)

    it "The result of pcBelong ((1,0),[(s\\.np)/.np, \"Desig\", \"likes'\")],1) ((1,0),[(s\\.np)/.np, \"Desig\", \"likes'\")],1) is True" $ do
      let pc1 = createPhraCate 1 0 (getCateFromString "(s\\.np)/.np") "Desig" "likes'" 1
      let pc2 = createPhraCate 1 0 (getCateFromString "(s\\.np)/.np") "Desig" "likes'" 1
      pcBelong pc1 pc2 `shouldBe` True

    it "The result of pcBelong ((1,0),[(s\\.np)/.np, \"Desig\", \"likes'\")],1) ((1,0),[(s\\.np)/.np, \"Desig\", \"likes'\"),(np, \">\",\"book'\")],1) is True" $ do
      let cate = getCateFromString "(s\\.np)/.np"
      let pc1 = createPhraCate 1 0 cate "Desig" "likes'" 1
      let pc2 = createPhraCate2 1 0 [(cate,"Desig","likes'"),(npCate,">","book'")] 1
      pcBelong pc1 pc2 `shouldBe` True

    it "The result of pcBelong ((1,0),[(s\\.np)/.np, \"Desig\", \"likes'\"),(np, \">\",\"book'\")],1) ((1,0),[(s\\.np)/.np, \"Desig\", \"likes'\")],1) is False" $ do
      let cate = getCateFromString "(s\\.np)/.np"
      let pc1 = createPhraCate 1 0 cate "Desig" "likes'" 1
      let pc2 = createPhraCate2 1 0 [(cate,"Desig","likes'"),(npCate,">","book'")] 1
      pcBelong pc2 pc1 `shouldBe` False

    it "The result of stOfCate ((1,0),[(s\\.np, \"Desig\", \"smiles'\")],1) is 1" $ do
      let c = getCateFromString "s\\.np"
      let pc = createPhraCate 1 0 c "Desig" "smiles'" 1
      stOfCate pc `shouldBe` 1

    it "The result of spOfCate ((1,0),[(s\\.np, \"Desig\", \"smiles'\")],1) is 0" $ do
      let c = getCateFromString "s\\.np"
      let pc = createPhraCate 1 0 c "Desig" "smiles'" 1
      spOfCate pc `shouldBe` 0

    it "The result of ssOfCate ((1,0),[(s\\.np, \"Desig\", \"smiles'\")],1) is 1" $ do
      let c = getCateFromString "s\\.np"
      let pc = createPhraCate 1 0 c "Desig" "smiles'" 1
      ssOfCate pc `shouldBe` 1

    it "The result of ctsOfCate ((1,0),[(s\\.np, \"Desig\", \"smiles'\")],1) is [(s\\.np, \"Desig\", \"smiles'\")]" $ do
      let c = getCateFromString "s\\.np"
      let pc = createPhraCate 1 0 c "Desig" "smiles'" 1
      ctsOfCate pc `shouldBe` [(c,"Desig","smiles'")]

    it "The result of caOfCate ((1,0),[(s\\.np, \"Desig\", \"smiles'\")],1) is [s\\.np]" $ do
      let c = getCateFromString "s\\.np"
      let pc = createPhraCate 1 0 c "Desig" "smiles'" 1
      caOfCate pc `shouldBe` [c]
   
    it "The result of cateComb ((0,0),[(np, \"Desig\", \"Frank'\")],0) ((1,0),[(s\\.np, \"Desig\", \"smiles\")],1) is ((0,1),[(s, \"<\", \"smiles' Frank'\")],1)" $ do
      let c1 = npCate
      let c2 = getCateFromString "s\\.np"
      let c31 = sCate
      let c32 = getCateFromString "s/.np"
      let pc1 = createPhraCate 0 0 c1 "Desig" "Frank'" 0
      let pc2 = createPhraCate 1 0 c2 "Desig" "smiles'" 1
      let pc3 = createPhraCate2 0 1 [(c31,"<","smiles' Frank'"),(c32,"A/n-<Bx","smiles' Frank'")] 1
      cateComb pc1 pc2 `shouldBe` pc3

    it "The result of cateComb ((0,0),[(np, \"Desig\", \"Frank'\")],0) ((1,0),[(s\\.np,\"Desig\",\"smiles'\")],1) is NOT ((0,1),[(np,\"Desig\",\"smiles' Frank'\"],1)" $ do
      let c1 = npCate
      let c2 = getCateFromString "s\\.np"
      let c3 = npCate
      let pc1 = createPhraCate 0 0 c1 "Desig" "Frank'"  0
      let pc2 = createPhraCate 1 0 c2 "Desig" "smiles'" 1
      let pc3 = createPhraCate 0 1 c3 "Desig" "smiles' Frank'" 1
      cateComb pc1 pc2 /= pc3

    it "The result of cateComb ((0,0),[(np,\"Desig\",\"Frank'\")],0) ((1,0),[((s\\.np)/.np,\"Desig\",\"loves'\")],1) is ((0,1),[(s/.np,\">T->B\"),\"Frank' loves\"],1)" $ do
      let c1 = npCate
      let c2 = getCateFromString "(s\\.np)/.np"
      let c3 = getCateFromString "s/.np"
      let pc1 = createPhraCate 0 0 c1 "Desig" "Frank'" 0
      let pc2 = createPhraCate 1 0 c2 "Desig" "loves'" 1
      let pc3 = createPhraCate 0 1 c3 ">T->B" "Frank' loves'" 1
      cateComb pc1 pc2 `shouldBe` pc3

    it "The result of cateComb ((0,0),[(s,\"Desig\",\"Frank_cries'\")],0) ((1,0),[(s\\.np,\"Desig\",\"is_bad'\")],1) is ((0,1),[(s,\"Np/s-<\",\"is_bad' Frank_cries'\")],1)" $ do
      let c1 = sCate
      let c2 = getCateFromString "s\\.np"
      let c3 = sCate
      let pc1 = createPhraCate 0 0 c1 "Desig" "Frank_cries'" 0
      let pc2 = createPhraCate 1 0 c2 "Desig" "is_bad'" 1
      let pc3 = createPhraCate 0 1 c3 "Np/s-<" "is_bad' Frank_cries'" 1
      cateComb pc1 pc2 `shouldBe` pc3
    
    it "The result of cateComb ((0,0),[((s\\.np)/.np,\"Desig\",\"loves'\")],0) ((1,0),[(s,\"Desig\",\"Frank_does_it'\")],1) is ((0,1),[(s\\.np,\"Np/s->\",\"loves' Frank_does_it'\")],1)" $ do
      let c1 = getCateFromString "(s\\.np)/.np"
      let c2 = sCate
      let c3 = getCateFromString "s\\.np"
      let pc1 = createPhraCate 0 0 c1 "Desig" "loves'" 0
      let pc2 = createPhraCate 1 0 c2 "Desig" "Frank_does_it'" 1
      let pc3 = createPhraCate 0 1 c3 "Np/s->" "loves' Frank_does_it'" 1
      cateComb pc1 pc2 `shouldBe` pc3
    
    it "The result of cateComb ((0,0),[((s\\.np)/.np,\"Desig\",\"loving'\")],0) ((1,0),[(s\\.np,\"Desig\",\"is_forever'\")],1) is ((0,1),[(s,\"Np/v-<\",\"is_forever' loving'\"),(s\\.np,\"Np/v->\",\"loving' is_forever'\")],1)" $ do
      let c1 = getCateFromString "(s\\.np)/.np"
      let c2 = getCateFromString "s\\.np"
      let c3 = sCate
      let c4 = getCateFromString "s\\.np"
      let pc1 = createPhraCate 0 0 c1 "Desig" "loving'" 0
      let pc2 = createPhraCate 1 0 c2 "Desig" "is_forever'" 1
      let pc3 = createPhraCate2 0 1 [(c3,"Np/v-<","is_forever' loving'"),(c4,"Np/v->","loving' is_forever'")] 1
      cateComb pc1 pc2 `shouldBe` pc3

    it "The result of cateComb ((0,0),[(np/.np,\"Desig\",\"Good'\")],0) ((1,1),[((s\\.np)/.np,\">B\",\"is_better_than' bad'\"), (s\\.np,\">\",\"is_better_than' bad'\")],2) is ((0,2),[(s/.np,\"<Bx\",\"(is_better_than' bad') Good'\"),(np,\"Np/v->\",\"Good' (is_better_than' bad')\"),(s,\"Np/a-<\",\"(is_better_than' bad') Good'\"),(s/.np,\"Np/a->T->B\",\"Good' (is_better_than' bad')\")],1)" $ do
      let c1 = getCateFromString "np/.np"
      let c21 = getCateFromString "(s\\.np)/.np"
      let c22 = getCateFromString "s\\.np"
      let c31 = getCateFromString "s/.np"
      let c34 = c31
      let pc1 = createPhraCate 0 0 c1 "Desig" "Good'" 0
      let pc2 = createPhraCate2 1 1 [(c21,">B","is_better_than' bad'"),(c22,">","is_better_than' bad'")] 2
      let pc3 = createPhraCate2 0 2 [(c31,"<Bx","(is_better_than' bad') Good'"),(npCate,"Np/v->","Good' (is_better_than' bad')"),(sCate,"Np/a-<","(is_better_than' bad') Good'"),(c34,"Np/a->T->B","Good' (is_better_than' bad')")] 1
      cateComb pc1 pc2 `shouldBe` pc3

    
    it "The result of getNuOfInputCates ((0,0),[(np,\"Desig\",\"Frank'\")],0) ((1,0),[((s\\.np)/.np,\"Desig\",\"loves'\")],1) ((0,1),[(s/.np,\"<\",\"loves' Frank'\")],1) is 2" $ do
      let c1 = npCate
      let c2 = getCateFromString "(s\\.np)/.np"
      let c3 = getCateFromString "s/.np"
      let pc1 = createPhraCate 0 0 c1 "Desig" "Frank'" 0
      let pc2 = createPhraCate 1 0 c2 "Desig" "loves'" 1
      let pc3 = createPhraCate 0 1 c3 "<" "loves' Frank'" 1
      let pcs = [pc1,pc2,pc3]
      getNuOfInputCates pcs `shouldBe` 2

    it "The result of parse [((0,0),[(np,\"Desig\",\"Frank'\")],0), ((1,0),[((s\\.np)/.np,\"Desig\",\"loves'\")],1)] is [((0,0),[(np,\"Desig\",\"Frank'\")],0), ((1,0),[(s\\.np)/.np,\"Desig\",\"loves'\")],1), ((0,1),[(s/.np,\">T->B\",\"Frank' loves'\")],1)]" $ do
      let c1 = npCate
      let c2 = getCateFromString "(s\\.np)/.np"
      let c3 = getCateFromString "s/.np"
      let pc1 = createPhraCate 0 0 c1 "Desig" "Frank'" 0
      let pc2 = createPhraCate 1 0 c2 "Desig" "loves'" 1
      let pc3 = createPhraCate 0 1 c3 ">T->B" "Frank' loves'" 1
      let pcs = [pc1,pc2]
      let pcClo = pcs ++ [pc3]
      parse pcs `shouldBe` pcClo

    it "The result of parse [((0,0),[(np,\"Desig\",\"Frank'\")],0), ((1,0),[((s\\.np)/.np,\"Desig\",\"loves'\")],1), ((2,0),[(np,\"Desig\",\"Mary'\")],2)] is [((0,0),[(np,\"Desig\",\"Frank'\")],0), ((1,0),[((s\\.np)/.np,\"Desig\",\"loves'\")],1), ((2,0),[(np,\"Desig\",\"Mary'\")],2), ((0,1),[(s/.np,\">T->B\",\"Frank' loves'\")],1), ((1,1),[(s\\.np,\">\",\"loves' Mary'\"),((s\\.np)/.np,\"A/n->B\",\"loves' Mary'\")],2), ((0,2),[(s,\"<\",\"(loves' Mary') Frank'\"),(s/.np,\">T->B\",\"Frank' (loves' Mary')\"),(s/.np,\"A/n-<Bx\",\"(loves' Mary') Frank'\")],1), ((0,2),[(s,\">\",\"(Frank' loves') Mary'\"),(s/.np,\"A/n->B\",\"(Frank' loves') Mary'\")],2)], without considering element order." $ do
      let c01 = npCate
      let c02 = getCateFromString "(s\\.np)/.np"
      let c03 = npCate
      let c11 = getCateFromString "s/.np"
      let c121 = getCateFromString "s\\.np"
      let c122 = getCateFromString "(s\\.np)/.np"
      let c211 = sCate
      let c212 = getCateFromString "s/.np"
      let c213 = getCateFromString "s/.np"
      let c221 = sCate
      let c222 = getCateFromString "s/.np"
      let pc01 = createPhraCate 0 0 c01 "Desig" "Frank'" 0
      let pc02 = createPhraCate 1 0 c02 "Desig" "loves'" 1
      let pc03 = createPhraCate 2 0 c03 "Desig" "Mary'" 2
      let pc11 = createPhraCate 0 1 c11 ">T->B" "Frank' loves'" 1
      let pc12 = createPhraCate2 1 1 [(c121,">","loves' Mary'"),(c122,"A/n->B","loves' Mary'")] 2
      let pc21 = createPhraCate2 0 2 [(c211,"<","(loves' Mary') Frank'"),(c212,">T->B","Frank' (loves' Mary')"),(c213,"A/n-<Bx","(loves' Mary') Frank'")] 1
      let pc22 = createPhraCate2 0 2 [(c221,">","(Frank' loves') Mary'"),(c222,"A/n->B","(Frank' loves') Mary'")] 2 
      let pcs = [pc01,pc02,pc03]
      let pcClo = pcs ++ [pc11,pc12,pc21,pc22]
      parse pcs `shouldBe` pcClo

    it "The result of parse [((0,0),[(np/.np,\"Desig\",\"Brave'\")],0), ((1,0),[(np,\"Desig\",\"Frank'\")],1), ((2,0),[(s\\*np,\"Desig\",\"wins'\")],2)] is [((0,0),[(np/.np,\"Desig\",\"Brave'\")],0), ((1,0),[(np,\"Desig\",\"Frank'\")],1), ((2,0),[(s\\*np,\"Desig\",\"wins'\")],2), ((0,1),[(np,\">\",\"Brave' Frank'\")],1), ((1,1),[(s,\"<\",\"wins' Frank'\"),(np/.np,\"A/n->B\",\"Brave' Frank'\")],2), ((0,2),[(np,\"Np/s->\",\"Brave' (wins' Frank')\")],1), ((0,2),[(s,\"<\",\"wins' (Brave' Frank')\"),(np,\"Np/v->\",\"(Brave' Frank') wins'\"),(s,\"Np/a-<\",\"wins' (Brave' Frank')\")],2)], without considering element order." $ do
      let c01 = getCateFromString "np/.np"
      let c02 = npCate
      let c03 = getCateFromString "s\\*np"
      let c111 = npCate
      let c112 = getCateFromString "np/.np"
      let c12 = sCate
      let c21 = npCate
      let c221 = sCate
      let c222 = npCate
      let c223 = sCate
      let pc01 = createPhraCate 0 0 c01 "Desig" "Brave'" 0
      let pc02 = createPhraCate 1 0 c02 "Desig" "Frank'" 1
      let pc03 = createPhraCate 2 0 c03 "Desig" "wins'" 2
      let pc11 = createPhraCate2 0 1 [(c111,">","Brave' Frank'"),(c112,"A/n->B","Brave' Frank'")] 1
      let pc12 = createPhraCate 1 1 c12 "<" "wins' Frank'" 2
      let pc21 = createPhraCate 0 2 c21 "Np/s->" "Brave' (wins' Frank')" 1
      let pc22 = createPhraCate2 0 2 [(c221,"<","wins' (Brave' Frank')"),(c222,"Np/v->","(Brave' Frank') wins'"),(c223,"Np/a-<","wins' (Brave' Frank')")] 2 
      let pcs = [pc01,pc02,pc03]
      let pcClo = pcs ++ [pc11,pc12,pc21,pc22]
      parse pcs `shouldBe` pcClo

    it "The result of atomizePhraCate [((0,0),[(np,\"Desig\",\"Frank\")],1), ((1,1),[(s\\.np,\">\",\"loves' Mary'\"),((s/.np,\">T->B\",\"Tim' loves'\"))],2)] is [((0,0),[(np,\"Desig\",\"Frank\")],1), ((1,1),[(s\\.np,\">\",\"loves' Mary'\"),2)],2), ((1,1),[(s/.np,\">T->B\",\"Tim' loves'\")],2)]" $ do
      let pc1 = createPhraCate 0 0 npCate "Desig" "Frank" 1
      let pc2 = createPhraCate2 1 1 [(getCateFromString "s\\.np",">","loves' Mary'"),(getCateFromString "s/.np",">T->B","Tom' loves'")] 2
      let pc21 = createPhraCate 1 1 (getCateFromString "s\\.np") ">" "loves' Mary'" 2
      let pc22 = createPhraCate 1 1 (getCateFromString "s/.np") ">T->B" "Tom' loves'" 2
      atomizePhraCate [pc1,pc2] `shouldBe` [pc1,pc21,pc22]

    it "The result of findCate （0,-1) [((0,0),[(np,\"Desig\",\"Frank'\")],0), ((1,0),[(s\\.np,\"Desig\",\"smiles\")],1)] is []" $ do
      let c1 = npCate
      let c2 = getCateFromString "s\\.np"
      findCate (0,-1) [((0,0),[(c1,"Desig","Frank'")],0), ((1,0),[(c2,"Desig","smiles'")],1)] `shouldBe` []

    it "The result of findCate （0,0) [((0,0),[(np,\"Desig\",\"Frank'\")],0), ((1,0),[(s\\.np,\"Desig\",\"loves' Mary'\")],1)] is ((0,0),[(np,\"Desig\",\"Frank'\")],0)" $ do
      let c1 = npCate
      let c2 = getCateFromString "s\\.np"
      let pc1 = createPhraCate 0 0 c1 "Desig" "Frank'" 0
      let pc2 = createPhraCate 1 0 c2 "Desig" "loves' Mary'" 1
      findCate (0,0) [pc1, pc2] `shouldBe` [pc1]

    it "The result of findSplitCate ((1,0),[(np,\"Desig\",\"Frank'\")],1) [((0,0),[(np/.np,\"Desig\",\"Brave'\")],0), ((1,0),[(np,\"Desig\",\"Frank'\")],1), ((2,0),[(s\\*np,\"Desig\"),\"wins'\"],2), ((0,1),[(np,\">\",\"Brave' Frank'\")],1), ((1,1),[(s,\"<\",\"wins' Frank'\")],2), ((0,2),[np,\"Np/s->\",\"Brave' (wins' Frank')\"],1), ((0,2),[(s,\"<\",\"wins' (Brave' Frank')\")],2)] is []" $ do
      let c01 = getCateFromString "np/.np"
      let c02 = npCate
      let c03 = getCateFromString "s\\*np"
      let c11 = npCate
      let c12 = sCate
      let c21 = npCate
      let c22 = sCate
      let pc01 = createPhraCate 0 0 c01 "Desig" "Brave'" 0
      let pc02 = createPhraCate 1 0 c02 "Desig" "Frank'" 1
      let pc03 = createPhraCate 2 0 c03 "Desig" "wins'" 2
      let pc11 = createPhraCate 0 1 c11 ">" "Brave' Frank'" 1
      let pc12 = createPhraCate 1 1 c12 "<" "wins' Frank'" 2
      let pc21 = createPhraCate 0 2 c21 "Np/s->" "Brave' (wins' Frank')" 1
      let pc22 = createPhraCate 0 2 c22 "<" "wins' (Brave' Frank')" 2 
      let pcs = [pc01,pc02,pc03]
      let pcClo = pcs ++ [pc11,pc12,pc21,pc22]
      findSplitCate pc02 pcClo `shouldBe` []


    it "The result of findSplitCate ((0,2),[(s,\"<\",\"wins' (Brave' Frank')\")],2) [((0,0),[(np/.np,\"Desig\",\"Brave'\")],0), ((1,0),[(np,\"Desig\",\"Frank'\")],1), ((2,0),[(s\\*np,\"Desig\"),\"wins'\"],2), ((0,1),[(np,\">\",\"Brave' Frank'\")],1), ((1,1),[(s,\"<\",\"wins' Frank'\")],2), ((0,2),[np,\"Np/s->\",\"Brave' (wins' Frank')\"],1), ((0,2),[(s,\"<\",\"wins' (Brave' Frank')\")],2)] is [((0,1),[(np,\">\",\"Brave' Frank'\")],1), (2,0),[(s\\.np,\"Desig\",\"wins'\")],2)]" $ do
      let c01 = getCateFromString "np/.np"
      let c02 = npCate
      let c03 = getCateFromString "s\\*np"
      let c11 = npCate
      let c12 = sCate
      let c21 = npCate
      let c22 = sCate
      let pc01 = createPhraCate 0 0 c01 "Desig" "Brave'" 0
      let pc02 = createPhraCate 1 0 c02 "Desig" "Frank'" 1
      let pc03 = createPhraCate 2 0 c03 "Desig" "wins'" 2
      let pc11 = createPhraCate 0 1 c11 ">" "Brave' Frank'" 1
      let pc12 = createPhraCate 1 1 c12 "<" "wins' Frank'" 2
      let pc21 = createPhraCate 0 2 c21 "Np/s->" "Brave' (wins' Frank')" 1
      let pc22 = createPhraCate 0 2 c22 "<" "wins' (Brave' Frank')" 2 
      let pcs = [pc01,pc02,pc03]
      let pcClo = pcs ++ [pc11,pc12,pc21,pc22]
      findSplitCate pc22 pcClo `shouldBe` [(pc11,pc03)]
    
    it "The result of findSplitCate ((0,2),[(s,\"<\",\"wins' (Brave' Frank')\")],2) [((0,0),[(np/.np,\"Desig\",\"Brave'\")],0), ((1,0),[(np,\"Desig\",\"Frank'\")],1), ((2,0),[(s\\*np,\"Desig\"),\"wins'\"],2), ((0,1),[(np,\">\",\"Brave' Frank'\")],1), ((1,1),[(s,\"<\",\"wins' Frank'\")],2), ((0,2),[np,\"Np/s->\",\"Brave' (wins' Frank')\"],1), ((0,2),[(s,\"<\",\"wins' (Brave' Frank')\")],2)] is [((0,1),[(np,\">\",\"Brave' Frank'\")],1), (2,0),[(s\\.np,\"Desig\",\"wins'\")],2)]" $ do
      let c01 = getCateFromString "np/.np"
      let c02 = npCate
      let c03 = getCateFromString "s\\*np"
      let c11 = npCate
      let c12 = sCate
      let c21 = npCate
      let c22 = sCate
      let pc01 = createPhraCate 0 0 c01 "Desig" "Brave'" 0
      let pc02 = createPhraCate 1 0 c02 "Desig" "Frank'" 1
      let pc03 = createPhraCate 2 0 c03 "Desig" "wins'" 2
      let pc11 = createPhraCate 0 1 c11 ">" "Brave' Frank'" 1
      let pc12 = createPhraCate 1 1 c12 "<" "wins' Frank'" 2
      let pc21 = createPhraCate 0 2 c21 "Np/s->" "Brave' (wins' Frank')" 1
      let pc22 = createPhraCate 0 2 c22 "<" "wins' (Brave' Frank')" 2 
      let pcs = [pc01,pc02,pc03]
      let pcClo = pcs ++ [pc11,pc12,pc21,pc22]
      findSplitCate pc22 pcClo `shouldBe` [(pc11,pc03)]

    it "The result of findSplitCate ((1,1),[(s,\"<\",\"wins' Frank'\")],2) [((0,0),[(np/.np,\"Desig\",\"Brave'\")],0), ((1,0),[(np,\"Desig\",\"Frank'\")],1), ((2,0),[(s\\*np,\"Desig\"),\"wins'\"],2), ((0,1),[(np,\">\",\"Brave' Frank'\")],1), ((1,1),[(s,\"<\",\"wins' Frank'\")],2), ((0,2),[np,\"Np/s->\",\"Brave' (wins' Frank')\"],1), ((0,2),[(s,\"<\",\"wins' (Brave' Frank')\")],2)] is [((1,0),[(np,\"Desig\",\"Frank'\")],1), (2,0),[(s\\*np,\"Desig\",\"wins'\")],2)]" $ do
      let c01 = getCateFromString "np/.np"
      let c02 = npCate
      let c03 = getCateFromString "s\\*np"
      let c11 = npCate
      let c12 = sCate
      let c21 = npCate
      let c22 = sCate
      let pc01 = createPhraCate 0 0 c01 "Desig" "Brave'" 0
      let pc02 = createPhraCate 1 0 c02 "Desig" "Frank'" 1
      let pc03 = createPhraCate 2 0 c03 "Desig" "wins'" 2
      let pc11 = createPhraCate 0 1 c11 ">" "Brave' Frank'" 1
      let pc12 = createPhraCate 1 1 c12 "<" "wins' Frank'" 2
      let pc21 = createPhraCate 0 2 c21 "Np/s->" "Brave' (wins' Frank')" 1
      let pc22 = createPhraCate 0 2 c22 "<" "wins' (Brave' Frank')" 2
      let pcs = [pc01,pc02,pc03]
      let pcClo = pcs ++ [pc11,pc12,pc21,pc22]
      findSplitCate pc12 pcClo `shouldBe` [(pc02,pc03)]

 {-   it "The result of findSplitCate ((0,2),[(s,\"Np/a-<\",\"(is_better_than' bad') Good'\")],1)  [((0,0),[(np/.np,\"Desig\",\"Good'\")],0), ((1,0),[((s\\.np)/.np,\"Desig\",\"is_better_than'\")],1), ((2,0),[(np/.np,\"Desig\",\"bad'\")],2), ((0,1),[(np,\"Np/v->\",\"Good' is_better_than'\")],1), ((0,1),[(s/.np,\"Np/a->T->B\",\"Good' is_better_than'\")],1), ((1,1),[((s\\.np)/.np,\">B\",\"is_better_than' bad'\")],2), ((1,1),[(s\\.np,\"Np/a->\",\"is_better_than' bad'\")],2), ((0,2),[(np,\"Np/v->\",\"Good' (is_better_than' bad')\")],1), ((0,2),[(s,\"Np/a-<\",\"(is_better_than' bad') Good'\")],1), ((0,2),[(s/.np,\"Np/a->T->B\",\"Good' (is_better_than' bad')\")],1), ((0,2),[(s/.np,\">B\",\"(Good' is_better_than') bad'\")],2), ((0,2),[(s,\"Np/a->\",\"(Good' is_better_than') bad'\")],2)] is [(((0,0),[(np/.np,\"Desig\",\"Good'\")],0), ((1,1),[((s\\.np)/.np,\">B\",\"is_better_than' bad'\")],2)), (((0,0),[(np/.np,\"Desig\",\"Good'\"),0),((1,1),[(s\\.np,\"Np/a->\",\"is_better_than' bad'\")],2)]" $ do
      let c01 = getCateFromString "np/.np"
      let c02 = getCateFromString "(s\\.np)/.np"
      let c03 = getCateFromString "np/.np"
      let c11 = npCate
      let c12 = getCateFromString "s/.np"
      let c13 = getCateFromString "(s\\.np)/.np"
      let c14 = getCateFromString "s\\.np"
      let c21 = npCate
      let c22 = sCate
      let c23 = getCateFromString "s/.np"
      let c24 = getCateFromString "s/.np"
      let c25 = sCate
      let pc01 = createPhraCate 0 0 c01 "Desig" "Good'" 0
      let pc02 = createPhraCate 1 0 c02 "Desig" "is_better_than'" 1
      let pc03 = createPhraCate 2 0 c03 "Desig" "bad'" 2
      let pc11 = createPhraCate 0 1 c11 "Np/v->" "Good' is_better_than'" 1
      let pc12 = createPhraCate 0 1 c12 "Np/a->T->B" "Good' is_better_than'" 1
      let pc13 = createPhraCate 1 1 c13 ">B" "is_better_than' bad'" 2
      let pc14 = createPhraCate 1 1 c14 "Np/a->" "is_better_than' bad'" 2
      let pc21 = createPhraCate 0 2 c21 "Np/v->" "Good' (is_better_than' bad')" 1
      let pc22 = createPhraCate 0 2 c22 "Np/a-<" "(is_better_than' bad') Good'" 1
      let pc23 = createPhraCate 0 2 c23 "Np/a->T->B" "Good' (is_better_than' bad')" 1
      let pc24 = createPhraCate 0 2 c24 ">B" "(Good' is_better_than') bad'" 2
      let pc25 = createPhraCate 0 2 c25 "Np/a->" "(Good' is_better_than') bad'" 2
      let pcClo = [pc01,pc02,pc03,pc11,pc12,pc13,pc14,pc21,pc22,pc23,pc24,pc25]
      findSplitCate pc21 pcClo `shouldBe` [(pc01,pc13),(pc01,pc14)]
-}
    it "The result of findTipsOfTree [((1,1),[(s,\"<\",\"wins' Frank'\")],2)] [((0,0),[(np/.np,\"Desig\",\"Brave'\")],0), ((1,0),[(np,\"Desig\",\"Frank'\")],1), ((2,0),[(s\\*np,\"Desig\"),\"wins'\"],2), ((0,1),[(np,\">\",\"Brave' Frank'\")],1), ((1,1),[(s,\"<\",\"wins' Frank'\")],2), ((0,2),[np,\"Np/s->\",\"Brave' (wins' Frank')\"],1), ((0,2),[(s,\"<\",\"wins' (Brave' Frank')\")],2)] is [((1,1),[(s,\"<\",\"wins' Frank'\")],2)]" $ do
      let c01 = getCateFromString "np/.np"
      let c02 = npCate
      let c03 = getCateFromString "s\\*np"
      let c11 = npCate
      let c12 = sCate
      let c21 = npCate
      let c22 = sCate
      let pc01 = createPhraCate 0 0 c01 "Desig" "Brave'" 0
      let pc02 = createPhraCate 1 0 c02 "Desig" "Frank'" 1
      let pc03 = createPhraCate 2 0 c03 "Desig" "wins'" 2
      let pc11 = createPhraCate 0 1 c11 ">" "Brave' Frank'" 1
      let pc12 = createPhraCate 1 1 c12 "<" "wins' Frank'" 2
      let pc21 = createPhraCate 0 2 c21 "Np/s->" "Brave' (wins' Frank')" 1
      let pc22 = createPhraCate 0 2 c22 "<" "wins' (Brave' Frank')" 2
      let pcs = [pc01,pc02,pc03]
      let pcClo = pcs ++ [pc11,pc12,pc21,pc22]
      findTipsOfTree [pc12] pcClo `shouldBe` [pc12]

    it "The result of findTipsOfTree [((0,2),[(s,\"<\",\"wins' (Brave' Frank')\")],2),((0,1),[(np,\">\",\"Brave' Frank'\")],1),((2,0),[(s\\*np,\"Desig\",\"wins'\")],2)] [((0,0),[(np/.np,\"Desig\",\"Brave'\")],0), ((1,0),[(np,\"Desig\",\"Frank'\")],1), ((2,0),[(s\\*np,\"Desig\"),\"wins'\"],2), ((0,1),[(np,\">\",\"Brave' Frank'\")],1), ((1,1),[(s,\"<\",\"wins' Frank'\")],2), ((0,2),[np,\"Np/s->\",\"Brave' (wins' Frank')\"],1), ((0,2),[(s,\"<\",\"wins' (Brave' Frank')\")],2)] is [((0,1),[(np,\">\",\"Brave' Frank'\")],1)]" $ do
      let c01 = getCateFromString "np/.np"
      let c02 = npCate
      let c03 = getCateFromString "s\\*np"
      let c11 = npCate
      let c12 = sCate
      let c21 = npCate
      let c22 = sCate
      let pc01 = createPhraCate 0 0 c01 "Desig" "Brave'" 0
      let pc02 = createPhraCate 1 0 c02 "Desig" "Frank'" 1
      let pc03 = createPhraCate 2 0 c03 "Desig" "wins'" 2
      let pc11 = createPhraCate 0 1 c11 ">" "Brave' Frank'" 1
      let pc12 = createPhraCate 1 1 c12 "<" "wins' Frank'" 2
      let pc21 = createPhraCate 0 2 c21 "Np/s->" "Brave' (wins' Frank')" 1
      let pc22 = createPhraCate 0 2 c22 "<" "wins' (Brave' Frank')" 2
      let pcs = [pc01,pc02,pc03]
      let pcClo = pcs ++ [pc11,pc12,pc21,pc22]
      findTipsOfTree [pc22,pc11,pc03] pcClo `shouldBe` [pc11]

    it "The result of findTipsOfTree [((0,0),[(np/.np,\"Desig\",\"Good'\")],0), ((1,1),[((s\\.np)/.np,\">B\",\"is_better_than' bad'\")],2) ((0,2),[(np,\"Np/v->\",\"Good' (is_better_than' bad')\")],1)] [(s,\"Np/a-<\",\"(is_better_than' bad') Good'\")],1)  [((0,0),[(np/.np,\"Desig\",\"Good'\")],0), ((1,0),[((s\\.np)/.np,\"Desig\",\"is_better_than'\")],1), ((2,0),[(np/.np,\"Desig\",\"bad'\")],2), ((0,1),[(np,\"Np/v->\",\"Good' is_better_than'\")],1), ((0,1),[(s/.np,\"Np/a->T->B\",\"Good' is_better_than'\")],1), ((1,1),[((s\\.np)/.np,\">B\",\"is_better_than' bad'\")],2), ((1,1),[(s\\.np,\"Np/a->\",\"is_better_than' bad'\")],2), ((0,2),[(np,\"Np/v->\",\"Good' (is_better_than' bad')\")],1), ((0,2),[(s,\"Np/a-<\",\"(is_better_than' bad') Good'\")],1), ((0,2),[(s/.np,\"Np/a->T->B\",\"Good' (is_better_than' bad')\")],1), ((0,2),[(s/.np,\">B\",\"(Good' is_better_than') bad'\")],2), ((0,2),[(s,\"Np/a->\",\"(Good' is_better_than') bad'\")],2)] is [((1,1),[((s\\.np)/.np,\">B\",\"is_better_than' bad'\")],2), ((0,2),[(np,\"Np/v->\",\"Good' (is_better_than' bad')\")],1)]" $ do
      let c01 = getCateFromString "np/.np"
      let c02 = getCateFromString "(s\\.np)/.np"
      let c03 = getCateFromString "np/.np"
      let c11 = npCate
      let c12 = getCateFromString "s/.np"
      let c13 = getCateFromString "(s\\.np)/.np"
      let c14 = getCateFromString "s\\.np"
      let c21 = npCate
      let c22 = sCate
      let c23 = getCateFromString "s/.np"
      let c24 = getCateFromString "s/.np"
      let c25 = sCate
      let pc01 = createPhraCate 0 0 c01 "Desig" "Good'" 0
      let pc02 = createPhraCate 1 0 c02 "Desig" "is_better_than'" 1
      let pc03 = createPhraCate 2 0 c03 "Desig" "bad'" 2
      let pc11 = createPhraCate 0 1 c11 "Np/v->" "Good' is_better_than'" 1
      let pc12 = createPhraCate 0 1 c12 "Np/a->T->B" "Good' is_better_than'" 1
      let pc13 = createPhraCate 1 1 c13 ">B" "is_better_than' bad'" 2
      let pc14 = createPhraCate 1 1 c14 "Np/a->" "is_better_than' bad'" 2
      let pc21 = createPhraCate 0 2 c21 "Np/v->" "Good' (is_better_than' bad')" 1
      let pc22 = createPhraCate 0 2 c22 "Np/a-<" "(is_better_than' bad') Good'" 1
      let pc23 = createPhraCate 0 2 c23 "Np/a->T->B" "Good' (is_better_than' bad')" 1
      let pc24 = createPhraCate 0 2 c24 ">B" "(Good' is_better_than') bad'" 2
      let pc25 = createPhraCate 0 2 c25 "Np/a->" "(Good' is_better_than') bad'" 2
      let pcClo = [pc01,pc02,pc03,pc11,pc12,pc13,pc14,pc21,pc22,pc23,pc24,pc25]
      findTipsOfTree [pc01,pc13,pc21] pcClo `shouldBe` [pc13]

    it "The result of findCateBySpan 1 [((0,0),[(np/.np,\"Desig\",\"Brave'\")],0), ((1,0),[(np,\"Desig\",\"Frank'\")],1), ((2,0),[(s\\*np,\"Desig\"),\"wins'\"],2), ((0,1),[(np,\">\",\"Brave' Frank'\")],1), ((1,1),[(s,\"<\",\"wins' Frank'\")],2), ((0,2),[np,\"Np/s->\",\"Brave' (wins' Frank')\"],1), ((0,2),[(s,\"<\",\"wins' (Brave' Frank')\")],2)] is [((0,1),[(np,\">\",\"Brave' Frank'\")],1), ((1,1),[(s,\"<\",\"wins' Frank'\")],2)]" $ do
      let c01 = getCateFromString "np/.np"
      let c02 = npCate
      let c03 = getCateFromString "s\\*np"
      let c11 = npCate
      let c12 = sCate
      let c21 = npCate
      let c22 = sCate
      let pc01 = createPhraCate 0 0 c01 "Desig" "Brave'" 0
      let pc02 = createPhraCate 1 0 c02 "Desig" "Frank'" 1
      let pc03 = createPhraCate 2 0 c03 "Desig" "wins'" 2
      let pc11 = createPhraCate 0 1 c11 ">" "Brave' Frank'" 1
      let pc12 = createPhraCate 1 1 c12 "<" "wins' Frank'" 2
      let pc21 = createPhraCate 0 2 c21 "Np/s->" "Brave' (wins' Frank')" 1
      let pc22 = createPhraCate 0 2 c22 "<" "wins' (Brave' Frank')" 2
      let pcs = [pc01,pc02,pc03]
      let pcClo = pcs ++ [pc11,pc12,pc21,pc22]
      findCateBySpan 1 pcClo `shouldBe` [pc11,pc12]

    it "The result of findCateBySpan 0 [((0,0),[(np/.np,\"Desig\",\"Brave'\")],0), ((1,0),[(np,\"Desig\",\"Frank'\")],1), ((2,0),[(s\\*np,\"Desig\"),\"wins'\"],2), ((0,1),[(np,\">\",\"Brave' Frank'\")],1), ((1,1),[(s,\"<\",\"wins' Frank'\")],2), ((0,2),[np,\"Np/s->\",\"Brave' (wins' Frank')\"],1), ((0,2),[(s,\"<\",\"wins' (Brave' Frank')\")],2)] is [((0,0),[(np/.np, \"Desig\",\"Brave'\")],0), ((1,0),[(np,\"Desig\",\"Frank'\")],1), ((2,0),[(s, \"Desig\",\"wins'\")],2)]" $ do
      let c01 = getCateFromString "np/.np"
      let c02 = npCate
      let c03 = getCateFromString "s\\*np"
      let c11 = npCate
      let c12 = sCate
      let c21 = npCate
      let c22 = sCate
      let pc01 = createPhraCate 0 0 c01 "Desig" "Brave'" 0
      let pc02 = createPhraCate 1 0 c02 "Desig" "Frank'" 1
      let pc03 = createPhraCate 2 0 c03 "Desig" "wins'" 2
      let pc11 = createPhraCate 0 1 c11 ">" "Brave' Frank'" 1
      let pc12 = createPhraCate 1 1 c12 "<" "wins' Frank'" 2
      let pc21 = createPhraCate 0 2 c21 "Np/s->" "Brave' (wins' Frank')" 1
      let pc22 = createPhraCate 0 2 c22 "<" "wins' (Brave' Frank')" 2
      let pcs = [pc01,pc02,pc03]
      let pcClo = pcs ++ [pc11,pc12,pc21,pc22]
      findCateBySpan 0 pcClo `shouldBe` [pc01,pc02,pc03]

    it "The result of findCateBySpan 2 [((0,0),[(np/.np,\"Desig\",\"Brave'\")],0), ((1,0),[(np,\"Desig\",\"Frank'\")],1), ((2,0),[(s\\*np,\"Desig\"),\"wins'\"],2), ((0,1),[(np,\">\",\"Brave' Frank'\")],1), ((1,1),[(s,\"<\",\"wins' Frank'\")],2), ((0,2),[np,\"Np/s->\",\"Brave' (wins' Frank')\"],1), ((0,2),[(s,\"<\",\"wins' (Brave' Frank')\")],2)] is [((0,2),[np,\"Np/s->\",\"Brave' (wins' Frank')\"],1), ((0,2),[(s,\"<\",\"wins' (Brave' Frank')\")],2)]" $ do
      let c01 = getCateFromString "np/.np"
      let c02 = npCate
      let c03 = getCateFromString "s\\*np"
      let c11 = npCate
      let c12 = sCate
      let c21 = npCate
      let c22 = sCate
      let pc01 = createPhraCate 0 0 c01 "Desig" "Brave'" 0
      let pc02 = createPhraCate 1 0 c02 "Desig" "Frank'" 1
      let pc03 = createPhraCate 2 0 c03 "Desig" "wins'" 2
      let pc11 = createPhraCate 0 1 c11 ">" "Brave' Frank'" 1
      let pc12 = createPhraCate 1 1 c12 "<" "wins' Frank'" 2
      let pc21 = createPhraCate 0 2 c21 "Np/s->" "Brave' (wins' Frank')" 1
      let pc22 = createPhraCate 0 2 c22 "<" "wins' (Brave' Frank')" 2
      let pcs = [pc01,pc02,pc03]
      let pcClo = pcs ++ [pc11,pc12,pc21,pc22]
      findCateBySpan 2 pcClo `shouldBe` [pc21,pc22]

    it "The result of divPhraCateBySpan [((0,0),[(np/.np,\"Desig\",\"Brave'\")],0), ((1,0),[(np,\"Desig\",\"Frank'\")],1), ((2,0),[(s\\*np,\"Desig\"),\"wins'\"],2), ((0,1),[(np,\">\",\"Brave' Frank'\")],1), ((0,2),[(s,\"<\",\"wins' (Brave' Frank')\")],2)] is [[((0,0),[(np/.np, \"Desig\",\"Brave'\")],0), ((1,0),[(np,\"Desig\",\"Frank'\")],1), ((2,0),[(s\\*np, \"Desig\",\"wins'\")],2)], [((0,1),[(np, \">\",\"Brave' Frank'\")],1)], [((0,2),[(s,\"<\",\"wins' (Brave' Frank')\")],2)]]" $ do
      let c01 = getCateFromString "np/.np"
      let c02 = npCate
      let c03 = getCateFromString "s\\*np"
      let c11 = npCate
      let c12 = sCate
      let c21 = npCate
      let c22 = sCate
      let pc01 = createPhraCate 0 0 c01 "Desig" "Brave'" 0
      let pc02 = createPhraCate 1 0 c02 "Desig" "Frank'" 1
      let pc03 = createPhraCate 2 0 c03 "Desig" "wins'" 2
      let pc11 = createPhraCate 0 1 c11 ">" "Brave' Frank'" 1
      let pc12 = createPhraCate 1 1 c12 "<" "wins' Frank'" 2
      let pc21 = createPhraCate 0 2 c21 "Np/s->" "Brave' (wins' Frank')" 1
      let pc22 = createPhraCate 0 2 c22 "<" "wins' (Brave' Frank')" 2
      let pcClo = [pc01,pc02,pc03,pc11,pc22]
      divPhraCateBySpan pcClo `shouldBe` [[pc01,pc02,pc03],[pc11],[pc22]]

    it "The result of growTree [((0,2),[(s, \">\",\"(loves' Frank') Mary'\")],2)] [((0,0),[(np, \"Desig\",\"Frank'\")],0), ((1,0),[((s\\.np)/.np, \"Desig\",\"loves'\")],1), ((2,0),[(np, \"Desig\",\"Mary'\")],2), ((0,1),[(s/.np,\">T->B\",\"Frank' loves'\")],1), ((1,1),[(s\\.np, \"<\",\"loves' Frank'\")],2), ((0,2),[(s, \"<\",\"(loves' Mary') Frank'\")],1), ((0,2),[(s, \">\", \"(Frank' loves') Mary'\")],2) is [((0,0),[(np, \"Desig\",\"Frank'\")],0), ((1,0),[((s\\.np)/.np, \"Desig\",\"loves'\")],1), ((0,1),[(s/.np, \">T->B\",\"Frank' loves'\")],1), ((2,0),[(np, \"Desig\",\"Mary'\")],2), ((0,2),[(s, \">\",\"(Frank' loves') Mary'\")],2)]" $ do
      let c01 = npCate
      let c02 = getCateFromString "(s\\.np)/.np"
      let c03 = npCate
      let c11 = getCateFromString "s/.np"
      let c12 = getCateFromString "s\\.np"
      let c21 = sCate
      let c22 = sCate
      let pc01 = createPhraCate 0 0 c01 "Desig" "Frank'" 0
      let pc02 = createPhraCate 1 0 c02 "Desig" "loves'" 1
      let pc03 = createPhraCate 2 0 c03 "Desig" "Mary'" 2
      let pc11 = createPhraCate 0 1 c11 ">T->B" "Frank' loves'" 1 
      let pc12 = createPhraCate 1 1 c12 ">" "loves' Mary'" 2
      let pc21 = createPhraCate 0 2 c21 "<" "(loves' Mary') Frank'" 1
      let pc22 = createPhraCate 0 2 c22 ">" "(Frank' loves') Mary'" 2 
      let pcClo = [pc01,pc02,pc03,pc11,pc12,pc21,pc22]
      growTree [pc22] pcClo `shouldBe` [[pc01,pc02,pc11,pc03,pc22]]

    it "The result of growTree [((0,2),[(s, \"<\", \"(loves' Mary') Frank'\")],1)]  [((0,0),[(np, \"Desig\",\"Frank'\")],0), ((1,0),[((s\\.np)/.np, \"Desig\",\"loves'\")],1), ((2,0),[(np, \"Desig\",\"Mary'\")],2), ((0,1),[(s/.np,\">T->B\",\"Frank' loves'\")],1), ((1,1),[(s\\.np, \"<\",\"loves' Frank'\")],2), ((0,2),[(s, \"<\",\"(loves' Mary') Frank'\")],1), ((0,2),[(s, \">\", \"(Frank' loves') Mary'\")],2) is [((1,0),[(s\\.np/.np, \"Desig\",\"loves'\")],1), ((2,0),[(np, \"Desig\",\"Mary'\")],2), ((0,0),[(np, \"Desig\",\"Frank'\")],0), ((1,1),[(s\\.np, \">\",\"loves' Mary'\")],2), ((0,2),[(s, \"<\", \"(loves' Mary') Frank'\")],1)]" $ do
      let c01 = npCate
      let c02 = getCateFromString "(s\\.np)/.np"
      let c03 = npCate
      let c11 = getCateFromString "s/.np"
      let c12 = getCateFromString "s\\.np"
      let c21 = sCate
      let c22 = sCate
      let pc01 = createPhraCate 0 0 c01 "Desig" "Frank'" 0
      let pc02 = createPhraCate 1 0 c02 "Desig" "loves'" 1
      let pc03 = createPhraCate 2 0 c03 "Desig" "Mary'" 2
      let pc11 = createPhraCate 0 1 c11 ">T->B" "Frank' loves'" 1
      let pc12 = createPhraCate 1 1 c12 ">" "loves' Mary'" 2
      let pc21 = createPhraCate 0 2 c21 "<" "(loves' Mary') Frank'" 1
      let pc22 = createPhraCate 0 2 c22 ">" "(Frank' loves') Mary'" 2
      let pcClo = [pc01,pc02,pc03,pc11,pc12,pc21,pc22]
      growTree [pc21] pcClo `shouldBe` [[pc02,pc03,pc01,pc12,pc21]]

    it "The result of growTree ((0,2),[(np,\"Np/v->\",\"Good' (is_better_than' bad')\")],1)  [((0,0),[(np/.np,\"Desig\",\"Good'\")],0), ((1,0),[((s\\.np)/.np,\"Desig\",\"is_better_than'\")],1), ((2,0),[(np/.np,\"Desig\",\"bad'\")],2), ((0,1),[(np,\"Np/v->\",\"Good' is_better_than'\")],1), ((0,1),[(s/.np,\"Np/a->T->B\",\"Good' is_better_than'\")],1), ((1,1),[((s\\.np)/.np,\">B\",\"is_better_than' bad'\")],2), ((1,1),[(s\\.np,\"Np/a->\",\"is_better_than' bad'\")],2), ((0,2),[(np,\"Np/v->\",\"Good' (is_better_than' bad')\")],1), ((0,2),[(s,\"Np/a-<\",\"(is_better_than' bad') Good'\")],1), ((0,2),[(s/.np,\"Np/a->T->B\",\"Good' (is_better_than' bad')\")],1), ((0,2),[(s/.np,\">B\",\"(Good' is_better_than') bad'\")],2), ((0,2),[(s,\"Np/a->\",\"(Good' is_better_than') bad'\")],2)] is [(((1,0),[(s\\np/.np,\"Desig\",\"is_better_than'\")],1), ((2,0),[(np/.np,\"Desig\",\"bad'\")],1))]" $ do
      let c01 = getCateFromString "np/.np"
      let c02 = getCateFromString "(s\\.np)/.np"
      let c03 = getCateFromString "np/.np"
      let c11 = npCate
      let c12 = getCateFromString "s/.np"
      let c13 = getCateFromString "(s\\.np)/.np"
      let c14 = getCateFromString "s\\.np"
      let c21 = npCate
      let c22 = sCate
      let c23 = getCateFromString "s/.np"
      let c24 = getCateFromString "s/.np"
      let c25 = sCate
      let pc01 = createPhraCate 0 0 c01 "Desig" "Good'" 0
      let pc02 = createPhraCate 1 0 c02 "Desig" "is_better_than'" 1
      let pc03 = createPhraCate 2 0 c03 "Desig" "bad'" 2
      let pc11 = createPhraCate 0 1 c11 "Np/v->" "Good' is_better_than'" 1
      let pc12 = createPhraCate 0 1 c12 "Np/a->T->B" "Good' is_better_than'" 1
      let pc13 = createPhraCate 1 1 c13 ">B" "is_better_than' bad'" 2
      let pc14 = createPhraCate 1 1 c14 "Np/a->" "is_better_than' bad'" 2
      let pc21 = createPhraCate 0 2 c21 "Np/v->" "Good' (is_better_than' bad')" 1
      let pc22 = createPhraCate 0 2 c22 "Np/a-<" "(is_better_than' bad') Good'" 1
      let pc23 = createPhraCate 0 2 c23 "Np/a->T->B" "Good' (is_better_than' bad')" 1
      let pc24 = createPhraCate 0 2 c24 ">B" "(Good' is_better_than') bad'" 2
      let pc25 = createPhraCate 0 2 c25 "Np/a->" "(Good' is_better_than') bad'" 2
      let pcClo = [pc01,pc02,pc03,pc11,pc12,pc13,pc14,pc21,pc22,pc23,pc24,pc25]
      growTree [pc21] pcClo `shouldBe` [[pc02,pc03,pc01,pc13,pc21],[pc02,pc03,pc01,pc14,pc21]]

    it "The result of growForest [[((0,2),[(s, \"<\", \"(loves' Mary') Frank'\")],1)], [((0,2),[(s, \">\",\"(loves' Frank') Mary'\")],2)]]  [((0,0),[(np, \"Desig\",\"Frank'\")],0), ((1,0),[((s\\.np)/.np, \"Desig\",\"loves'\")],1), ((2,0),[(np, \"Desig\",\"Mary'\")],2), ((0,1),[(s/.np,\">T->B\",\"Frank' loves'\")],1), ((1,1),[(s\\.np, \"<\",\"loves' Frank'\")],2), ((0,2),[(s, \"<\",\"(loves' Mary') Frank'\")],1), ((0,2),[(s, \">\", \"(Frank' loves') Mary'\")],2) is [[((1,0),[(s\\.np/.np, \"Desig\",\"loves'\")],1), ((2,0),[(np, \"Desig\",\"Mary'\")],2), ((0,0),[(np, \"Desig\",\"Frank'\")],0), ((1,1),[(s\\.np, \">\",\"loves' Mary'\")],2), ((0,2),[(s, \"<\", \"(loves' Mary') Frank'\")],1)], [((0,0),[(np, \"Desig\",\"Frank'\")],0), ((1,0),[((s\\.np)/.np, \"Desig\",\"loves'\")],1), ((0,1),[(s/.np, \">T->B\",\"Frank' loves'\")],1), ((2,0),[(np, \"Desig\",\"Mary'\")],2), ((0,2),[(s, \">\",\"(Frank' loves') Mary'\")],2)]]" $ do
      let c01 = npCate
      let c02 = getCateFromString "(s\\.np)/.np"
      let c03 = npCate
      let c11 = getCateFromString "s/.np"
      let c12 = getCateFromString "s\\.np"
      let c21 = sCate
      let c22 = sCate
      let pc01 = createPhraCate 0 0 c01 "Desig" "Frank'" 0
      let pc02 = createPhraCate 1 0 c02 "Desig" "loves'" 1
      let pc03 = createPhraCate 2 0 c03 "Desig" "Mary'" 2
      let pc11 = createPhraCate 0 1 c11 ">T->B" "Frank' loves'" 1
      let pc12 = createPhraCate 1 1 c12 ">" "loves' Mary'" 2
      let pc21 = createPhraCate 0 2 c21 "<" "(loves' Mary') Frank'" 1
      let pc22 = createPhraCate 0 2 c22 ">" "(Frank' loves') Mary'" 2
      let pcClo = [pc01,pc02,pc03,pc11,pc12,pc21,pc22]
      growForest [[pc21],[pc22]] pcClo `shouldBe` [[pc02,pc03,pc01,pc12,pc21],[pc01,pc02,pc11,pc03,pc22]]

    it "The result of growForest [[((0,2),[(np,\"Np/s->\",\"Brave' (wins' Frank')\"],1)], [((0,2),[(s, \"<\", \"wins' (Brave' Frank')\")],2)] [((0,0),[(np/.np,\"Desig\",\"Brave'\")],0), ((1,0),[(np,\"Desig\",\"Frank'\")],1), ((2,0),[(s\\*np,\"Desig\"),\"wins'\"],2), ((0,1),[(np,\">\",\"Brave' Frank'\")],1), ((1,1),[(s,\"<\",\"wins' Frank'\")],2), ((0,2),[np,\"Np/s->\",\"Brave' (wins' Frank')\"],1), ((0,2),[(s,\"<\",\"wins' (Brave' Frank')\")],2)] is [[((1,0),[(np, \"Desig\",\"Frank'\")],1), ((2,0),[(s\\*np, \"Desig\",\"wins'\")],2), ((0,0),[(np/.np, \"Desig\",\"Brave'\")],0), ((1,1),[(s, \"<\",\"wins' Frank'\")],2), ((0,2),[(np,\"Np/s->\",\"Brave' (wins' Frank')\"],1)], [((0,0),[(np/.np, \"Desig\",\"Brave'\")],0), ((1,0),[(np, \"Desig\",\"Frank'\")],1), ((0,1),[(np, \">\",\"Brave' Frank'\")],1), ((2,0),[(s\\*np, \"Desig\",\"wins'\")],2), ((0,2),[(s, \"<\",\"wins' (Brave' Frank')\")],2)" $ do
      let c01 = getCateFromString "np/.np"
      let c02 = npCate
      let c03 = getCateFromString "s\\*np"
      let c11 = npCate
      let c12 = sCate
      let c21 = npCate
      let c22 = sCate
      let pc01 = createPhraCate 0 0 c01 "Desig" "Brave'" 0
      let pc02 = createPhraCate 1 0 c02 "Desig" "Frank'" 1
      let pc03 = createPhraCate 2 0 c03 "Desig" "wins'" 2
      let pc11 = createPhraCate 0 1 c11 ">" "Brave' Frank'" 1
      let pc12 = createPhraCate 1 1 c12 "<" "wins' Frank'" 2
      let pc21 = createPhraCate 0 2 c21 "Np/s->" "Brave' (wins' Frank')" 1
      let pc22 = createPhraCate 0 2 c22 "<" "wins' (Brave' Frank')" 2
      let pcClo = [pc01,pc02,pc03,pc11,pc12,pc21,pc22]
      growForest [[pc21],[pc22]] pcClo `shouldBe` [[pc02,pc03,pc01,pc12,pc21],[pc01,pc02,pc11,pc03,pc22]]


