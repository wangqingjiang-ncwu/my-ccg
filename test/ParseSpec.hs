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
      initPhraCate [(sCate, "Frank'")] `shouldBe` ([((0,0),[(sCate,"Desig","Frank'",True)],0)] :: [PhraCate])

    it "The result of initPhraCate [np, (s\\.np)/.np, np] is [((0,0),[(np,\"Desig\",\"Frank'\")],0),((1,0),[((s\\.np)/.np,\"Desig\",\"loves'\")],1),((2,0),[(np,\"Desig\",\"Mary'\")],2)]" $ do
      let c1 = npCate
      let c2 = getCateFromString "(s\\.np)/.np"
      let c3 = npCate
      initPhraCate [(c1,"Frank'"),(c2,"loves'"),(c3,"Mary'")] `shouldBe` ([((0,0),[(c1,"Desig","Frank'",True)],0),((1,0),[(c2,"Desig","loves'",True)],1),((2,0),[(c3,"Desig","Mary'",True)],2)] :: [PhraCate])

    it "The result of createPhraCate 0 1 s \"Np/s\" \"smiles' Frank'\" 1 is ((0,1),[(s, \"Np/s\", \"smiles' Frank'\")],1)" $ do
      let c = sCate
      createPhraCate 0 1 c "Np/s" "smiles' Frank'" True 1 `shouldBe` (((0,1),[(c,"Np/s","smiles' Frank'",True)],1)::PhraCate)

    it "The result of applying func pclt to ((0,0),[(np,\"Desig\",\"Frank'\")],0) and ((1,0),[((s\\.np)/.np,\"Desig\",\"loves'\")],1) is True" $ do
      let c1 = npCate
      let c2 = getCateFromString "(s\\.np)/.np"
      let pcs = initPhraCate [(c1,"Frank'"), (c2,"smile'")]
      let pc1 = head pcs
      let pc2 = last pcs
      pclt pc1 pc2 `shouldBe` (True :: Bool)

    it "The result of applying func pclt to ((0,0),[(np,\"Desig\",\"Frank'\",True)],0) and ((0,0),[(s/.np,\"Desig\",\"loves' Mary'\",True)],0) is False" $ do
      let c1 = npCate
      let c2 = getCateFromString "s/.np"
      let pcs1 = initPhraCate [(c1,"Frank'")]
      let pc1 = head pcs1
      let pcs2 = initPhraCate [(c2,"loves' Mary'")]
      let pc2 = head pcs2
      pclt pc1 pc2 `shouldBe` (False :: Bool)
    
    it "The result of ctsaOfCate ((1,0),[((s\\.np)/.np, \"Desig\", \"likes'\",True),(s/.np, \"Desig\", \"likes'\",True)],1) is [\"(s\\.np)/.np\", \"Desig\", \"likes'\", True),(\"s/.np\", \"Desig\", \"likes'\", True)]" $ do
      let pc = createPhraCate2 1 0 [(getCateFromString "(s\\.np)/.np", "Desig", "likes'", True),(getCateFromString "s/.np", "Desig", "likes'", True)] 1
      let ctsa = [(getCateFromString "(s\\.np)/.np", "Desig", "likes'", True),(getCateFromString "s/.np", "Desig", "likes'", True)]
      ctsaOfCate pc `shouldBe` ctsa

    it "The result of ctsOfCate ((1,0),[((s\\.np)/.np, \"Desig\", \"likes'\",True),(s/.np, \"Desig\", \"likes'\",True)],1) is [(\"(s\\.np)/.np\", \"Desig\", \"likes'\"),(\"s/.np\", \"Desig\", \"likes'\")]" $ do
      let pc = createPhraCate2 1 0 [(getCateFromString "(s\\.np)/.np", "Desig", "likes'", True),(getCateFromString "s/.np", "Desig", "likes'", True)] 1
      let cts = [(getCateFromString "(s\\.np)/.np", "Desig", "likes'"),(getCateFromString "s/.np", "Desig", "likes'")]
      ctsOfCate pc `shouldBe` cts  

    it "The result of csOfActCate ((1,0),[((s\\.np)/.np, \"Desig\", \"likes'\",True),(s/.np, \"Desig\", \"likes'\",False)],1) is [(\"(s\\.np)/.np\", \"likes'\")]" $ do
      let pc = createPhraCate2 1 0 [(getCateFromString "(s\\.np)/.np", "Desig", "likes'", True),(getCateFromString "s/.np", "Desig", "likes'", False)] 1
      let cs = [(getCateFromString "(s\\.np)/.np", "likes'")]
      csOfActCate pc `shouldBe` cs  
    
    it "The result of caOfActCate ((1,0),[((s\\.np)/.np, \"Desig\", \"likes'\",True),(s/.np, \"Desig\", \"likes'\",False)],1) is [\"(s\\.np)/.np\"]" $ do
      let pc = createPhraCate2 1 0 [(getCateFromString "(s\\.np)/.np", "Desig", "likes'", True),(getCateFromString "s/.np", "Desig", "likes'", False)] 1
      let ca = [getCateFromString "(s\\.np)/.np"]
      caOfActCate pc `shouldBe` ca 
    
    it "The result of csOfActCate ((1,0),[((s\\.np)/.np, \"Desig\", \"likes'\",False),(s/.np, \"Desig\", \"likes'\",True)],1) is [(\"s/.np\", \"likes'\")]" $ do
      let pc = createPhraCate2 1 0 [(getCateFromString "(s\\.np)/.np", "Desig", "likes'", False),(getCateFromString "s/.np", "Desig", "likes'", True)] 1
      let cs = [(getCateFromString "s/.np", "likes'")]
      csOfActCate pc `shouldBe` cs  

    it "The result of pcBelong ((1,0),[(s\\.np)/.np, \"Desig\", \"likes'\",True)],1) ((1,0),[(s\\.np)/.np, \"Desig\", \"likes'\",True)],1) is True" $ do
      let pc1 = createPhraCate 1 0 (getCateFromString "(s\\.np)/.np") "Desig" "likes'" True 1
      let pc2 = createPhraCate 1 0 (getCateFromString "(s\\.np)/.np") "Desig" "likes'" True 1
      pcBelong pc1 pc2 `shouldBe` True

    it "The result of pcBelong ((1,0),[(s\\.np)/.np, \"Desig\", \"likes'\",True)],1) ((1,0),[(s\\.np)/.np, \"Desig\", \"likes'\",True),(np, \">\",\"book'\",True)],1) is True" $ do
      let cate = getCateFromString "(s\\.np)/.np"
      let pc1 = createPhraCate 1 0 cate "Desig" "likes'" True 1
      let pc2 = createPhraCate2 1 0 [(cate,"Desig","likes'",True),(npCate,">","book'",True)] 1
      pcBelong pc1 pc2 `shouldBe` True

    it "The result of pcBelong ((1,0),[(s\\.np)/.np, \"Desig\", \"likes'\",True),(np, \">\",\"book'\",True)],1) ((1,0),[(s\\.np)/.np, \"Desig\", \"likes'\",True)],1) is False" $ do
      let cate = getCateFromString "(s\\.np)/.np"
      let pc1 = createPhraCate 1 0 cate "Desig" "likes'" True 1
      let pc2 = createPhraCate2 1 0 [(cate,"Desig","likes'",True),(npCate,">","book'",True)] 1
      pcBelong pc2 pc1 `shouldBe` False

    it "The result of pcBelong' ((1,0),[(s\\.np)/.np, \"Desig\", \"likes'\",False)],1) ((1,0),[(s\\.np)/.np, \"Desig\", \"likes'\",True),(np, \">\",\"book'\",True)],1) is False" $ do
      let cate = getCateFromString "(s\\.np)/.np"
      let pc1 = createPhraCate 1 0 cate "Desig" "likes'" False 1
      let pc2 = createPhraCate2 1 0 [(cate,"Desig","likes'",True),(npCate,">","book'",True)] 1
      pcBelong' pc1 pc2 `shouldBe` True

    it "The result of stOfCate ((1,0),[(s\\.np, \"Desig\", \"smiles'\",True)],1) is 1" $ do
      let c = getCateFromString "s\\.np"
      let pc = createPhraCate 1 0 c "Desig" "smiles'" True 1
      stOfCate pc `shouldBe` 1

    it "The result of spOfCate ((1,0),[(s\\.np, \"Desig\", \"smiles'\",True)],1) is 0" $ do
      let c = getCateFromString "s\\.np"
      let pc = createPhraCate 1 0 c "Desig" "smiles'" True 1
      spOfCate pc `shouldBe` 0

    it "The result of ssOfCate ((1,0),[(s\\.np, \"Desig\", \"smiles'\",True)],1) is 1" $ do
      let c = getCateFromString "s\\.np"
      let pc = createPhraCate 1 0 c "Desig" "smiles'" True 1
      ssOfCate pc `shouldBe` 1

    it "The result of ctsaOfCate ((1,0),[(s\\.np, \"Desig\", \"smiles'\",True)],1) is [(s\\.np, \"Desig\", \"smiles'\",True)]" $ do
      let c = getCateFromString "s\\.np"
      let pc = createPhraCate 1 0 c "Desig" "smiles'" True 1
      ctsaOfCate pc `shouldBe` [(c,"Desig","smiles'",True)]

    it "The result of caOfCate ((1,0),[(s\\.np, \"Desig\", \"smiles'\",True)],1) is [s\\.np]" $ do
      let c = getCateFromString "s\\.np"
      let pc = createPhraCate 1 0 c "Desig" "smiles'" True 1
      caOfCate pc `shouldBe` [c]
   
    it "The result of cateComb_CS \"+++++\" ((0,0),[(np, \"Desig\", \"Frank'\",True)],0) ((1,0),[(s\\.np, \"Desig\", \"smiles\",True)],1) is ((0,1),[(s, \"<\", \"smiles' Frank'\",True)],1)" $ do
      let c1 = npCate
      let c2 = getCateFromString "s\\.np"
      let c3 = sCate
      let pc1 = createPhraCate 0 0 c1 "Desig" "Frank'" True 0
      let pc2 = createPhraCate 1 0 c2 "Desig" "smiles'" True 1
      let pc3 = createPhraCate2 0 1 [(c3,"<","smiles' Frank'",True)] 1
      cateComb_CS "+++++" pc1 pc2 `shouldBe` pc3

    it "The result of cateComb_CS \"+++++\" ((0,0),[(np, \"Desig\", \"Frank'\",False), ((1,0),[(s\\.np,\"Desig\",\"smiles'\",True)],1) is ((0,1),[],1)" $ do
      let c1 = npCate
      let c2 = getCateFromString "s\\.np"
      let c3 = sCate
      let pc1 = createPhraCate 0 0 c1 "Desig" "Frank'" False 0
      let pc2 = createPhraCate 1 0 c2 "Desig" "smiles'" True 1
      let pc3 = createPhraCate 0 1 c3 "<" "smiles' Frank'" True 1
      cateComb_CS "+++++" pc1 pc2 `shouldBe` pc3

    it "The result of cateComb_CS ((0,0),[(np,\"Desig\",\"Frank'\",True)],0) ((1,0),[((s\\.np)/.np,\"Desig\",\"loves'\",True)],1) is ((0,1),[(s/.np,\">T->B\"),\"Frank' loves\",True)],1)" $ do
      let c1 = npCate
      let c2 = getCateFromString "(s\\.np)/.np"
      let c3 = getCateFromString "s/.np"
      let pc1 = createPhraCate 0 0 c1 "Desig" "Frank'" True 0
      let pc2 = createPhraCate 1 0 c2 "Desig" "loves'" True 1
      let pc3 = createPhraCate 0 1 c3 ">T->B" "Frank' loves'" True 1
      cateComb_CS "+++++" pc1 pc2 `shouldBe` pc3

    it "The result of cateComb_CS ((0,0),[(s,\"Desig\",\"Frank_cries'\",True)],0) ((1,0),[(s\\.np,\"Desig\",\"is_bad'\",True)],1) is ((0,1),[(s,\"Np/s-<\",\"is_bad' Frank_cries'\",True)],1)" $ do
      let c1 = sCate
      let c2 = getCateFromString "s\\.np"
      let c3 = sCate
      let pc1 = createPhraCate 0 0 c1 "Desig" "Frank_cries'" True 0
      let pc2 = createPhraCate 1 0 c2 "Desig" "is_bad'" True 1
      let pc3 = createPhraCate 0 1 c3 "Np/s-<" "is_bad' Frank_cries'" True 1
      cateComb_CS "+++++" pc1 pc2 `shouldBe` pc3
    
    it "The result of cateComb_CS ((0,0),[((s\\.np)/.np,\"Desig\",\"loves'\",True)],0) ((1,0),[(s,\"Desig\",\"Frank_does_it'\",True)],1) is ((0,1),[(s\\.np,\"Np/s->\",\"loves' Frank_does_it'\",True)],1)" $ do
      let c1 = getCateFromString "(s\\.np)/.np"
      let c2 = sCate
      let c3 = getCateFromString "s\\.np"
      let pc1 = createPhraCate 0 0 c1 "Desig" "loves'" True 0
      let pc2 = createPhraCate 1 0 c2 "Desig" "Frank_does_it'" True 1
      let pc3 = createPhraCate 0 1 c3 "Np/s->" "loves' Frank_does_it'" True 1
      cateComb_CS "+++++" pc1 pc2 `shouldBe` pc3
    
    it "The result of atomizedPhraCate [cateComb_CS ((0,0),[((s\\.np)/.np,\"Desig\",\"loving'\",True)],0) ((1,0),[(s\\.np,\"Desig\",\"is_forever'\",True)],1)] is [((0,1),[(s,\"Np/v-<\",\"is_forever' loving'\",True),(s\\.np,\"Np/v->\",\"loving' is_forever'\",True)],1)]" $ do
      let c1 = getCateFromString "(s\\.np)/.np"
      let c2 = getCateFromString "s\\.np"
      let c3 = sCate
      let c4 = getCateFromString "s\\.np"
      let pc1 = createPhraCate 0 0 c1 "Desig" "loving'" True 0
      let pc2 = createPhraCate 1 0 c2 "Desig" "is_forever'" True 1
      let pc3 = createPhraCate2 0 1 [(c3,"Np/v-<","is_forever' loving'",True)] 1
      let pc4 = createPhraCate2 0 1 [(c4,"Np/v->","loving' is_forever'",True)] 1
      atomizePhraCate [cateComb_CS "+++++" pc1 pc2] `shouldBe` [pc3,pc4]

    it "The result of cateComb_CS ((0,0),[(np/.np,\"Desig\",\"Good'\",True)],0) ((1,1),[((s\\.np)/.np,\">B\",\"is_better_than' bad'\",True), (s\\.np,\"Np/a->\",\"is_better_than' bad'\",True)],2) is ((0,2),[(s/.np,\"<Bx\",\"(is_better_than' bad') Good'\",True),(s,\"Np/a-<\",\"(is_better_than' bad') Good'\",True),(s/.np,\"Np/a->T->B\",\"Good' (is_better_than' bad')\",True)],1)" $ do
      let c1 = getCateFromString "np/.np"
      let c21 = getCateFromString "(s\\.np)/.np"
      let c22 = getCateFromString "s\\.np"
      let c31 = getCateFromString "s/.np"
      let c34 = c31
      let pc1 = createPhraCate 0 0 c1 "Desig" "Good'" True 0
      let pc2 = createPhraCate2 1 1 [(c21,">B","is_better_than' bad'",True),(c22,"Np/a->","is_better_than' bad'",True)] 2
      let pc3 = createPhraCate2 0 2 [(c31,"<Bx","(is_better_than' bad') Good'",True),(sCate,"Np/a-<","(is_better_than' bad') Good'",True),(c34,"Np/a->T->B","Good' (is_better_than' bad')",True)] 1
      cateComb_CS "+++++" pc1 pc2 `shouldBe` pc3
    
    it "The result of [cateComb_CS \"+++++\" pc1 pc2 | pc1 <- trans, pc2 <- trans, pc1 /= pc2, (acOfCate pc1)!!0, (acOfCate pc2)!!0, pclt pc1 pc2] is [((0,1),[(s/.np,\">T->B\",\"loves' Mary'\",True)],1)，((-1,-1),[],-1), ((1,1),[(s\\.np,\">\",\"loves' Mary'\",True)],2)]" $ do
      let c01 = getCateFromString "np"
      let c02 = getCateFromString "(s\\.np)/.np"
      let c03 = getCateFromString "np"
      let c12 = getCateFromString "s\\.np"
      let c11 = getCateFromString "s/.np"
      let pc01 = createPhraCate 0 0 c01 "Desig" "Frank'" True 0
      let pc02 = createPhraCate 1 0 c02 "Desig" "loves'" True 1
      let pc03 = createPhraCate 2 0 c03 "Desig" "Mary'" True 2
      let trans = [pc01, pc02, pc03]
      let pc12 = createPhraCate 1 1 c12 ">" "loves' Mary'" True 2
      let pc11 = createPhraCate 0 1 c11 ">T->B" "Frank' loves'" True 1 
      let null = ((-1,-1),[],-1)
      [cateComb_CS "+++++" pc1 pc2 | pc1 <- trans, pc2 <- trans, pc1 /= pc2, (acOfCate pc1)!!0, (acOfCate pc2)!!0, pclt pc1 pc2] `shouldBe` [pc11,null,pc12]

    it "The result of getNuOfInputCates ((0,0),[(np,\"Desig\",\"Frank'\")],0) ((1,0),[((s\\.np)/.np,\"Desig\",\"loves'\")],1) ((0,1),[(s/.np,\"<\",\"loves' Frank'\")],1) is 2" $ do
      let c1 = npCate
      let c2 = getCateFromString "(s\\.np)/.np"
      let c3 = getCateFromString "s/.np"
      let pc1 = createPhraCate 0 0 c1 "Desig" "Frank'" True 0
      let pc2 = createPhraCate 1 0 c2 "Desig" "loves'" True 1
      let pc3 = createPhraCate 0 1 c3 "<" "loves' Frank'" True 1
      let pcs = [pc1,pc2,pc3]
      getNuOfInputCates pcs `shouldBe` 2

    it "The result of isOverlap ((0,1),[(np,\"Desig\",\"Frank'\",True)],0), ((1,1),[((s\\.np)/.np,\"Desig\",\"loves'\",True)],2) is True" $ do
      let c1 = npCate
      let c2 = getCateFromString "(s\\.np)/.np"
      let pc1 = createPhraCate 0 1 c1 "Desig" "Frank'" True 1
      let pc2 = createPhraCate 1 1 c2 "Desig" "loves'" True 2
      isOverlap pc1 pc2 `shouldBe` True

    it "The result of getOverlap [((0,1),[(np,\"Desig\",\"Frank'\",True)],1), ((1,1),[((s\\.np)/.np,\"Desig\",\"loves'\",True)],2)] is [(((0,1),[(np,\"Desig\",\"Frank'\",True)],1), ((1,1),[(s\\.np)/.np,\"Desig\",\"loves'\",True)],2))]" $ do
      let c1 = npCate
      let c2 = getCateFromString "(s\\.np)/.np"
      let pc1 = createPhraCate 0 1 c1 "Desig" "Frank'" True 1
      let pc2 = createPhraCate 1 1 c2 "Desig" "loves'" True 2
      let pcs = [pc1,pc2]
      let res = [(pc1,pc2)]
      getOverlap pcs `shouldBe` res

    it "The result of deactOnePC ((0,0),[(np,\"Desig\",\"Frank'\",True)],0) is ((0,0),[(np,\"Desig\",\"Frank'\",False)],0)" $ do
      let c1 = npCate
      let pc1 = createPhraCate 0 0 c1 "Desig" "Frank'" True 0
      let pc2 = createPhraCate 0 0 c1 "Desig" "Frank'" False 0
      deactOnePC pc1 `shouldBe` pc2

    it "The result of actOnePC ((0,0),[(np,\"Desig\",\"Frank'\",False)],0) is ((0,0),[(np,\"Desig\",\"Frank'\",True)],0)" $ do
      let c1 = npCate
      let pc1 = createPhraCate 0 0 c1 "Desig" "Frank'" False 0
      let pc2 = createPhraCate 0 0 c1 "Desig" "Frank'" True 0
      actOnePC pc1 `shouldBe` pc2

    it "The result of findSplitCate ((0,1),[(s\\.np,\">T->B\",\"Frank' loves'\",True)],1) [((0,0),[(np,\"Desig\",\"Frank'\")],0), ((1,0),[((s\\.np)/.np,\"Desig\",\"loves'\")],1), ((2,0),[(np,\"Desig\",\"Mary', True)],2), ((0,1),[(s/.np,\">T->B\",\"Frank' loves'\")],1), ((1,1),[(s\\.np,\">\",\"loves' Mary'\")],2) is ((0,0),[(np,\"Desig\",\"Frank'\",True],1), ((1,0),[((s\\.np)/.np,\"Desig\",\"loves'\")],1)]" $ do
      let c01 = getCateFromString "np"
      let c02 = getCateFromString "(s\\.np)/.np"
      let c03 = getCateFromString "np"
      let c11 = getCateFromString "s/.np"
      let c12 = getCateFromString "s\\.np"
      let pc01 = createPhraCate 0 0 c01 "Desig" "Frank'" True 0
      let pc02 = createPhraCate 1 0 c02 "Desig" "loves'" True 1
      let pc03 = createPhraCate 2 0 c03 "Desig" "Mary'" True 2
      let pc11 = createPhraCate 0 1 c11 ">T->B" "Frank' loves'" True 1
      let pc12 = createPhraCate 1 1 c12 "<" "loves' Mary'" True 2
      let pcs = [pc01,pc02,pc03]
      let pcClo = pcs ++ [pc11,pc12]
      findSplitCate "+++++" pc11 pcClo `shouldBe` [(pc01,pc02)]

    it "The result of changeAct \"+++++\" [((0,0),[(np,\"Desig\",\"Frank'\",True)],0), ((1,0),[((s\\.np)/.np,\"Desig\",\"loves'\",True)],1)] is [((0,0),[(np,\"Desig\",\"Frank'\",True)],0), ((1,0),[((s\\.np)/.np,\"Desig\",\"loves'\",True)],1)]" $ do
      let c01 = npCate
      let c02 = getCateFromString "(s\\.np)/.np"
      let pc01 = createPhraCate 0 0 c01 "Desig" "Frank'" True 0
      let pc02 = createPhraCate 1 0 c02 "Desig" "loves'" True 1
      let pcs = [pc01,pc02]
      changeAct "+++++" pcs `shouldBe` pcs

    it "The result of changeAct \"+++++\" [((0,0),[(np,\"Desig\",\"Frank'\",True)],0), ((1,0),[((s\\.np)/.np,\"Desig\",\"loves'\",True)],1), ((2,0),[(np,\"Desig\",\"Mary'\",True)],2), ((1,1),[(s\\.np,\">\",\"loves' Mary'\",True)],2)] is [((0,0),[(np,\"Desig\",\"Frank'\",True)],0), ((1,0),[((s\\.np)/.np,\"Desig\",\"loves'\",False)],1), ((2,0),[(np,\"Desig\",\"Mary'\",False)],2), ((1,1),[(s\\.np,\">\",\"loves' Mary'\",True)],2)]" $ do
      let c01 = npCate
      let c02 = getCateFromString "(s\\.np)/.np"
      let c03 = npCate
      let c11 = getCateFromString "s/.np"
      let c12 = getCateFromString "s\\.np"
      let pc01 = createPhraCate 0 0 c01 "Desig" "Frank'" True 0
      let pc02 = createPhraCate 1 0 c02 "Desig" "loves'" True 1
      let pc03 = createPhraCate 2 0 c03 "Desig" "Mary'" True 2
      let pc11 = createPhraCate 0 1 c11 ">T->B" "Frank' loves'" True 1
      let pc12 = createPhraCate 1 1 c12 ">" "loves' Mary'" True 2
      let pc010 = deactOnePC pc01
      let pc020 = deactOnePC pc02
      let pc030 = deactOnePC pc03
      let pcs = [pc01,pc02,pc03]
      let pcs0 = [pc010,pc020,pc030]
      let pcClo = pcs ++ [pc11,pc12]
      let pcClo0 = pcs0 ++ [pc11,pc12]
      changeAct "+++++" pcClo `shouldBe` pcClo0

    it "The result of getOverlap [((0,0),[(np,\"Desig\",\"Frank'\",True)],0), ((1,0),[(s\\.np)/.np,\"Desig\",\"loves'\",True)],1), ((0,1),[(s/.np,\">T->B\",\"Frank' loves'\",True)],1), ((1,1),[(s\\.np,\"<\",\"loves' Mary'\",True)],2)] is [(((0,1),[(s/.np,\">T->B\",\"Frank' loves'\",True)],1), ((1,1),[(s\\.np,\">\",\"loves' Mary'\",True)],2)]" $ do
      let c1 = npCate
      let c2 = getCateFromString "(s\\.np)/.np"
      let c3 = getCateFromString "s/.np"
      let c4 = getCateFromString "s\\.np"
      let pc1 = createPhraCate 0 0 c1 "Desig" "Frank'" True 0
      let pc2 = createPhraCate 1 0 c2 "Desig" "loves'" True 1
      let pc3 = createPhraCate 0 1 c3 ">T->B" "Frank' loves'" True 1
      let pc4 = createPhraCate 1 1 c4 ">" "loves' Mary'" True 2
      let pcs = [pc1,pc2]
      let pcClo = pcs ++ [pc3,pc4]
      getOverlap pcClo `shouldBe` [(pc3,pc4)]

    it "The result of prune \"+++++\" [((0,0),[(np,\"Desig\",\"Frank'\",True)],0), ((1,0),[(s\\.np)/.np,\"Desig\",\"loves'\",True)],1), ((2,0),[(s\\.np,\">\",\"Mary'\",True)],2), ((0,1),[(s/.np,\">T->B\",\"Frank' loves'\",True)],1), ((1,1),[(s\\.np,\"<\",\"loves' Mary'\",True)],2)] is [((0,0),[(np,\"Desig\",\"Frank'\",True)],0), ((1,0),[(s\\.np)/.np,\"Desig\",\"loves'\",True)],1), ((0,1),[(s/.np,\">T->B\",\"Frank' loves'\",True)],1)]" $ do
      let c01 = npCate
      let c02 = getCateFromString "(s\\.np)/.np"
      let c03 = npCate
      let c11 = getCateFromString "s/.np"
      let c12 = getCateFromString "s\\.np"
      let pc01 = createPhraCate 0 0 c01 "Desig" "Frank'" True 0
      let pc02 = createPhraCate 1 0 c02 "Desig" "loves'" True 1
      let pc03 = createPhraCate 2 0 c03 "Desig" "Mary'" True 2
      let pc11 = createPhraCate 0 1 c11 ">T->B" "Frank' loves'" True 1
      let pc12 = createPhraCate 1 1 c12 ">" "loves' Mary'" True 2
      let pcs = [pc01,pc02,pc03]
      let pcClo = pcs ++ [pc11,pc12]
      let pcClo' = pcs ++ [pc12]
      prune "+++++" pcClo `shouldBe` pcClo'

    it "The result of parse \"+++++\" [((0,0),[(np,\"Desig\",\"Frank'\",True)],0), ((1,0),[((s\\.np)/.np,\"Desig\",\"loves'\",True)],1)] is [((0,0),[(np,\"Desig\",\"Frank'\",False)],0), ((1,0),[(s\\.np)/.np,\"Desig\",\"loves'\",False)],1), ((0,1),[(s/.np,\">T->B\",\"Frank' loves'\",True)],1)]" $ do
      let c01 = npCate
      let c02 = getCateFromString "(s\\.np)/.np"
      let c11 = getCateFromString "s/.np"
      let pc01 = createPhraCate 0 0 c01 "Desig" "Frank'" True 0
      let pc010 = deactOnePC pc01
      let pc02 = createPhraCate 1 0 c02 "Desig" "loves'" True 1
      let pc020 = deactOnePC pc02
      let pc11 = createPhraCate 0 1 c11 ">T->B" "Frank' loves'" True 1
      let pcs = [pc01,pc02]
      let pcs0 = [pc010,pc020]
      let pcClo0 = pcs0 ++ [pc11]
      parse "+++++" pcs `shouldBe` pcClo0

    it "The result of removeOneActPC \"+++++\" ((0,1),[(s/.np,\">T->B\",\"Frank' loves'\",True)],1) [((0,0),[(np,\"Desig\",\"Frank'\",False)],0), ((1,0),[((s\\.np)/.np,\"Desig\",\"loves'\",False)],1), ((2,0),[(np,\"Desig\",\"Mary'\",False)],2)，((0,1),[(s/.np,\">T->B\",\"Frank' loves'\",True), ((1,1),[(s\\.np,\">\",\"loves' Mary'\",True)],2)] is [((0,0),[(np,\"Desig\",\"Frank'\",True)],0), ((1,0),[((s\\.np)/.np,\"Desig\",\"loves'\",False)],1), ((2,0),[(np,\"Desig\",\"Mary'\",False)],2)，((1,1),[(s\\.np,\">\",\"loves' Mary'\",True)],2)]" $ do
      let c01 = npCate
      let c02 = getCateFromString "(s\\.np)/.np"
      let c03 = npCate
      let c11 = getCateFromString "s/.np"
      let c12 = getCateFromString "s\\.np"
      let pc01 = createPhraCate 0 0 c01 "Desig" "Frank'" False 0
      let pc02 = createPhraCate 1 0 c02 "Desig" "loves'" False 1
      let pc03 = createPhraCate 2 0 c03 "Desig" "Mary'" False 2
      let pc11 = createPhraCate 0 1 c11 ">T->B" "Frank' loves'" True 1
      let pc12 = createPhraCate 1 1 c12 ">" "loves' Mary'" True 2
      let pc010 = deactOnePC pc01
      let pcs = [pc01,pc02,pc03]
      let pcs0 = [pc010,pc02,pc03]
      let pcClo = pcs ++ [pc11,pc12]
      let pcClo0 = pcs0 ++ [pc12]
      remOneActPC "+++++" pc11 pcClo `shouldBe` pcClo0
    
    it "The result of newSpanPCs \"+++++\" [((0,0),[(np,\"Desig\",\"Frank'\",True)],0), ((1,0),[((s\\.np)/.np,\"Desig\",\"loves'\",True)],1), ((2,0),[(np,\"Desig\",\"Mary'\",True)],2)] is [((1,0),[((s\\.np)/.np,\"Desig\",\"loves'\",False)],1), ((2,0),[(np,\"Desig\",\"Mary'\",False)],2), ((0,0),[(np,\"Desig\",\"Frank'\",True)],0), ((1,1),[(s\\.np,\">\",\"loves' Mary'\",True)],2)]" $ do
      let c01 = npCate
      let c02 = getCateFromString "(s\\.np)/.np"
      let c03 = npCate
      let c11 = getCateFromString "s\\.np"
      let pc01 = createPhraCate 0 0 c01 "Desig" "Frank'" True 0
      let pc02 = createPhraCate 1 0 c02 "Desig" "loves'" True 1
      let pc03 = createPhraCate 2 0 c03 "Desig" "Mary'" True 2
      let pc11 = createPhraCate 1 1 c11 ">" "loves' Mary'" True 2
      let pcs = [pc01,pc02,pc03]
      let pc020 = deactOnePC pc02
      let pc030 = deactOnePC pc03
      let pc110 = deactOnePC pc11
      let pcClo = [pc020,pc030,pc01,pc11]
      newSpanPCs "+++++" pcs `shouldBe` pcClo
    
    it "The result of parse \"+++++\" [((0,0),[(np,\"Desig\",\"Frank'\",True)],0), ((1,0),[((s\\.np)/.np,\"Desig\",\"loves'\",True)],1), ((2,0),[(np,\"Desig\",\"Mary'\",True)],2)] is [((0,0),[(np,\"Desig\",\"Frank'\",False)],0), ((1,0),[((s\\.np)/.np,\"Desig\",\"loves'\",False)],1), ((2,0),[(np,\"Desig\",\"Mary'\",False)],2), ((1,1),[(s\\.np,\">\",\"loves' Mary'\",False)],2), ((0,2),[(s,\"<\",\"(loves' Mary') Frank'\",True)]" $ do
      let c01 = npCate
      let c02 = getCateFromString "(s\\.np)/.np"
      let c03 = npCate
      let c11 = getCateFromString "s\\.np"
      let c21 = sCate
      let pc01 = createPhraCate 0 0 c01 "Desig" "Frank'" True 0
      let pc02 = createPhraCate 1 0 c02 "Desig" "loves'" True 1
      let pc03 = createPhraCate 2 0 c03 "Desig" "Mary'" True 2
      let pc010 = deactOnePC pc01
      let pc020 = deactOnePC pc02
      let pc030 = deactOnePC pc03
      let pc11 = createPhraCate 1 1 c11 ">" "loves' Mary'" False 2
      let pc21 = createPhraCate 0 2 c21 "<" "(loves' Mary') Frank'" True 1
      let pcs = [pc01,pc02,pc03]
      let pcClo = [pc020,pc030,pc010,pc11,pc21]
      parse "+++++" pcs `shouldBe` pcClo

    it "The result of parse \"+++++\" [((0,0),[(np/.np,\"Desig\",\"Brave'\"，True)],0), ((1,0),[(np,\"Desig\",\"Frank'\",True)],1), ((2,0),[(s\\*np,\"Desig\",\"wins'\",True)],2)] is [((0,0),[(np/.np,\"Desig\",\"Brave'\",True)],0), ((1,0),[(np,\"Desig\",\"Frank'\",True)],1), ((2,0),[(s\\*np,\"Desig\",\"wins'\",True)],2), ((0,1),[(np,\">\",\"Brave' Frank'\",True)],1), ((1,1),[(s,\"<\",\"wins' Frank'\",True),(np/.np,\"A/n->B\",\"Brave' Frank'\",True)],2), ((0,2),[(np,\"Np/s->\",\"Brave' (wins' Frank')\",True)],1), ((0,2),[(s,\"<\",\"wins' (Brave' Frank')\",True),(np,\"Np/v->\",\"(Brave' Frank') wins'\",True),(s,\"Np/a-<\",\"wins' (Brave' Frank')\",True)],2)], without considering element order." $ do
      let c01 = getCateFromString "np/.np"
      let c02 = npCate
      let c03 = getCateFromString "s\\.np"
      let c11 = npCate
      let c21 = sCate
      let pc01 = createPhraCate 0 0 c01 "Desig" "Brave'" True 0
      let pc02 = createPhraCate 1 0 c02 "Desig" "Frank'" True 1
      let pc03 = createPhraCate 2 0 c03 "Desig" "wins'" True 2
      let pc11 = createPhraCate 0 1 c11 ">" "Brave' Frank'" False 1
      let pc21 = createPhraCate 0 2 c21 "<" "wins' (Brave' Frank')" True 2 
      let pcs = [pc01,pc02,pc03]
      let pc010 = deactOnePC pc01
      let pc020 = deactOnePC pc02
      let pc030 = deactOnePC pc03
      let pcClo = [pc010,pc020,pc11,pc030,pc21]
      parse "+++++" pcs `shouldBe` pcClo

    it "The result of getNameOfPhraCate \"+++++\" ((0,0),[(np/.np,\"Desig\",\"Brave'\"，True)],0) [((0,0),[(np/.np,\"Desig\",\"Brave'\",True)],0), ((1,0),[(np,\"Desig\",\"Frank'\",True)],1), ((2,0),[(s\\*np,\"Desig\",\"wins'\",True)],2), ((0,1),[(np,\">\",\"Brave' Frank'\",True)],1), ((1,1),[(s,\"<\",\"wins' Frank'\",True)],2), ((0,2),[(np,\"Np/s->\",\"Brave' (wins' Frank')\",True)],1), ((0,2),[(s,\"<\",\"wins' (Brave' Frank')\",True)] is \"DE\"" $ do
      let c01 = getCateFromString "np/.np"
      let c02 = npCate
      let c03 = getCateFromString "s\\*np"
      let c11 = npCate
      let c12 = sCate
      let c21 = npCate
      let c22 = sCate
      let pc01 = createPhraCate 0 0 c01 "Desig" "Brave'" True 0
      let pc02 = createPhraCate 1 0 c02 "Desig" "Frank'" True 1
      let pc03 = createPhraCate 2 0 c03 "Desig" "wins'" True 2
      let pc11 = createPhraCate 0 1 c11 ">" "Brave' Frank'" True 1
      let pc12 = createPhraCate 1 1 c12 "<" "wins' Frank'" True 2
      let pc21 = createPhraCate 0 2 c21 "Np/s->" "Brave' (wins' Frank')" True 1
      let pc22 = createPhraCate 0 2 c22 "<" "wins' (Brave' Frank')" True 2 
      let pcs = [pc01,pc02,pc03]
      let pcClo = pcs ++ [pc11,pc12,pc21,pc22]
      getNameOfPhraCate "+++++" pc01 pcClo `shouldBe` "DE"

    it "The result of getNameOfPhraCate \"+++++\" ((0,1),[(np,\">\",\"Brave' Frank'\",True)],1) [((0,0),[(np/.np,\"Desig\",\"Brave'\",True)],0), ((1,0),[(np,\"Desig\",\"Frank'\",True)],1), ((2,0),[(s\\*np,\"Desig\",\"wins'\",True)],2), ((0,1),[(np,\">\",\"Brave' Frank'\",True)],1), ((1,1),[(s,\"<\",\"wins' Frank'\",True)],2), ((0,2),[(np,\"Np/s->\",\"Brave' (wins' Frank')\",True)],1), ((0,2),[(s,\"<\",\"wins' (Brave' Frank')\",True)],2) is \"AHn\"" $ do
      let c01 = getCateFromString "np/.np"
      let c02 = npCate
      let c03 = getCateFromString "s\\*np"
      let c11 = npCate
      let c12 = sCate
      let c21 = npCate
      let c22 = sCate
      let pc01 = createPhraCate 0 0 c01 "Desig" "Brave'" True 0
      let pc02 = createPhraCate 1 0 c02 "Desig" "Frank'" True 1
      let pc03 = createPhraCate 2 0 c03 "Desig" "wins'" True 2
      let pc11 = createPhraCate 0 1 c11 ">" "Brave' Frank'" True 1
      let pc12 = createPhraCate 1 1 c12 "<" "wins' Frank'" True 2
      let pc21 = createPhraCate 0 2 c21 "Np/s->" "Brave' (wins' Frank')" True 1
      let pc22 = createPhraCate 0 2 c22 "<" "wins' (Brave' Frank')" True 2 
      let pcs = [pc01,pc02,pc03]
      let pcClo = pcs ++ [pc11,pc12,pc21,pc22]
      getNameOfPhraCate "+++++" pc11 pcClo `shouldBe` "AHn"

    it "The result of getNameOfPhraCate \"+++++\" ((0,2),[(np,\"Np/s->\",\"Brave' (wins' Frank')\",True)],1) [((0,0),[(np/.np,\"Desig\",\"Brave'\",False)],0), ((1,0),[(np,\"Desig\",\"Frank'\",False)],1), ((2,0),[(s\\*np,\"Desig\",\"wins'\",False)],2), ((0,1),[(np,\">\",\"Brave' Frank'\",False)],1), ((1,1),[(s,\"<\",\"wins' Frank'\",False)],2), ((0,2),[(np,\"Np/s->\",\"Brave' (wins' Frank')\",True)],1), ((0,2),[(s,\"<\",\"wins' (Brave' Frank')\",True)], 2)] is \"AHn\"" $ do
      let c01 = getCateFromString "np/.np"
      let c02 = npCate
      let c03 = getCateFromString "s\\*np"
      let c11 = npCate
      let c12 = sCate
      let c21 = npCate
      let c22 = sCate
      let pc01 = createPhraCate 0 0 c01 "Desig" "Brave'" True 0
      let pc02 = createPhraCate 1 0 c02 "Desig" "Frank'" True 1
      let pc03 = createPhraCate 2 0 c03 "Desig" "wins'" True 2
      let pc11 = createPhraCate 0 1 c11 ">" "Brave' Frank'" True 1
      let pc12 = createPhraCate 1 1 c12 "<" "wins' Frank'" True 2
      let pc21 = createPhraCate 0 2 c21 "Np/s->" "Brave' (wins' Frank')" True 1
      let pc22 = createPhraCate 0 2 c22 "<" "wins' (Brave' Frank')" True 2 
      let pcs = [pc01,pc02,pc03]
      let pcClo = pcs ++ [pc11,pc12,pc21,pc22]
      getNameOfPhraCate "+++++" pc21 pcClo `shouldBe` "AHn"

    it "The result of getNameOfPhraCate \"+++++\" ((0,2),[(s,\"<\",\"wins' (Brave' Frank')\",True)], 2) [((0,0),[(np/.np,\"Desig\",\"Brave'\",False)],0), ((1,0),[(np,\"Desig\",\"Frank'\",False)],1), ((2,0),[(s\\*np,\"Desig\",\"wins'\",False)],2), ((0,1),[(np,\">\",\"Brave' Frank'\",False)],1), ((1,1),[(s,\"<\",\"wins' Frank'\",False)],2), ((0,2),[(np,\"Np/s->\",\"Brave' (wins' Frank')\",True)],1), ((0,2),[(s,\"<\",\"wins' (Brave' Frank')\",True)], 2)] is \"AHn\"" $ do
      let c01 = getCateFromString "np/.np"
      let c02 = npCate
      let c03 = getCateFromString "s\\*np"
      let c11 = npCate
      let c12 = sCate
      let c21 = npCate
      let c22 = sCate
      let pc01 = createPhraCate 0 0 c01 "Desig" "Brave'" True 0
      let pc02 = createPhraCate 1 0 c02 "Desig" "Frank'" True 1
      let pc03 = createPhraCate 2 0 c03 "Desig" "wins'" True 2
      let pc11 = createPhraCate 0 1 c11 ">" "Brave' Frank'" True 1
      let pc12 = createPhraCate 1 1 c12 "<" "wins' Frank'" True 2
      let pc21 = createPhraCate 0 2 c21 "Np/s->" "Brave' (wins' Frank')" True 1
      let pc22 = createPhraCate 0 2 c22 "<" "wins' (Brave' Frank')" True 2 
      let pcs = [pc01,pc02,pc03]
      let pcClo = pcs ++ [pc11,pc12,pc21,pc22]
      getNameOfPhraCate "+++++" pc22 pcClo `shouldBe` "SP"

    it "The result of getNameOfPhraCate \"+++++\"  ((1,1),[((s\\.np)/.np,\">B\",\"is_better_than' bad'\",False)],2) [((0,0),[(np/.np,\"Desig\",\"Good'\",False)],0), ((1,0),[((s\\.np)/.np,\"Desig\",\"is_better_than'\",False)],1), ((2,0),[(np/.np,\"Desig\",\"bad'\",False)],2), ((0,1),[((s\\.np)/.np,\">B\",\"is_better_than' bad'\",True)],1), ((0,1),[(s/.np,\"Np/a->T->B\",\"Good' is_better_than'\",True)],1),((1,1),[((s\\.np)/.np,\">B\",\"is_better_than' bad'\",True)],2),((1,1),[(s\\.np,\"Np/a->\",\"is_better_than' bad'\",True)],2)] is \"NR\"" $ do
      let c01 = getCateFromString "np/.np"
      let c02 = getCateFromString "(s\\.np)/.np"
      let c03 = getCateFromString "np/.np"
      let c12 = getCateFromString "s/.np"
      let c13 = getCateFromString "(s\\.np)/.np"
      let c14 = getCateFromString "s\\.np"
      let pc01 = createPhraCate 0 0 c01 "Desig" "Good'" True 0
      let pc02 = createPhraCate 1 0 c02 "Desig" "is_better_than'" True 1
      let pc03 = createPhraCate 2 0 c03 "Desig" "bad'" True 2
      let pc12 = createPhraCate 0 1 c12 "Np/a->T->B" "Good' is_better_than'" True 1
      let pc13 = createPhraCate 1 1 c13 ">B" "is_better_than' bad'" True 2
      let pc14 = createPhraCate 1 1 c14 "Np/a-<" "is_better_than' bad'" True 2
      let pcClo = [pc01,pc02,pc03,pc12,pc13,pc14]
      getNameOfPhraCate "+++++" pc13 [pc01,pc02,pc03,pc12,pc13,pc14] `shouldBe` "NR"

    it "The result of parse \"+++++\"  [((0,0),[(np/.np,\"Desig\",\"Good'\",True)],0), ((1,0),[((s\\.np)/.np,\"Desig\",\"is_better_than'\",True)],1), ((2,0),[(np/.np,\"Desig\",\"bad'\",True)],2)] is [((0,0),[(np/.np,\"Desig\",\"Good'\",False)],0), ((1,0),[((s\\.np)/.np,\"Desig\",\"is_better_than'\",False)],1), ((2,0),[(np/.np,\"Desig\",\"bad'\",False)],2), ((1,1),[(s\\.np,\"Np/a->\",\"is_better_than' bad'\",False)],2),((0,2),[(s,\"Np/a-<\",\"(is_better_than' bad') Good'\",True)],2)]" $ do
      let c01 = getCateFromString "np/.np"
      let c02 = getCateFromString "(s\\.np)/.np"
      let c03 = getCateFromString "np/.np"
      let c11 = getCateFromString "s\\.np"
      let c21 = sCate
      let pc01 = createPhraCate 0 0 c01 "Desig" "Good'" True 0
      let pc02 = createPhraCate 1 0 c02 "Desig" "is_better_than'" True 1
      let pc03 = createPhraCate 2 0 c03 "Desig" "bad'" True 2
      let pc010 = deactOnePC pc01
      let pc020 = deactOnePC pc02
      let pc030 = deactOnePC pc03
      let pc11 = createPhraCate 1 1 c11 "Np/a->" "is_better_than' bad'" False 2
      let pc21 = createPhraCate 0 2 c21 "Np/a-<" "(is_better_than' bad') Good'" True 1
      let pcs = [pc01,pc02,pc03]
      let pcClo = [pc020,pc030,pc010,pc11,pc21]
      parse "+++++" pcs `shouldBe` pcClo


    it "The result of isPrior \"+++++\" [((0,0),[(np/.np,\"Desig\",\"Brave'\",True)],0), ((1,0),[(np,\"Desig\",\"Frank'\",True)],1), ((2,0),[(s\\*np,\"Desig\",\"wins'\",True)],2), ((0,1),[(np,\">\",\"Brave' Frank'\",True)],1), ((1,1),[(s,\"<\",\"wins' Frank'\",True)],2)] ((0,1),[(np,\">\",\"Brave' Frank'\",True)],1) ((1,1),[(s,\"<\",\"wins' Frank'\",True)],2) is False" $ do
      let c01 = getCateFromString "np/.np"
      let c02 = npCate
      let c03 = getCateFromString "s\\*np"
      let c11 = npCate
      let c12 = sCate
      let pc01 = createPhraCate 0 0 c01 "Desig" "Brave'" True 0
      let pc02 = createPhraCate 1 0 c02 "Desig" "Frank'" True 1
      let pc03 = createPhraCate 2 0 c03 "Desig" "wins'" True 2
      let pc11 = createPhraCate 0 1 c11 ">" "Brave' Frank'" True 1
      let pc12 = createPhraCate 1 1 c12 "<" "wins' Frank'" True 2
      let pcs = [pc01,pc02,pc03]
      let pcClo = pcs ++ [pc11,pc12]
      isPrior "+++++" pcClo pc11 pc12 `shouldBe` True 

    it "The result of isPrior \"+++++\"  [((0,0),[(np/.np,\"Desig\",\"Good'\",True)],0), ((1,0),[((s\\.np)/.np,\"Desig\",\"is_better_than'\",True)],1), ((2,0),[(np/.np,\"Desig\",\"bad'\",True)],2), ((0,1),[(s/.np,\"Np/a->T->B\",\"Good' is_better_than'\",True)],1),((1,1),[((s\\.np)/.np,\">B\",\"is_better_than' bad'\",True)],2), ((1,1),[(s\\.np,\"Np/a->\",\"is_better_than' bad'\",True)],2)] ((0,1),[(s/.np,\"Np/a->T->B\",\"Good' is_better_than'\",True)],1),((1,1),[((s\\.np)/.np,\">B\",\"is_better_than' bad'\",True)],2) is False" $ do
      let c01 = getCateFromString "np/.np"
      let c02 = getCateFromString "(s\\.np)/.np"
      let c03 = getCateFromString "np/.np"
      let c11 = getCateFromString "s/.np"
      let c12 = getCateFromString "(s\\.np)/.np"
      let c13 = getCateFromString "s\\.np"
      let pc01 = createPhraCate 0 0 c01 "Desig" "Good'" True 0
      let pc02 = createPhraCate 1 0 c02 "Desig" "is_better_than'" True 1
      let pc03 = createPhraCate 2 0 c03 "Desig" "bad'" True 2
      let pc11 = createPhraCate 0 1 c11 "Np/a->T->B" "Good' is_better_than'" True 1
      let pc12 = createPhraCate 1 1 c12 ">B" "is_better_than' bad'" True 2
      let pc13 = createPhraCate 1 1 c13 "Np/a->" "is_better_than' bad'" True 2
      let pcClo = [pc01,pc02,pc03,pc11,pc12,pc13]
      isPrior "+++++" [pc01,pc02,pc03,pc11,pc12,pc13] pc12 pc13 `shouldBe` False 
 
    it "The result of atomizePhraCate [((0,0),[(np,\"Desig\",\"Frank\")],1), ((1,1),[(s\\.np,\">\",\"loves' Mary'\"),((s/.np,\">T->B\",\"Tim' loves'\"))],2)] is [((0,0),[(np,\"Desig\",\"Frank\")],1), ((1,1),[(s\\.np,\">\",\"loves' Mary'\"),2)],2), ((1,1),[(s/.np,\">T->B\",\"Tim' loves'\")],2)]" $ do
      let pc1 = createPhraCate 0 0 npCate "Desig" "Frank" True 1
      let pc2 = createPhraCate2 1 1 [(getCateFromString "s\\.np",">","loves' Mary'",True),(getCateFromString "s/.np",">T->B","Tom' loves'",True)] 2
      let pc21 = createPhraCate 1 1 (getCateFromString "s\\.np") ">" "loves' Mary'" True 2
      let pc22 = createPhraCate 1 1 (getCateFromString "s/.np") ">T->B" "Tom' loves'" True 2
      atomizePhraCate [pc1,pc2] `shouldBe` [pc1,pc21,pc22]

    it "The result of findCate （0,-1) [((0,0),[(np,\"Desig\",\"Frank'\")],0), ((1,0),[(s\\.np,\"Desig\",\"smiles\")],1)] is []" $ do
      let c1 = npCate
      let c2 = getCateFromString "s\\.np"
      findCate (0,-1) [((0,0),[(c1,"Desig","Frank'",True)],0), ((1,0),[(c2,"Desig","smiles'",True)],1)] `shouldBe` []

    it "The result of findCate （0,0) [((0,0),[(np,\"Desig\",\"Frank'\")],0), ((1,0),[(s\\.np,\"Desig\",\"loves' Mary'\")],1)] is ((0,0),[(np,\"Desig\",\"Frank'\")],0)" $ do
      let c1 = npCate
      let c2 = getCateFromString "s\\.np"
      let pc1 = createPhraCate 0 0 c1 "Desig" "Frank'" True 0
      let pc2 = createPhraCate 1 0 c2 "Desig" "loves' Mary'" True 1
      findCate (0,0) [pc1, pc2] `shouldBe` [pc1]

    it "The result of findSplitCate ((1,0),[(np,\"Desig\",\"Frank'\")],1) [((0,0),[(np/.np,\"Desig\",\"Brave'\")],0), ((1,0),[(np,\"Desig\",\"Frank'\")],1), ((2,0),[(s\\*np,\"Desig\"),\"wins'\"],2), ((0,1),[(np,\">\",\"Brave' Frank'\")],1), ((1,1),[(s,\"<\",\"wins' Frank'\")],2), ((0,2),[np,\"Np/s->\",\"Brave' (wins' Frank')\"],1), ((0,2),[(s,\"<\",\"wins' (Brave' Frank')\")],2)] is []" $ do
      let c01 = getCateFromString "np/.np"
      let c02 = npCate
      let c03 = getCateFromString "s\\*np"
      let c11 = npCate
      let c12 = sCate
      let c21 = npCate
      let c22 = sCate
      let pc01 = createPhraCate 0 0 c01 "Desig" "Brave'" True 0
      let pc02 = createPhraCate 1 0 c02 "Desig" "Frank'" True 1
      let pc03 = createPhraCate 2 0 c03 "Desig" "wins'" True 2
      let pc11 = createPhraCate 0 1 c11 ">" "Brave' Frank'" True 1
      let pc12 = createPhraCate 1 1 c12 "<" "wins' Frank'" True 2
      let pc21 = createPhraCate 0 2 c21 "Np/s->" "Brave' (wins' Frank')" True 1
      let pc22 = createPhraCate 0 2 c22 "<" "wins' (Brave' Frank')" True 2 
      let pcs = [pc01,pc02,pc03]
      let pcClo = pcs ++ [pc11,pc12,pc21,pc22]
      findSplitCate "+++++" pc02 pcClo `shouldBe` []

    it "The result of findSplitCate ((0,2),[(s,\"<\",\"wins' (Brave' Frank')\")],2) [((0,0),[(np/.np,\"Desig\",\"Brave'\")],0), ((1,0),[(np,\"Desig\",\"Frank'\")],1), ((2,0),[(s\\*np,\"Desig\"),\"wins'\"],2), ((0,1),[(np,\">\",\"Brave' Frank'\")],1), ((1,1),[(s,\"<\",\"wins' Frank'\")],2), ((0,2),[np,\"Np/s->\",\"Brave' (wins' Frank')\"],1), ((0,2),[(s,\"<\",\"wins' (Brave' Frank')\")],2)] is [((0,1),[(np,\">\",\"Brave' Frank'\")],1), (2,0),[(s\\.np,\"Desig\",\"wins'\")],2)]" $ do
      let c01 = getCateFromString "np/.np"
      let c02 = npCate
      let c03 = getCateFromString "s\\*np"
      let c11 = npCate
      let c12 = sCate
      let c21 = npCate
      let c22 = sCate
      let pc01 = createPhraCate 0 0 c01 "Desig" "Brave'" True 0
      let pc02 = createPhraCate 1 0 c02 "Desig" "Frank'" True 1
      let pc03 = createPhraCate 2 0 c03 "Desig" "wins'" True 2
      let pc11 = createPhraCate 0 1 c11 ">" "Brave' Frank'" True 1
      let pc12 = createPhraCate 1 1 c12 "<" "wins' Frank'" True 2
      let pc21 = createPhraCate 0 2 c21 "Np/s->" "Brave' (wins' Frank')" True 1
      let pc22 = createPhraCate 0 2 c22 "<" "wins' (Brave' Frank')" True 2 
      let pcs = [pc01,pc02,pc03]
      let pcClo = pcs ++ [pc11,pc12,pc21,pc22]
      findSplitCate "+++++" pc22 pcClo `shouldBe` [(pc11,pc03)]
    
    it "The result of findSplitCate ((0,2),[(s,\"<\",\"wins' (Brave' Frank')\")],2) [((0,0),[(np/.np,\"Desig\",\"Brave'\")],0), ((1,0),[(np,\"Desig\",\"Frank'\")],1), ((2,0),[(s\\*np,\"Desig\"),\"wins'\"],2), ((0,1),[(np,\">\",\"Brave' Frank'\")],1), ((1,1),[(s,\"<\",\"wins' Frank'\")],2), ((0,2),[np,\"Np/s->\",\"Brave' (wins' Frank')\"],1), ((0,2),[(s,\"<\",\"wins' (Brave' Frank')\")],2)] is [((0,1),[(np,\">\",\"Brave' Frank'\")],1), (2,0),[(s\\.np,\"Desig\",\"wins'\")],2)]" $ do
      let c01 = getCateFromString "np/.np"
      let c02 = npCate
      let c03 = getCateFromString "s\\*np"
      let c11 = npCate
      let c12 = sCate
      let c21 = npCate
      let c22 = sCate
      let pc01 = createPhraCate 0 0 c01 "Desig" "Brave'" True 0
      let pc02 = createPhraCate 1 0 c02 "Desig" "Frank'" True 1
      let pc03 = createPhraCate 2 0 c03 "Desig" "wins'" True 2
      let pc11 = createPhraCate 0 1 c11 ">" "Brave' Frank'" True 1
      let pc12 = createPhraCate 1 1 c12 "<" "wins' Frank'" True 2
      let pc21 = createPhraCate 0 2 c21 "Np/s->" "Brave' (wins' Frank')" True 1
      let pc22 = createPhraCate 0 2 c22 "<" "wins' (Brave' Frank')" True 2 
      let pcs = [pc01,pc02,pc03]
      let pcClo = pcs ++ [pc11,pc12,pc21,pc22]
      findSplitCate "+++++" pc22 pcClo `shouldBe` [(pc11,pc03)]

    it "The result of findSplitCate ((1,1),[(s,\"<\",\"wins' Frank'\")],2) [((0,0),[(np/.np,\"Desig\",\"Brave'\")],0), ((1,0),[(np,\"Desig\",\"Frank'\")],1), ((2,0),[(s\\*np,\"Desig\"),\"wins'\"],2), ((0,1),[(np,\">\",\"Brave' Frank'\")],1), ((1,1),[(s,\"<\",\"wins' Frank'\")],2), ((0,2),[np,\"Np/s->\",\"Brave' (wins' Frank')\"],1), ((0,2),[(s,\"<\",\"wins' (Brave' Frank')\")],2)] is [((1,0),[(np,\"Desig\",\"Frank'\")],1), (2,0),[(s\\*np,\"Desig\",\"wins'\")],2)]" $ do
      let c01 = getCateFromString "np/.np"
      let c02 = npCate
      let c03 = getCateFromString "s\\*np"
      let c11 = npCate
      let c12 = sCate
      let c21 = npCate
      let c22 = sCate
      let pc01 = createPhraCate 0 0 c01 "Desig" "Brave'" True 0
      let pc02 = createPhraCate 1 0 c02 "Desig" "Frank'" True 1
      let pc03 = createPhraCate 2 0 c03 "Desig" "wins'" True 2
      let pc11 = createPhraCate 0 1 c11 ">" "Brave' Frank'" True 1
      let pc12 = createPhraCate 1 1 c12 "<" "wins' Frank'" True 2
      let pc21 = createPhraCate 0 2 c21 "Np/s->" "Brave' (wins' Frank')" True 1
      let pc22 = createPhraCate 0 2 c22 "<" "wins' (Brave' Frank')" True 2
      let pcs = [pc01,pc02,pc03]
      let pcClo = pcs ++ [pc11,pc12,pc21,pc22]
      findSplitCate "+++++" pc12 pcClo `shouldBe` [(pc02,pc03)]

    it "The result of findSplitCate ((0,2),[(s,\"Np/a-<\",\"(is_better_than' bad') Good'\",False)],1)  [((0,0),[(np/.np,\"Desig\",\"Good'\",False)],0), ((1,0),[((s\\.np)/.np,\"Desig\",\"is_better_than'\",False)],1), ((2,0),[(np/.np,\"Desig\",\"bad'\",False)],2), ((1,1),[(s\\.np,\"Np/a->\",\"is_better_than' bad'\"),False],2), ((0,2),[(s,\"Np/a-<\",\"(is_better_than' bad') Good'\",True)],1) is [(((0,0),[(np/.np,\"Desig\",\"Good'\",False),0),((1,1),[(s\\.np,\"Np/a->\",\"is_better_than' bad'\",False)],2)]" $ do
      let c01 = getCateFromString "np/.np"
      let c02 = getCateFromString "(s\\.np)/.np"
      let c03 = getCateFromString "np/.np"
      let c11 = getCateFromString "s\\.np"
      let c21 = sCate
      let pc01 = createPhraCate 0 0 c01 "Desig" "Good'" False 0
      let pc02 = createPhraCate 1 0 c02 "Desig" "is_better_than'" False 1
      let pc03 = createPhraCate 2 0 c03 "Desig" "bad'" False 2
      let pc11 = createPhraCate 1 1 c11 "Np/a->" "is_better_than' bad'" False 2
      let pc21 = createPhraCate 0 2 c21 "Np/a-<" "(is_better_than' bad') Good'" True 1
      let pcClo = [pc01,pc02,pc03,pc11,pc21]
      findSplitCate "+++++" pc21 pcClo `shouldBe` [(pc01,pc11)]

    it "The result of findTipsOfTree [((1,1),[(s,\"<\",\"wins' Frank'\")],2)] [((0,0),[(np/.np,\"Desig\",\"Brave'\")],0), ((1,0),[(np,\"Desig\",\"Frank'\")],1), ((2,0),[(s\\*np,\"Desig\"),\"wins'\"],2), ((0,1),[(np,\">\",\"Brave' Frank'\")],1), ((1,1),[(s,\"<\",\"wins' Frank'\")],2), ((0,2),[np,\"Np/s->\",\"Brave' (wins' Frank')\"],1), ((0,2),[(s,\"<\",\"wins' (Brave' Frank')\")],2)] is [((1,1),[(s,\"<\",\"wins' Frank'\")],2)]" $ do
      let c01 = getCateFromString "np/.np"
      let c02 = npCate
      let c03 = getCateFromString "s\\*np"
      let c11 = npCate
      let c12 = sCate
      let c21 = npCate
      let c22 = sCate
      let pc01 = createPhraCate 0 0 c01 "Desig" "Brave'" True 0
      let pc02 = createPhraCate 1 0 c02 "Desig" "Frank'" True 1
      let pc03 = createPhraCate 2 0 c03 "Desig" "wins'" True 2
      let pc11 = createPhraCate 0 1 c11 ">" "Brave' Frank'" True 1
      let pc12 = createPhraCate 1 1 c12 "<" "wins' Frank'" True 2
      let pc21 = createPhraCate 0 2 c21 "Np/s->" "Brave' (wins' Frank')" True 1
      let pc22 = createPhraCate 0 2 c22 "<" "wins' (Brave' Frank')" True 2
      let pcs = [pc01,pc02,pc03]
      let pcClo = pcs ++ [pc11,pc12,pc21,pc22]
      findTipsOfTree "+++++" [pc12] pcClo `shouldBe` [pc12]

    it "The result of findTipsOfTree [((0,2),[(s,\"<\",\"wins' (Brave' Frank')\")],2),((0,1),[(np,\">\",\"Brave' Frank'\")],1),((2,0),[(s\\*np,\"Desig\",\"wins'\")],2)] [((0,0),[(np/.np,\"Desig\",\"Brave'\")],0), ((1,0),[(np,\"Desig\",\"Frank'\")],1), ((2,0),[(s\\*np,\"Desig\"),\"wins'\"],2), ((0,1),[(np,\">\",\"Brave' Frank'\")],1), ((1,1),[(s,\"<\",\"wins' Frank'\")],2), ((0,2),[np,\"Np/s->\",\"Brave' (wins' Frank')\"],1), ((0,2),[(s,\"<\",\"wins' (Brave' Frank')\")],2)] is [((0,1),[(np,\">\",\"Brave' Frank'\")],1)]" $ do
      let c01 = getCateFromString "np/.np"
      let c02 = npCate
      let c03 = getCateFromString "s\\*np"
      let c11 = npCate
      let c12 = sCate
      let c21 = npCate
      let c22 = sCate
      let pc01 = createPhraCate 0 0 c01 "Desig" "Brave'" True 0
      let pc02 = createPhraCate 1 0 c02 "Desig" "Frank'" True 1
      let pc03 = createPhraCate 2 0 c03 "Desig" "wins'" True 2
      let pc11 = createPhraCate 0 1 c11 ">" "Brave' Frank'" True 1
      let pc12 = createPhraCate 1 1 c12 "<" "wins' Frank'" True 2
      let pc21 = createPhraCate 0 2 c21 "Np/s->" "Brave' (wins' Frank')" True 1
      let pc22 = createPhraCate 0 2 c22 "<" "wins' (Brave' Frank')" True 2
      let pcs = [pc01,pc02,pc03]
      let pcClo = pcs ++ [pc11,pc12,pc21,pc22]
      findTipsOfTree "+++++" [pc22,pc11,pc03] pcClo `shouldBe` [pc11]

    it "The result of findTipsOfTree \"+++++\" [((0,0),[(np/.np,\"Desig\",\"Good'\")],0), ((1,1),[((s\\.np)/.np,\">B\",\"is_better_than' bad'\")],2) ((0,2),[(np,\"Np/v->\",\"Good' (is_better_than' bad')\")],1)] [(s,\"Np/a-<\",\"(is_better_than' bad') Good'\")],1)  [((0,0),[(np/.np,\"Desig\",\"Good'\")],0), ((1,0),[((s\\.np)/.np,\"Desig\",\"is_better_than'\")],1), ((2,0),[(np/.np,\"Desig\",\"bad'\")],2), ((0,1),[(np,\"Np/v->\",\"Good' is_better_than'\")],1), ((0,1),[(s/.np,\"Np/a->T->B\",\"Good' is_better_than'\")],1), ((1,1),[((s\\.np)/.np,\">B\",\"is_better_than' bad'\")],2), ((1,1),[(s\\.np,\"Np/a->\",\"is_better_than' bad'\")],2), ((0,2),[(np,\"Np/v->\",\"Good' (is_better_than' bad')\")],1), ((0,2),[(s,\"Np/a-<\",\"(is_better_than' bad') Good'\")],1), ((0,2),[(s/.np,\"Np/a->T->B\",\"Good' (is_better_than' bad')\")],1), ((0,2),[(s/.np,\">B\",\"(Good' is_better_than') bad'\")],2), ((0,2),[(s,\"Np/a->\",\"(Good' is_better_than') bad'\")],2)] is [((1,1),[((s\\.np)/.np,\">B\",\"is_better_than' bad'\")],2), ((0,2),[(np,\"Np/v->\",\"Good' (is_better_than' bad')\")],1)]" $ do
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
      let pc01 = createPhraCate 0 0 c01 "Desig" "Good'" True 0
      let pc02 = createPhraCate 1 0 c02 "Desig" "is_better_than'" True 1
      let pc03 = createPhraCate 2 0 c03 "Desig" "bad'" True 2
      let pc11 = createPhraCate 0 1 c11 "Np/v->" "Good' is_better_than'" True 1
      let pc12 = createPhraCate 0 1 c12 "Np/a->T->B" "Good' is_better_than'" True 1
      let pc13 = createPhraCate 1 1 c13 ">B" "is_better_than' bad'" True 2
      let pc14 = createPhraCate 1 1 c14 "Np/a->" "is_better_than' bad'" True 2
      let pc21 = createPhraCate 0 2 c21 "Np/v->" "Good' (is_better_than' bad')" True 1
      let pc22 = createPhraCate 0 2 c22 "Np/a-<" "(is_better_than' bad') Good'" True 1
      let pc23 = createPhraCate 0 2 c23 "Np/a->T->B" "Good' (is_better_than' bad')" True 1
      let pc24 = createPhraCate 0 2 c24 ">B" "(Good' is_better_than') bad'" True 2
      let pc25 = createPhraCate 0 2 c25 "Np/a->" "(Good' is_better_than') bad'" True 2
      let pcClo = [pc01,pc02,pc03,pc11,pc12,pc13,pc14,pc21,pc22,pc23,pc24,pc25]
      findTipsOfTree "+++++" [pc01,pc13,pc21] pcClo `shouldBe` [pc13]

    it "The result of findCateBySpan 1 [((0,0),[(np/.np,\"Desig\",\"Brave'\")],0), ((1,0),[(np,\"Desig\",\"Frank'\")],1), ((2,0),[(s\\*np,\"Desig\"),\"wins'\"],2), ((0,1),[(np,\">\",\"Brave' Frank'\")],1), ((1,1),[(s,\"<\",\"wins' Frank'\")],2), ((0,2),[np,\"Np/s->\",\"Brave' (wins' Frank')\"],1), ((0,2),[(s,\"<\",\"wins' (Brave' Frank')\")],2)] is [((0,1),[(np,\">\",\"Brave' Frank'\")],1), ((1,1),[(s,\"<\",\"wins' Frank'\")],2)]" $ do
      let c01 = getCateFromString "np/.np"
      let c02 = npCate
      let c03 = getCateFromString "s\\*np"
      let c11 = npCate
      let c12 = sCate
      let c21 = npCate
      let c22 = sCate
      let pc01 = createPhraCate 0 0 c01 "Desig" "Brave'" True 0
      let pc02 = createPhraCate 1 0 c02 "Desig" "Frank'" True 1
      let pc03 = createPhraCate 2 0 c03 "Desig" "wins'" True 2
      let pc11 = createPhraCate 0 1 c11 ">" "Brave' Frank'" True 1
      let pc12 = createPhraCate 1 1 c12 "<" "wins' Frank'" True 2
      let pc21 = createPhraCate 0 2 c21 "Np/s->" "Brave' (wins' Frank')" True 1
      let pc22 = createPhraCate 0 2 c22 "<" "wins' (Brave' Frank')" True 2
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
      let pc01 = createPhraCate 0 0 c01 "Desig" "Brave'" True 0
      let pc02 = createPhraCate 1 0 c02 "Desig" "Frank'" True 1
      let pc03 = createPhraCate 2 0 c03 "Desig" "wins'" True 2
      let pc11 = createPhraCate 0 1 c11 ">" "Brave' Frank'" True 1
      let pc12 = createPhraCate 1 1 c12 "<" "wins' Frank'" True 2
      let pc21 = createPhraCate 0 2 c21 "Np/s->" "Brave' (wins' Frank')" True 1
      let pc22 = createPhraCate 0 2 c22 "<" "wins' (Brave' Frank')" True 2
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
      let pc01 = createPhraCate 0 0 c01 "Desig" "Brave'" True 0
      let pc02 = createPhraCate 1 0 c02 "Desig" "Frank'" True 1
      let pc03 = createPhraCate 2 0 c03 "Desig" "wins'" True 2
      let pc11 = createPhraCate 0 1 c11 ">" "Brave' Frank'" True 1
      let pc12 = createPhraCate 1 1 c12 "<" "wins' Frank'" True 2
      let pc21 = createPhraCate 0 2 c21 "Np/s->" "Brave' (wins' Frank')" True 1
      let pc22 = createPhraCate 0 2 c22 "<" "wins' (Brave' Frank')" True 2
      let pcs = [pc01,pc02,pc03]
      let pcClo = pcs ++ [pc11,pc12,pc21,pc22]
      findCateBySpan 2 pcClo `shouldBe` [pc21,pc22]

    it "The result of divPhraCateBySpan [((0,0),[(np/.np,\"Desig\",\"Brave'\",True)],0), ((1,0),[(np,\"Desig\",\"Frank'\",True)],1), ((2,0),[(s\\*np,\"Desig\"),\"wins'\",True)],2), ((0,1),[(np,\">\",\"Brave' Frank'\",True)],1), ((0,2),[(s,\"<\",\"wins' (Brave' Frank')\",True)],2)] is [[((0,0),[(np/.np, \"Desig\",\"Brave'\",True)],0), ((1,0),[(np,\"Desig\",\"Frank'\",True)],1), ((2,0),[(s\\*np, \"Desig\",\"wins'\",True)],2)], [((0,1),[(np, \">\",\"Brave' Frank'\",True)],1)], [((0,2),[(s,\"<\",\"wins' (Brave' Frank')\",True)],2)]]" $ do
      let c01 = getCateFromString "np/.np"
      let c02 = npCate
      let c03 = getCateFromString "s\\*np"
      let c11 = npCate
      let c12 = sCate
      let c21 = npCate
      let c22 = sCate
      let pc01 = createPhraCate 0 0 c01 "Desig" "Brave'" True 0
      let pc02 = createPhraCate 1 0 c02 "Desig" "Frank'" True 1
      let pc03 = createPhraCate 2 0 c03 "Desig" "wins'" True 2
      let pc11 = createPhraCate 0 1 c11 ">" "Brave' Frank'" True 1
      let pc12 = createPhraCate 1 1 c12 "<" "wins' Frank'" True 2
      let pc21 = createPhraCate 0 2 c21 "Np/s->" "Brave' (wins' Frank')" True 1
      let pc22 = createPhraCate 0 2 c22 "<" "wins' (Brave' Frank')" True 2
      let pcClo = [pc01,pc02,pc03,pc11,pc22]
      divPhraCateBySpan pcClo `shouldBe` [[pc01,pc02,pc03],[pc11],[pc22]]

    it "The result of growTree \"+++++\" [((0,2),[(s, \">\",\"(loves' Frank') Mary'\",True)],2)] [((0,0),[(np, \"Desig\",\"Frank'\",False)],0), ((1,0),[((s\\.np)/.np, \"Desig\",\"loves'\",False)],1), ((2,0),[(np, \"Desig\",\"Mary'\",False)],2), ((0,1),[(s/.np,\">T->B\",\"Frank' loves'\",False)],1), ((1,1),[(s\\.np, \"<\",\"loves' Frank'\",False)],2), ((0,2),[(s, \"<\",\"(loves' Mary') Frank'\",True)],1), ((0,2),[(s, \">\", \"(Frank' loves') Mary'\",True)],2) is [((0,0),[(np, \"Desig\",\"Frank'\",True)],0), ((1,0),[((s\\.np)/.np, \"Desig\",\"loves'\",True)],1), ((0,1),[(s/.np, \">T->B\",\"Frank' loves'\",True)],1), ((2,0),[(np, \"Desig\",\"Mary'\",True)],2), ((0,2),[(s, \">\",\"(Frank' loves') Mary'\",True)],2)]" $ do
      let c01 = npCate
      let c02 = getCateFromString "(s\\.np)/.np"
      let c03 = npCate
      let c11 = getCateFromString "s/.np"
      let c12 = getCateFromString "s\\.np"
      let c21 = sCate
      let c22 = sCate
      let pc01 = createPhraCate 0 0 c01 "Desig" "Frank'" True 0
      let pc02 = createPhraCate 1 0 c02 "Desig" "loves'" True 1
      let pc03 = createPhraCate 2 0 c03 "Desig" "Mary'" True 2
      let pc11 = createPhraCate 0 1 c11 ">T->B" "Frank' loves'" True 1 
      let pc12 = createPhraCate 1 1 c12 ">" "loves' Mary'" True 2
      let pc21 = createPhraCate 0 2 c21 "<" "(loves' Mary') Frank'" True 1
      let pc22 = createPhraCate 0 2 c22 ">" "(Frank' loves') Mary'" True 2 
      let pcClo = [pc01,pc02,pc03,pc11,pc12,pc21,pc22]
      growTree "+++++" [pc22] pcClo `shouldBe` [[pc01,pc02,pc11,pc03,pc22]]

    it "The result of growTree \"+++++\" [((0,2),[(s, \"<\", \"(loves' Mary') Frank'\",True)],1)]  [((0,0),[(np, \"Desig\",\"Frank'\",True)],0), ((1,0),[((s\\.np)/.np, \"Desig\",\"loves'\",True)],1), ((2,0),[(np, \"Desig\",\"Mary'\",True)],2), ((0,1),[(s/.np,\">T->B\",\"Frank' loves'\",True)],1), ((1,1),[(s\\.np, \"<\",\"loves' Frank'\",True)],2), ((0,2),[(s, \"<\",\"(loves' Mary') Frank'\",True)],1), ((0,2),[(s, \">\", \"(Frank' loves') Mary'\",True)],2) is [((1,0),[(s\\.np/.np, \"Desig\",\"loves'\",True)],1), ((2,0),[(np, \"Desig\",\"Mary'\",True)],2), ((0,0),[(np, \"Desig\",\"Frank'\",True)],0), ((1,1),[(s\\.np, \">\",\"loves' Mary'\",True)],2), ((0,2),[(s, \"<\", \"(loves' Mary') Frank'\",True)],1)]" $ do
      let c01 = npCate
      let c02 = getCateFromString "(s\\.np)/.np"
      let c03 = npCate
      let c11 = getCateFromString "s/.np"
      let c12 = getCateFromString "s\\.np"
      let c21 = sCate
      let c22 = sCate
      let pc01 = createPhraCate 0 0 c01 "Desig" "Frank'" True 0
      let pc02 = createPhraCate 1 0 c02 "Desig" "loves'" True 1
      let pc03 = createPhraCate 2 0 c03 "Desig" "Mary'" True 2
      let pc11 = createPhraCate 0 1 c11 ">T->B" "Frank' loves'" True 1
      let pc12 = createPhraCate 1 1 c12 ">" "loves' Mary'" True 2
      let pc21 = createPhraCate 0 2 c21 "<" "(loves' Mary') Frank'" True 1
      let pc22 = createPhraCate 0 2 c22 ">" "(Frank' loves') Mary'" True 2
      let pcClo = [pc01,pc02,pc03,pc11,pc12,pc21,pc22]
      growTree "+++++" [pc21] pcClo `shouldBe` [[pc02,pc03,pc01,pc12,pc21]]

    it "The result of growTree \"+++++\" ((0,2),[(s,\"Np/a-<\",\"(is_better_than' bad') Good'\",True)],1)  [((0,0),[(np/.np,\"Desig\",\"Good'\",False)],0), ((1,0),[((s\\.np)/.np,\"Desig\",\"is_better_than'\",False)],1), ((2,0),[(np/.np,\"Desig\",\"bad'\",False)],2), ((1,1),[(s\\.np,\"Np/a->\",\"is_better_than' bad'\",False)],2), ((0,2),[(s,\"Np/a-<\",\"(is_better_than' bad') Good'\",True)],1)] is [((1,0),[((s\\.np)/.np,\"Desig\",\"is_better_than'\",False)],1), ((2,0),[(np/.np,\"Desig\",\"bad'\",False)],2), ((0,0),[(np/.np,\"Desig\",\"Good'\",False)],0), ((1,1),[(s\\.np,\"Np/a->\",\"is_better_than' bad'\",False)],2), ((0,2),[(s,\"Np/a-<\",\"(is_better_than' bad') Good'\",True)],1)]" $ do
      let c01 = getCateFromString "np/.np"
      let c02 = getCateFromString "(s\\.np)/.np"
      let c03 = getCateFromString "np/.np"
      let c11 = getCateFromString "s\\.np"
      let c21 = sCate
      let pc01 = createPhraCate 0 0 c01 "Desig" "Good'" False 0
      let pc02 = createPhraCate 1 0 c02 "Desig" "is_better_than'" False 1
      let pc03 = createPhraCate 2 0 c03 "Desig" "bad'" False 2
      let pc11 = createPhraCate 1 1 c11 "Np/a->" "is_better_than' bad'" False 2
      let pc21 = createPhraCate 0 2 c21 "Np/a-<" "(is_better_than' bad') Good'" True 1
      let pcClo = [pc02,pc03,pc01,pc11,pc21]
      growTree "+++++" [pc21] pcClo `shouldBe` [pcClo]

    it "The result of growForest \"+++++\" [[((0,2),[(s, \"<\", \"(loves' Mary') Frank'\",True)],1)], [((0,2),[(s, \">\",\"(loves' Frank') Mary'\",True)],2)]]  [((0,0),[(np, \"Desig\",\"Frank'\",True)],0), ((1,0),[((s\\.np)/.np, \"Desig\",\"loves'\",True)],1), ((2,0),[(np, \"Desig\",\"Mary'\",True)],2), ((0,1),[(s/.np,\">T->B\",\"Frank' loves'\",True)],1), ((1,1),[(s\\.np, \"<\",\"loves' Frank'\",True)],2), ((0,2),[(s, \"<\",\"(loves' Mary') Frank'\",True)],1), ((0,2),[(s, \">\", \"(Frank' loves') Mary'\",True)],2) is [[((1,0),[(s\\.np/.np, \"Desig\",\"loves'\",True)],1), ((2,0),[(np, \"Desig\",\"Mary'\",True)],2), ((0,0),[(np, \"Desig\",\"Frank'\",True)],0), ((1,1),[(s\\.np, \">\",\"loves' Mary'\",True)],2), ((0,2),[(s, \"<\", \"(loves' Mary') Frank'\",True)],1)], [((0,0),[(np, \"Desig\",\"Frank'\",True)],0), ((1,0),[((s\\.np)/.np, \"Desig\",\"loves'\",True)],1), ((0,1),[(s/.np, \">T->B\",\"Frank' loves'\",True)],1), ((2,0),[(np, \"Desig\",\"Mary'\",True)],2), ((0,2),[(s, \">\",\"(Frank' loves') Mary'\",True)],2)]]" $ do
      let c01 = npCate
      let c02 = getCateFromString "(s\\.np)/.np"
      let c03 = npCate
      let c11 = getCateFromString "s/.np"
      let c12 = getCateFromString "s\\.np"
      let c21 = sCate
      let c22 = sCate
      let pc01 = createPhraCate 0 0 c01 "Desig" "Frank'" True 0
      let pc02 = createPhraCate 1 0 c02 "Desig" "loves'" True 1
      let pc03 = createPhraCate 2 0 c03 "Desig" "Mary'" True 2
      let pc11 = createPhraCate 0 1 c11 ">T->B" "Frank' loves'" True 1
      let pc12 = createPhraCate 1 1 c12 ">" "loves' Mary'" True 2
      let pc21 = createPhraCate 0 2 c21 "<" "(loves' Mary') Frank'" True 1
      let pc22 = createPhraCate 0 2 c22 ">" "(Frank' loves') Mary'" True 2
      let pcClo = [pc01,pc02,pc03,pc11,pc12,pc21,pc22]
      growForest "+++++" [[pc21],[pc22]] pcClo `shouldBe` [[pc02,pc03,pc01,pc12,pc21],[pc01,pc02,pc11,pc03,pc22]]

    it "The result of growForest \"+++++\" [[((0,2),[(np,\"Np/s->\",\"Brave' (wins' Frank')\",True],1)], [((0,2),[(s, \"<\", \"wins' (Brave' Frank')\",True)],2)] [((0,0),[(np/.np,\"Desig\",\"Brave'\",True)],0), ((1,0),[(np,\"Desig\",\"Frank'\",True)],1), ((2,0),[(s\\*np,\"Desig\"),\"wins'\",True],2), ((0,1),[(np,\">\",\"Brave' Frank'\",True)],1), ((1,1),[(s,\"<\",\"wins' Frank'\",True)],2), ((0,2),[np,\"Np/s->\",\"Brave' (wins' Frank')\",True)],1), ((0,2),[(s,\"<\",\"wins' (Brave' Frank')\",True)],2)] is [[((1,0),[(np, \"Desig\",\"Frank'\",True)],1), ((2,0),[(s\\*np, \"Desig\",\"wins'\",True)],2), ((0,0),[(np/.np, \"Desig\",\"Brave'\",True)],0), ((1,1),[(s, \"<\",\"wins' Frank'\",True)],2), ((0,2),[(np,\"Np/s->\",\"Brave' (wins' Frank')\",True)],1)], [((0,0),[(np/.np, \"Desig\",\"Brave'\",True)],0), ((1,0),[(np, \"Desig\",\"Frank'\",True)],1), ((0,1),[(np, \">\",\"Brave' Frank'\",True)],1), ((2,0),[(s\\*np, \"Desig\",\"wins'\",True)],2), ((0,2),[(s, \"<\",\"wins' (Brave' Frank')\",True)],2)" $ do
      let c01 = getCateFromString "np/.np"
      let c02 = npCate
      let c03 = getCateFromString "s\\*np"
      let c11 = npCate
      let c12 = sCate
      let c21 = npCate
      let c22 = sCate
      let pc01 = createPhraCate 0 0 c01 "Desig" "Brave'" True 0
      let pc02 = createPhraCate 1 0 c02 "Desig" "Frank'" True 1
      let pc03 = createPhraCate 2 0 c03 "Desig" "wins'" True 2
      let pc11 = createPhraCate 0 1 c11 ">" "Brave' Frank'" True 1
      let pc12 = createPhraCate 1 1 c12 "<" "wins' Frank'" True 2
      let pc21 = createPhraCate 0 2 c21 "Np/s->" "Brave' (wins' Frank')" True 1
      let pc22 = createPhraCate 0 2 c22 "<" "wins' (Brave' Frank')" True 2
      let pcClo = [pc01,pc02,pc03,pc11,pc12,pc21,pc22]
      growForest "+++++" [[pc21],[pc22]] pcClo `shouldBe` [[pc02,pc03,pc01,pc12,pc21],[pc01,pc02,pc11,pc03,pc22]]


