-- Copyright China University of Water Resources and Electric Power (c) 2019
-- All rights reserved.

module ParseSpec where

import Category
import Rule
import Parse
import Test.Hspec
import Utils

spec :: Spec
spec = do
  describe "Parse" $ do
    it "The result of initPhraCate [] is []" $ do
      initPhraCate [] `shouldBe` ([] :: [PhraCate])

    it "The result of initPhraCate [s] is [((0,0),[(s,\"Desig\",\"Frank'\",\"DE\",True)],0)]" $ do
      initPhraCate [(sCate, "Frank'")] `shouldBe` ([((0,0),[(sCate,"Desig","Frank'","DE",True)],0)] :: [PhraCate])

    it "The result of initPhraCate [np, (s\\.np)/.np, np] is [((0,0),[(np,\"Desig\",\"Frank'\",\"DE\",True)],0),((1,0),[((s\\.np)/.np,\"Desig\",\"loves'\",\"DE\",True)],1),((2,0),[(np,\"Desig\",\"Mary'\",\"DE\",True)],2)]" $ do
      let c1 = npCate
      let c2 = getCateFromString "(s\\.np)/.np"
      let c3 = npCate
      initPhraCate [(c1,"Frank'"),(c2,"loves'"),(c3,"Mary'")] `shouldBe` ([((0,0),[(c1,"Desig","Frank'","DE",True)],0),((1,0),[(c2,"Desig","loves'","DE",True)],1),((2,0),[(c3,"Desig","Mary'","DE",True)],2)] :: [PhraCate])

    it "The result of createPhraCate 0 1 s \"Np/s\" \"smiles' Frank'\" \"DE\" True 1 is ((0,1),[(s, \"Np/s\", \"smiles' Frank'\",\"DE\",True)],1)" $ do
      createPhraCate 0 1 sCate "Np/s" "smiles' Frank'" "DE" True 1 `shouldBe` (((0,1),[(sCate,"Np/s","smiles' Frank'","DE",True)],1)::PhraCate)

    it "The result of applying func pclt to ((0,0),[(np,\"Desig\",\"Frank'\",\"DE\",True)],0) and ((1,0),[((s\\.np)/.np,\"Desig\",\"loves'\",\"DE\",True)],1) is True" $ do
      let c1 = npCate
      let c2 = getCateFromString "(s\\.np)/.np"
      let pcs = initPhraCate [(c1,"Frank'"), (c2,"smile'")]
      let pc1 = head pcs
      let pc2 = last pcs
      pclt pc1 pc2 `shouldBe` (True :: Bool)

    it "The result of applying func pclt to ((0,0),[(np,\"Desig\",\"Frank'\",\"DE\",True)],0) and ((0,0),[(s/.np,\"Desig\",\"loves' Mary'\",\"DE\",True)],0) is False" $ do
      let c1 = npCate
      let c2 = getCateFromString "s/.np"
      let pcs1 = initPhraCate [(c1,"Frank'")]
      let pc1 = head pcs1
      let pcs2 = initPhraCate [(c2,"loves' Mary'")]
      let pc2 = head pcs2
      pclt pc1 pc2 `shouldBe` (False :: Bool)
    
    it "The result of ctscaOfCate ((1,0),[((s\\.np)/.np, \"Desig\", \"likes'\",\"DE\",True),(s/.np, \"Desig\", \"likes'\",\"DE\",True)],1) is [\"(s\\.np)/.np\", \"Desig\", \"likes'\", True),(\"s/.np\", \"Desig\", \"likes'\", True)]" $ do
      let pc = createPhraCate2 1 0 [(getCateFromString "(s\\.np)/.np", "Desig", "likes'", "DE", True),(getCateFromString "s/.np", "Desig", "likes'", "DE", True)] 1
      let ctsca = [(getCateFromString "(s\\.np)/.np", "Desig", "likes'", "DE", True),(getCateFromString "s/.np", "Desig", "likes'", "DE", True)]
      ctscaOfCate pc `shouldBe` ctsca

    it "The result of ctscOfCate ((1,0),[((s\\.np)/.np, \"Desig\", \"likes'\",\"DE\",True),(s/.np, \"Desig\", \"likes'\",\"DE\",True)],1) is [(\"(s\\.np)/.np\", \"Desig\", \"likes'\"),(\"s/.np\", \"Desig\", \"likes'\")]" $ do
      let pc = createPhraCate2 1 0 [(getCateFromString "(s\\.np)/.np", "Desig", "likes'", "DE", True),(getCateFromString "s/.np", "Desig", "likes'", "DE", True)] 1
      let ctsc = [(getCateFromString "(s\\.np)/.np", "Desig", "likes'","DE"),(getCateFromString "s/.np", "Desig", "likes'","DE")]
      ctscOfCate pc `shouldBe` ctsc  

    it "The result of csOfActCate ((1,0),[((s\\.np)/.np, \"Desig\", \"likes'\",\"DE\",True),(s/.np, \"Desig\", \"likes'\",\"DE\",False],1) is [(\"(s\\.np)/.np\", \"likes'\")]" $ do
      let pc = createPhraCate2 1 0 [(getCateFromString "(s\\.np)/.np", "Desig", "likes'", "DE", True),(getCateFromString "s/.np", "Desig", "likes'", "DE",False)] 1
      let cs = [(getCateFromString "(s\\.np)/.np", "likes'")]
      csOfActCate pc `shouldBe` cs  
    
    it "The result of caOfActCate ((1,0),[((s\\.np)/.np, \"Desig\", \"likes'\",\"DE\",True),(s/.np, \"Desig\", \"likes'\",\"DE\",False],1) is [\"(s\\.np)/.np\"]" $ do
      let pc = createPhraCate2 1 0 [(getCateFromString "(s\\.np)/.np", "Desig", "likes'","DE",True),(getCateFromString "s/.np", "Desig", "likes'", "DE",False)] 1
      let ca = [getCateFromString "(s\\.np)/.np"]
      caOfActCate pc `shouldBe` ca 
    
    it "The result of csOfActCate ((1,0),[((s\\.np)/.np, \"Desig\", \"likes'\",\"DE\",False),(s/.np, \"Desig\", \"likes'\",\"DE\",True)],1) is [(\"s/.np\", \"likes'\")]" $ do
      let pc = createPhraCate2 1 0 [(getCateFromString "(s\\.np)/.np", "Desig", "likes'", "DE",False),(getCateFromString "s/.np", "Desig", "likes'", "DE", True)] 1
      let cs = [(getCateFromString "s/.np", "likes'")]
      csOfActCate pc `shouldBe` cs  

    it "The result of pcBelong ((1,0),[(s\\.np)/.np, \"Desig\", \"likes'\",\"DE\",True)],1) ((1,0),[(s\\.np)/.np, \"Desig\", \"likes'\",\"DE\",True)],1) is True" $ do
      let pc1 = createPhraCate 1 0 (getCateFromString "(s\\.np)/.np") "Desig" "likes'" "DE" True 1
      let pc2 = createPhraCate 1 0 (getCateFromString "(s\\.np)/.np") "Desig" "likes'" "DE" True 1
      pcBelong pc1 pc2 `shouldBe` True

    it "The result of pcBelong ((1,0),[(s\\.np)/.np, \"Desig\", \"likes'\",\"DE\",True)],1) ((1,0),[(s\\.np)/.np, \"Desig\", \"likes'\",\"DE\",True),(np, \">\",\"book'\",\"DE\",True)],1) is True" $ do
      let cate = getCateFromString "(s\\.np)/.np"
      let pc1 = createPhraCate 1 0 cate "Desig" "likes'" "DE" True 1
      let pc2 = createPhraCate2 1 0 [(cate,"Desig","likes'","DE",True),(npCate,">","book'","DE",True)] 1
      pcBelong pc1 pc2 `shouldBe` True

    it "The result of pcBelong ((1,0),[(s\\.np)/.np, \"Desig\", \"likes'\",\"DE\",True),(np, \">\",\"book'\",\"DE\",True)],1) ((1,0),[(s\\.np)/.np, \"Desig\", \"likes'\",\"DE\",True)],1) is False" $ do
      let cate = getCateFromString "(s\\.np)/.np"
      let pc1 = createPhraCate 1 0 cate "Desig" "likes'" "DE" True 1
      let pc2 = createPhraCate2 1 0 [(cate,"Desig","likes'","DE",True),(npCate,">","book'","DE",True)] 1
      pcBelong pc2 pc1 `shouldBe` False

    it "The result of pcBelong' ((1,0),[(s\\.np)/.np, \"Desig\", \"likes'\",\"DE\",False],1) ((1,0),[(s\\.np)/.np, \"Desig\", \"likes'\",\"DE\",True),(np, \">\",\"book'\",\"DE\",True)],1) is False" $ do
      let cate = getCateFromString "(s\\.np)/.np"
      let pc1 = createPhraCate 1 0 cate "Desig" "likes'" "DE" False 1
      let pc2 = createPhraCate2 1 0 [(cate,"Desig","likes'","DE",True),(npCate,">","book'","DE",True)] 1
      pcBelong' pc1 pc2 `shouldBe` True

    it "The result of stOfCate ((1,0),[(s\\.np, \"Desig\", \"smiles'\",\"DE\",True)],1) is 1" $ do
      let c = getCateFromString "s\\.np"
      let pc = createPhraCate 1 0 c "Desig" "smiles'" "DE" True 1
      stOfCate pc `shouldBe` 1

    it "The result of spOfCate ((1,0),[(s\\.np, \"Desig\", \"smiles'\",\"DE\",True)],1) is 0" $ do
      let c = getCateFromString "s\\.np"
      let pc = createPhraCate 1 0 c "Desig" "smiles'" "DE" True 1
      spOfCate pc `shouldBe` 0

    it "The result of ssOfCate ((1,0),[(s\\.np, \"Desig\", \"smiles'\",\"DE\",True)],1) is 1" $ do
      let c = getCateFromString "s\\.np"
      let pc = createPhraCate 1 0 c "Desig" "smiles'" "DE" True 1
      ssOfCate pc `shouldBe` 1

    it "The result of ctscaOfCate ((1,0),[(s\\.np, \"Desig\", \"smiles'\",\"DE\",True)],1) is [(s\\.np, \"Desig\", \"smiles'\",\"DE\",True)]" $ do
      let c = getCateFromString "s\\.np"
      let pc = createPhraCate 1 0 c "Desig" "smiles'" "DE" True 1
      ctscaOfCate pc `shouldBe` [(c,"Desig","smiles'","DE",True)]

    it "The result of caOfCate ((1,0),[(s\\.np, \"Desig\", \"smiles'\",\"DE\",True)],1) is [s\\.np]" $ do
      let c = getCateFromString "s\\.np"
      let pc = createPhraCate 1 0 c "Desig" "smiles'" "DE" True 1
      caOfCate pc `shouldBe` [c]
   
    it "The result of cateComb_CS \"+++++\" ((0,0),[(np, \"Desig\", \"Frank'\",\"DE\",True)],0) ((1,0),[(s\\.np, \"Desig\", \"smiles\",\"DE\",True)],1) is ((0,1),[(s, \"<\", \"smiles' Frank'\",\"DE\",True)],1)" $ do
      let c1 = npCate
      let c2 = getCateFromString "s\\.np"
      let c3 = sCate
      let pc1 = createPhraCate 0 0 c1 "Desig" "Frank'" "DE" True 0
      let pc2 = createPhraCate 1 0 c2 "Desig" "smiles'" "DE" True 1
      let pc3 = createPhraCate2 0 1 [(c3,"<","smiles' Frank'","SP",True)] 1
      cateComb_CS "+++++" pc1 pc2 `shouldBe` pc3

    it "The result of cateComb_CS \"+++++\" ((0,0),[(np, \"Desig\", \"Frank'\",\"DE\",False), ((1,0),[(s\\.np,\"Desig\",\"smiles'\",\"DE\",True)],1) is ((0,1),[],1)" $ do
      let c1 = npCate
      let c2 = getCateFromString "s\\.np"
      let c3 = sCate
      let pc1 = createPhraCate 0 0 c1 "Desig" "Frank'" "DE" False 0
      let pc2 = createPhraCate 1 0 c2 "Desig" "smiles'" "DE" True 1
      let pc3 = createPhraCate 0 1 c3 "<" "smiles' Frank'" "SP" True 1
      cateComb_CS "+++++" pc1 pc2 `shouldBe` pc3

    it "The result of cateComb_CS ((0,0),[(np,\"Desig\",\"Frank'\",\"DE\",True)],0) ((1,0),[((s\\.np)/.np,\"Desig\",\"loves'\",\"DE\",True)],1) is ((0,1),[(s/.np,\">T->B\"),\"Frank' loves'\",\"DE\",True)],1)" $ do
      let c1 = npCate
      let c2 = getCateFromString "(s\\.np)/.np"
      let c3 = getCateFromString "s/.np"
      let pc1 = createPhraCate 0 0 c1 "Desig" "Frank'" "DE" True 0
      let pc2 = createPhraCate 1 0 c2 "Desig" "loves'" "DE" True 1
      let pc3 = createPhraCate 0 1 c3 ">T->B" "Frank' loves'" "OE" True 1
      cateComb_CS "+++++" pc1 pc2 `shouldBe` pc3

    it "The result of cateComb_CS ((0,0),[(s,\"Desig\",\"Frank_cries'\",\"DE\",True)],0) ((1,0),[(s\\.np,\"Desig\",\"is_bad'\",\"DE\",True)],1) is ((0,1),[(s,\"Np/s-<\",\"is_bad' Frank_cries'\",\"DE\",True)],1)" $ do
      let c1 = sCate
      let c2 = getCateFromString "s\\.np"
      let c3 = sCate
      let pc1 = createPhraCate 0 0 c1 "Desig" "Frank_cries'" "DE" True 0
      let pc2 = createPhraCate 1 0 c2 "Desig" "is_bad'" "DE" True 1
      let pc3 = createPhraCate 0 1 c3 "Np/s-<" "is_bad' Frank_cries'" "SP" True 1
      cateComb_CS "+++++" pc1 pc2 `shouldBe` pc3
    
    it "The result of cateComb_CS ((0,0),[((s\\.np)/.np,\"Desig\",\"loves'\",\"DE\",True)],0) ((1,0),[(s,\"Desig\",\"Frank_does_it'\",\"DE\",True)],1) is ((0,1),[(s\\.np,\"Np/s->\",\"loves' Frank_does_it'\",\"DE\",True)],1)" $ do
      let c1 = getCateFromString "(s\\.np)/.np"
      let c2 = sCate
      let c3 = getCateFromString "s\\.np"
      let pc1 = createPhraCate 0 0 c1 "Desig" "loves'" "DE" True 0
      let pc2 = createPhraCate 1 0 c2 "Desig" "Frank_does_it'" "DE" True 1
      let pc3 = createPhraCate 0 1 c3 "Np/s->" "loves' Frank_does_it'" "VO" True 1
      cateComb_CS "+++++" pc1 pc2 `shouldBe` pc3
    
    it "The result of atomizedPhraCate [cateComb_CS ((0,0),[((s\\.np)/.np,\"Desig\",\"loving'\",\"DE\",True)],0) ((1,0),[(s\\.np,\"Desig\",\"is_forever'\",\"DE\",True)],1)] is [((0,1),[(s,\"Np/v-<\",\"is_forever' loving'\",\"DE\",True),(s\\.np,\"Np/v->\",\"loving' is_forever'\",\"DE\",True)],1)]" $ do
      let c1 = getCateFromString "(s\\.np)/.np"
      let c2 = getCateFromString "s\\.np"
      let c3 = sCate
      let c4 = getCateFromString "s\\.np"
      let pc1 = createPhraCate 0 0 c1 "Desig" "loving'" "DE" True 0
      let pc2 = createPhraCate 1 0 c2 "Desig" "is_forever'" "DE" True 1
      let pc3 = createPhraCate2 0 1 [(c3,"Np/v-<","is_forever' loving'","SP",True)] 1
      let pc4 = createPhraCate2 0 1 [(c4,"Np/v->","loving' is_forever'","VO",True)] 1
      atomizePhraCate [cateComb_CS "+++++" pc1 pc2] `shouldBe` [pc3,pc4]

    it "The result of cateComb_CS ((0,0),[(np/.np,\"Desig\",\"Good'\",\"DE\",True)],0) ((1,1),[((s\\.np)/.np,\">B\",\"is_better_than' bad'\",\"DE\",True), (s\\.np,\"Np/a->\",\"is_better_than' bad'\",\"DE\",True)],2) is ((0,2),[(s/.np,\"<Bx\",\"(is_better_than' bad') Good'\",\"DE\",True),(s,\"Np/a-<\",\"(is_better_than' bad') Good'\",\"DE\",True),(s/.np,\"Np/a->T->B\",\"Good' (is_better_than' bad')\",\"DE\",True)],1)" $ do
      let c1 = getCateFromString "np/.np"
      let c21 = getCateFromString "(s\\.np)/.np"
      let c22 = getCateFromString "s\\.np"
      let c31 = getCateFromString "s/.np"
      let c34 = c31
      let pc1 = createPhraCate 0 0 c1 "Desig" "Good'" "DE" True 0
      let pc2 = createPhraCate2 1 1 [(c21,">B","is_better_than' bad'","DE",True),(c22,"Np/a->","is_better_than' bad'","DE",True)] 2
      let pc3 = createPhraCate2 0 2 [(c31,"<Bx","(is_better_than' bad') Good'","NR",True),(sCate,"Np/a-<","(is_better_than' bad') Good'","SP",True),(c34,"Np/a->T->B","Good' (is_better_than' bad')","OE",True)] 1
      cateComb_CS "+++++" pc1 pc2 `shouldBe` pc3
    
    it "The result of [cateComb_CS \"+++++\" pc1 pc2 | pc1 <- trans, pc2 <- trans, pc1 /= pc2, (acOfCate pc1)!!0, (acOfCate pc2)!!0, pclt pc1 pc2] is [((0,1),[(s/.np,\">T->B\",\"loves' Mary'\",\"DE\",True)],1)，((-1,-1),[],-1), ((1,1),[(s\\.np,\">\",\"loves' Mary'\",\"DE\",True)],2)]" $ do
      let c01 = getCateFromString "np"
      let c02 = getCateFromString "(s\\.np)/.np"
      let c03 = getCateFromString "np"
      let c12 = getCateFromString "s\\.np"
      let c11 = getCateFromString "s/.np"
      let pc01 = createPhraCate 0 0 c01 "Desig" "Frank'" "DE" True 0
      let pc02 = createPhraCate 1 0 c02 "Desig" "loves'" "DE" True 1
      let pc03 = createPhraCate 2 0 c03 "Desig" "Mary'" "DE" True 2
      let trans = [pc01, pc02, pc03]
      let pc12 = createPhraCate 1 1 c12 ">" "loves' Mary'" "VO" True 2
      let pc11 = createPhraCate 0 1 c11 ">T->B" "Frank' loves'" "OE" True 1 
      let null = ((-1,-1),[],-1)
      [cateComb_CS "+++++" pc1 pc2 | pc1 <- trans, pc2 <- trans, pc1 /= pc2, (acOfCate pc1)!!0, (acOfCate pc2)!!0, pclt pc1 pc2] `shouldBe` [pc11,null,pc12]

    it "The result of getNuOfInputCates ((0,0),[(np,\"Desig\",\"Frank'\")],0) ((1,0),[((s\\.np)/.np,\"Desig\",\"loves'\")],1) ((0,1),[(s/.np,\"<\",\"loves' Frank'\")],1) is 2" $ do
      let c1 = npCate
      let c2 = getCateFromString "(s\\.np)/.np"
      let c3 = getCateFromString "s/.np"
      let pc1 = createPhraCate 0 0 c1 "Desig" "Frank'" "DE" True 0
      let pc2 = createPhraCate 1 0 c2 "Desig" "loves'" "DE" True 1
      let pc3 = createPhraCate 0 1 c3 "<" "loves' Frank'" "DE" True 1
      let pcs = [pc1,pc2,pc3]
      getNuOfInputCates pcs `shouldBe` 2

    it "The result of isOverlap ((0,1),[(np,\"Desig\",\"Frank'\",\"DE\",True)],0), ((1,1),[((s\\.np)/.np,\"Desig\",\"loves'\",\"DE\",True)],2) is True" $ do
      let c1 = npCate
      let c2 = getCateFromString "(s\\.np)/.np"
      let pc1 = createPhraCate 0 1 c1 "Desig" "Frank'" "DE" True 1
      let pc2 = createPhraCate 1 1 c2 "Desig" "loves'" "DE" True 2
      isOverlap pc1 pc2 `shouldBe` True

    it "The result of getOverlap [((0,1),[(np,\"Desig\",\"Frank'\",\"DE\",True)],1), ((1,1),[((s\\.np)/.np,\"Desig\",\"loves'\",\"DE\",True)],2)] is [(((0,1),[(np,\"Desig\",\"Frank'\",\"DE\",True)],1), ((1,1),[(s\\.np)/.np,\"Desig\",\"loves'\",\"DE\",True)],2))]" $ do
      let c1 = npCate
      let c2 = getCateFromString "(s\\.np)/.np"
      let pc1 = createPhraCate 0 1 c1 "Desig" "Frank'" "DE" True 1
      let pc2 = createPhraCate 1 1 c2 "Desig" "loves'" "DE" True 2
      let pcs = [pc1,pc2]
      let res = [(pc1,pc2)]
      getOverlap pcs `shouldBe` res

    it "The result of deactOnePC ((0,0),[(np,\"Desig\",\"Frank'\",\"DE\",True)],0) is ((0,0),[(np,\"Desig\",\"Frank'\",\"DE\",False],0)" $ do
      let c1 = npCate
      let pc1 = createPhraCate 0 0 c1 "Desig" "Frank'" "DE" True 0
      let pc2 = createPhraCate 0 0 c1 "Desig" "Frank'" "DE" False 0
      deactOnePC pc1 `shouldBe` pc2

    it "The result of actOnePC ((0,0),[(np,\"Desig\",\"Frank'\",\"DE\",False],0) is ((0,0),[(np,\"Desig\",\"Frank'\",\"DE\",True)],0)" $ do
      let c1 = npCate
      let pc1 = createPhraCate 0 0 c1 "Desig" "Frank'" "DE" False 0
      let pc2 = createPhraCate 0 0 c1 "Desig" "Frank'" "DE" True 0
      actOnePC pc1 `shouldBe` pc2

    it "The result of findSplitCate ((0,1),[(s\\.np,\">T->B\",\"Frank' loves'\",\"DE\",True)],1) [((0,0),[(np,\"Desig\",\"Frank'\")],0), ((1,0),[((s\\.np)/.np,\"Desig\",\"loves'\")],1), ((2,0),[(np,\"Desig\",\"Mary', True)],2), ((0,1),[(s/.np,\">T->B\",\"Frank' loves'\")],1), ((1,1),[(s\\.np,\">\",\"loves' Mary'\")],2) is ((0,0),[(np,\"Desig\",\"Frank'\",\"DE\",True],1), ((1,0),[((s\\.np)/.np,\"Desig\",\"loves'\")],1)]" $ do
      let c01 = getCateFromString "np"
      let c02 = getCateFromString "(s\\.np)/.np"
      let c03 = getCateFromString "np"
      let c11 = getCateFromString "s/.np"
      let c12 = getCateFromString "s\\.np"
      let pc01 = createPhraCate 0 0 c01 "Desig" "Frank'" "DE" True 0
      let pc02 = createPhraCate 1 0 c02 "Desig" "loves'" "DE" True 1
      let pc03 = createPhraCate 2 0 c03 "Desig" "Mary'" "DE" True 2
      let pc11 = createPhraCate 0 1 c11 ">T->B" "Frank' loves'" "OE" True 1
      let pc12 = createPhraCate 1 1 c12 ">" "loves' Mary'" "VO" True 2
      let pcs = [pc01,pc02,pc03]
      let pcClo = pcs ++ [pc11,pc12]
      findSplitCate "+++++" pc11 pcClo `shouldBe` [(pc01,pc02)]

    it "The result of changeAct \"+++++\" [((0,0),[(np,\"Desig\",\"Frank'\",\"DE\",True)],0), ((1,0),[((s\\.np)/.np,\"Desig\",\"loves'\",\"DE\",True)],1)] is [((0,0),[(np,\"Desig\",\"Frank'\",\"DE\",True)],0), ((1,0),[((s\\.np)/.np,\"Desig\",\"loves'\",\"DE\",True)],1)]" $ do
      let c01 = npCate
      let c02 = getCateFromString "(s\\.np)/.np"
      let pc01 = createPhraCate 0 0 c01 "Desig" "Frank'" "DE" True 0
      let pc02 = createPhraCate 1 0 c02 "Desig" "loves'" "DE" True 1
      let pcs = [pc01,pc02]
      changeAct "+++++" pcs `shouldBe` pcs

    it "The result of changeAct \"+++++\" [((0,0),[(np,\"Desig\",\"Frank'\",\"DE\",True)],0), ((1,0),[((s\\.np)/.np,\"Desig\",\"loves'\",\"DE\",True)],1), ((2,0),[(np,\"Desig\",\"Mary'\",\"DE\",True)],2), ((1,1),[(s\\.np,\">\",\"loves' Mary'\",\"DE\",True)],2)] is [((0,0),[(np,\"Desig\",\"Frank'\",\"DE\",True)],0), ((1,0),[((s\\.np)/.np,\"Desig\",\"loves'\",\"DE\",False],1), ((2,0),[(np,\"Desig\",\"Mary'\",\"DE\",False],2), ((1,1),[(s\\.np,\">\",\"loves' Mary'\",\"DE\",True)],2)]" $ do
      let c01 = npCate
      let c02 = getCateFromString "(s\\.np)/.np"
      let c03 = npCate
      let c11 = getCateFromString "s/.np"
      let c12 = getCateFromString "s\\.np"
      let pc01 = createPhraCate 0 0 c01 "Desig" "Frank'" "DE" True 0
      let pc02 = createPhraCate 1 0 c02 "Desig" "loves'" "DE" True 1
      let pc03 = createPhraCate 2 0 c03 "Desig" "Mary'" "DE" True 2
      let pc11 = createPhraCate 0 1 c11 ">T->B" "Frank' loves'" "OE" True 1
      let pc12 = createPhraCate 1 1 c12 ">" "loves' Mary'" "VO" True 2
      let pc010 = deactOnePC pc01
      let pc020 = deactOnePC pc02
      let pc030 = deactOnePC pc03
      let pcs = [pc01,pc02,pc03]
      let pcClo = pcs ++ [pc11,pc12]
      let pcClo0 = [pc010,pc020,pc030,pc11,pc12]
      changeAct "+++++" pcClo `shouldBe` pcClo0

    it "The result of getOverlap [((0,0),[(np,\"Desig\",\"Frank'\",\"DE\",True)],0), ((1,0),[(s\\.np)/.np,\"Desig\",\"loves'\",\"DE\",True)],1), ((0,1),[(s/.np,\">T->B\",\"Frank' loves'\",\"DE\",True)],1), ((1,1),[(s\\.np,\"<\",\"loves' Mary'\",\"DE\",True)],2)] is [(((0,1),[(s/.np,\">T->B\",\"Frank' loves'\",\"DE\",True)],1), ((1,1),[(s\\.np,\">\",\"loves' Mary'\",\"DE\",True)],2)]" $ do
      let c1 = npCate
      let c2 = getCateFromString "(s\\.np)/.np"
      let c3 = getCateFromString "s/.np"
      let c4 = getCateFromString "s\\.np"
      let pc1 = createPhraCate 0 0 c1 "Desig" "Frank'" "DE" True 0
      let pc2 = createPhraCate 1 0 c2 "Desig" "loves'" "DE" True 1
      let pc3 = createPhraCate 0 1 c3 ">T->B" "Frank' loves'" "OE" True 1
      let pc4 = createPhraCate 1 1 c4 ">" "loves' Mary'" "VO" True 2
      let pcs = [pc1,pc2]
      let pcClo = pcs ++ [pc3,pc4]
      getOverlap pcClo `shouldBe` [(pc3,pc4)]

    it "The result of prune \"+++++\" [((0,0),[(np,\"Desig\",\"Frank'\",\"DE\",True)],0), ((1,0),[(s\\.np)/.np,\"Desig\",\"loves'\",\"DE\",True)],1), ((2,0),[(s\\.np,\">\",\"Mary'\",\"DE\",True)],2), ((0,1),[(s/.np,\">T->B\",\"Frank' loves'\",\"DE\",True)],1), ((1,1),[(s\\.np,\"<\",\"loves' Mary'\",\"DE\",True)],2)] is [((0,0),[(np,\"Desig\",\"Frank'\",\"DE\",True)],0), ((1,0),[(s\\.np)/.np,\"Desig\",\"loves'\",\"DE\",True)],1), ((0,1),[(s/.np,\">T->B\",\"Frank' loves'\",\"DE\",True)],1)]" $ do
      let c01 = npCate
      let c02 = getCateFromString "(s\\.np)/.np"
      let c03 = npCate
      let c11 = getCateFromString "s/.np"
      let c12 = getCateFromString "s\\.np"
      let pc01 = createPhraCate 0 0 c01 "Desig" "Frank'" "DE" False 0
      let pc02 = createPhraCate 1 0 c02 "Desig" "loves'" "DE" False 1
      let pc03 = createPhraCate 2 0 c03 "Desig" "Mary'" "DE" False 2
      let pc11 = createPhraCate 0 1 c11 ">T->B" "Frank' loves'" "OE" True 1
      let pc12 = createPhraCate 1 1 c12 ">" "loves' Mary'" "VO" True 2
      let pc010 = actOnePC pc01
      let pcs = [pc01,pc02,pc03]
      let pcClo = pcs ++ [pc11,pc12]
      let pcClo' = [pc02,pc03,pc010,pc12]
      prune "+++++" pcClo `shouldBe` pcClo'

    it "The result of findCombWithLowestPrio [((0,0),[(np,\"Desig\",\"China'\",\"DE\",True)],0), ((1,0),[np,\"Desig\",\"Henan'\",\"DE\",True)],1), ((2,0),[(np,\"Desig\",\"Zhengzhou'\",\"DE\",True)],2),((3,0),[(np,\"Desig\",\"JinShui_District'\",\"DE\",True)],3),((0,1),[(np,\"A/n->\",\"China' Henan'\",\"AHn\",True)],1),((1,1),[(np,\"A/n->\",\"Henan' Zhengzhou'\",\"AHn\",True)],2),((2,1),[(np,\"A/n->\",\"Zhengzhou' JinShui_District'\",\"AHn\",True)],3)] [(((0,1),[(np,\"A/n->\",\"China' Henan'\",\"AHn\",True)],1),((1,1),[(np,\"A/n->\",\"Henan' Zhengzhou'\",\"AHn\",True)],2)),(((1,1),[(np,\"A/n->\",\"Henan' Zhengzhou'\",\"AHn\",True)],2),((2,1),[(np,\"A/n->\",\"Zhengzhou' JinShui_District'\",\"AHn\",True)],3))] is ((1,1),[(np,\"A/n->\",\"Henan' Zhengzhou'\",\"AHn\",True)],2)" $ do
      let pc01 = createPhraCate 0 0 npCate "Desig" "China'" "DE" True 0
      let pc02 = createPhraCate 1 0 npCate "Desig" "Henan'" "DE" True 1
      let pc03 = createPhraCate 2 0 npCate "Desig" "Zhengzhou'" "DE" True 2
      let pc04 = createPhraCate 3 0 npCate "Desig" "JinShui_District'" "DE" True 3
      let pc11 = createPhraCate 0 1 npCate "A/n->" "China' Henan'" "AHn" True 1
      let pc12 = createPhraCate 1 1 npCate "A/n->" "Henan' Zhengzhou'" "AHn" True 2
      let pc13 = createPhraCate 2 1 npCate "A/n->" "Zhengzhou' JinShui_District'" "AHn" True 3
      let pcs = [pc01,pc02,pc03,pc04,pc11,pc12,pc13]
      let pcps = [(pc11,pc12),(pc12,pc13)]
      findCombWithLowestPrio pcs pcps `shouldBe` pc11

    it "The result of parse \"+++++\" [((0,0),[(np,\"Desig\",\"Frank'\",\"DE\",True)],0), ((1,0),[((s\\.np)/.np,\"Desig\",\"loves'\",\"DE\",True)],1)] is [((0,0),[(np,\"Desig\",\"Frank'\",\"DE\",False],0), ((1,0),[(s\\.np)/.np,\"Desig\",\"loves'\",\"DE\",False],1), ((0,1),[(s/.np,\">T->B\",\"Frank' loves'\",\"DE\",True)],1)]" $ do
      let c01 = npCate
      let c02 = getCateFromString "(s\\.np)/.np"
      let c11 = getCateFromString "s/.np"
      let pc01 = createPhraCate 0 0 c01 "Desig" "Frank'" "DE" True 0
      let pc010 = deactOnePC pc01
      let pc02 = createPhraCate 1 0 c02 "Desig" "loves'" "DE" True 1
      let pc020 = deactOnePC pc02
      let pc11 = createPhraCate 0 1 c11 ">T->B" "Frank' loves'" "OE" True 1
      let pcs = [pc01,pc02]
      let pcs0 = [pc010,pc020]
      let pcClo0 = pcs0 ++ [pc11]
      parse "+++++" pcs `shouldBe` pcClo0

    it "The result of removeOnePC \"+++++\" ((0,1),[(s/.np,\">T->B\",\"Frank' loves'\",\"DE\",False)],1) [((0,0),[(np,\"Desig\",\"Frank'\",\"DE\",False],0), ((1,0),[((s\\.np)/.np,\"Desig\",\"loves'\",\"DE\",False],1), ((2,0),[(np,\"Desig\",\"Mary'\",\"DE\",False],2)，((0,1),[(s/.np,\">T->B\",\"Frank' loves'\",\"DE\",False), ((1,1),[(s\\.np,\">\",\"loves' Mary'\",\"DE\",True)],2),((0,2),[(s,\">\",\"(Frank' loves') Mary'\",\"NR\",True)],2)] is [((0,0),[(np,\"Desig\",\"Frank'\",\"DE\",True)],0), ((1,0),[((s\\.np)/.np,\"Desig\",\"loves'\",\"DE\",False],1), ((2,0),[(np,\"Desig\",\"Mary'\",\"DE\",False],2)，((1,1),[(s\\.np,\">\",\"loves' Mary'\",\"DE\",True)],2)]" $ do
      let c01 = npCate
      let c02 = getCateFromString "(s\\.np)/.np"
      let c03 = npCate
      let c11 = getCateFromString "s/.np"
      let c12 = getCateFromString "s\\.np"
      let c21 = sCate
      let pc01 = createPhraCate 0 0 c01 "Desig" "Frank'" "DE" False 0
      let pc02 = createPhraCate 1 0 c02 "Desig" "loves'" "DE" False 1
      let pc03 = createPhraCate 2 0 c03 "Desig" "Mary'" "DE" False 2
      let pc11 = createPhraCate 0 1 c11 ">T->B" "Frank' loves'" "OE" False 1
      let pc12 = createPhraCate 1 1 c12 ">" "loves' Mary'" "VO" True 2
      let pc21 = createPhraCate 0 2 c21 ">" "(Frank' loves') Mary'" "NR" True 2
      let pc010 = actOnePC pc01
      let pcs = [pc01,pc02,pc03]
      let pcs0 = [pc02,pc03,pc010]
      let pcClo = pcs ++ [pc11,pc12,pc21]
      let pcClo0 = pcs0 ++ [pc12]
      removeOnePC "+++++" pc11 pcClo `shouldBe` pcClo0

    it "The result of newSpanPCs \"+++++\" [((0,0),[(np,\"Desig\",\"Frank'\",\"DE\",True)],0), ((1,0),[((s\\.np)/.np,\"Desig\",\"loves'\",\"DE\",True)],1), ((2,0),[(np,\"Desig\",\"Mary'\",\"DE\",True)],2)] is [((1,0),[((s\\.np)/.np,\"Desig\",\"loves'\",\"DE\",False],1), ((2,0),[(np,\"Desig\",\"Mary'\",\"DE\",False],2), ((0,0),[(np,\"Desig\",\"Frank'\",\"DE\",True)],0), ((1,1),[(s\\.np,\">\",\"loves' Mary'\",\"DE\",True)],2)]" $ do
      let c01 = npCate
      let c02 = getCateFromString "(s\\.np)/.np"
      let c03 = npCate
      let c11 = getCateFromString "s\\.np"
      let pc01 = createPhraCate 0 0 c01 "Desig" "Frank'" "DE" True 0
      let pc02 = createPhraCate 1 0 c02 "Desig" "loves'" "DE" True 1
      let pc03 = createPhraCate 2 0 c03 "Desig" "Mary'" "DE" True 2
      let pc11 = createPhraCate 1 1 c11 ">" "loves' Mary'" "VO" True 2
      let pcs = [pc01,pc02,pc03]
      let pc020 = deactOnePC pc02
      let pc030 = deactOnePC pc03
      let pc110 = deactOnePC pc11
      let pcClo = [pc020,pc030,pc01,pc11]
      newSpanPCs "+++++" pcs `shouldBe` pcClo
    
    it "The result of newSpanPCs \"+++++\" [((0,0),[(np,\"Desig\",\"以色列'\",\"DE\",True)],0), ((1,0),[(np,\"Desig\",\"士兵'\",\"DE\",True)],1), ((2,0),[((s\\.np)/#(s\\.np),\"Desig\",\"十五日'\",\"DE\",True)],2),((3,0),[(((s\\.np)/#(s\\.np))/*np,\"Desig\",\"把'\",\"DE\",True)],3),((4,0),[(s/*s,\"Desig\",\"4000'\",\"DE\",True)],4), ((5,0),[((np/*np)\\*(s/*s),\"Desig\",\"名'\",\"DE\",True)],5), ((6,0),[(np,\"Desig\",\"学生'\",\"DE\",True)],6),((7,0),[((X\\*X)/*X,\"Desig\",\"和'\",\"DE\",True)],7),((8,0),[(np,\"Desig\",\"教师'\",\"DE\",True)],8), ((9,0),[(s\\.np,\"Desig\",\"封锁'\",\"DE\",True)],9), ((10,0),[(((s\\.np)\\x(s\\.np))/*np,\"Desig\",\"在'\",\"DE\",True)],10),((11,0),[(np,\"Desig\",\"约旦河'\",\"DE\",True)],11),((12,0),[(np\\*np,\"Desig\",\"西岸'\",\"DE\",True)],12), ((13,0),[(np,\"Desig\",\"那布鲁斯'\",\"DE\",True)],13), ((14,0),[(np/.np,\"Desig\",\"最大'\",\"DE\",True)],14),((15,0),[((np/*np)\\*(np/.np),\"Desig\",\"的'\",\"DE\",True)],15),((16,0),[(s/*s,\"Desig\",\"一'\",\"DE\",True)],16), ((17,0),[((np/*np)\\*(s/*s),\"Desig\",\"个'\",\"DE\",True)],17), ((18,0),[(np,\"Desig\",\"大学'\",\"DE\",True)],18),((19,0),[(np\\*np,\"Desig\",\"里'\",\"DE\",True)],19)] is OK!" $ do
      let c01 = npCate
      let c02 = npCate
      let c03 = getCateFromString "(s\\.np)/#(s\\.np)"
      let c04 = getCateFromString "((s\\.np)/#(s\\.np))/*np"
      let c05 = getCateFromString "s/*s"
      let c06 = getCateFromString "(np/*np)\\*(s/*s)"
      let c07 = npCate
      let c08 = getCateFromString "(X\\*X)/*X"
      let c09 = npCate
      let c10 = getCateFromString "s\\.np"
      let c11 = getCateFromString "((s\\.np)\\x(s\\.np))/*np"
      let c12 = npCate
      let c13 = getCateFromString "np\\*np"
      let c14 = npCate
      let c15 = getCateFromString "np/.np"
      let c16 = getCateFromString "(np/*np)\\*(np/.np)"
      let c17 = getCateFromString "s/*s"
      let c18 = getCateFromString "(np/*np)\\*(s/*s)"
      let c19 = npCate
      let c20 = getCateFromString "np\\*np"
      let pc01 = createPhraCate 0 0 c01 "Desig" "以色列'" "DE" True 0
      let pc02 = createPhraCate 1 0 c02 "Desig" "士兵'" "DE" True 1
      let cp01 = createPhraCate 0 1 npCate "A/n->" "以色列' 士兵'" "AHn" True 1
      let pc03 = createPhraCate 2 0 c03 "Desig" "十五日'" "DE" True 2
      let pc04 = createPhraCate 3 0 c04 "Desig" "把'" "DE" True 3
      let cp02 = createPhraCate 2 1 (getCateFromString "((s\\.np)/#(s\\.np))/*np") ">B2" "十五日' 把'" "NR" True 3
      let pc05 = createPhraCate 4 0 c05 "Desig" "4000'" "DE" True 4
      let pc06 = createPhraCate 5 0 c06 "Desig" "名'" "DE" True 5
      let cp03 = createPhraCate 4 1 (getCateFromString "np/*np") "<" "名' 4000'" "MQ" True 5
      let pc07 = createPhraCate 6 0 c07 "Desig" "学生'" "DE" True 6
      let pc08 = createPhraCate 7 0 c08 "Desig" "和'" "DE" True 7
      let pc09 = createPhraCate 8 0 c09 "Desig" "教师'" "DE" True 8
      let cp04 = createPhraCate 7 1 (getCateFromString "np\\*np") ">" "和' 教师'" "XX" True 8
      let pc10 = createPhraCate 9 0 c10 "Desig" "封锁'" "DE" True 9
      let pc11 = createPhraCate 10 0 c11 "Desig" "在'" "DE" True 10
      let pc12 = createPhraCate 11 0 c12 "Desig" "约旦河'" "DE" True 11
      let pc13 = createPhraCate 12 0 c13 "Desig" "西岸'" "DE" True 12
      let cp05 = createPhraCate 11 1 npCate "<" "西岸' 约旦河'" "HnC" True 12
      let pc14 = createPhraCate 13 0 c14 "Desig" "那不勒斯'" "DE" True 13
      let pc15 = createPhraCate 14 0 c15 "Desig" "最大'" "DE" True 14
      let pc16 = createPhraCate 15 0 c16 "Desig" "的'" "DE" True 15
      let cp06 = createPhraCate 14 1 (getCateFromString "np/*np") "<" "的' 最大'" "U1P" True 15
      let pc17 = createPhraCate 16 0 c17 "Desig" "一'" "DE" True 16
      let pc18 = createPhraCate 17 0 c18 "Desig" "所'" "DE" True 17
      let cp07 = createPhraCate 16 1 (getCateFromString "np/*np") "<" "所' 一'" "MQ" True 17
      let pc19 = createPhraCate 18 0 c19 "Desig" "大学'" "DE" True 18
      let pc20 = createPhraCate 19 0 c20 "Desig" "里'" "DE" True 19
      let cp08 = createPhraCate 18 1 npCate "<" "里' 大学'" "HnC" True 19
      let pc010 = deactOnePC pc01
      let pc020 = deactOnePC pc02
      let pc030 = deactOnePC pc03
      let pc040 = deactOnePC pc04
      let pc050 = deactOnePC pc05
      let pc060 = deactOnePC pc06
      let pc070 = deactOnePC pc07
      let pc080 = deactOnePC pc08
      let pc090 = deactOnePC pc09
      let pc100 = deactOnePC pc10
      let pc110 = deactOnePC pc11
      let pc120 = deactOnePC pc12
      let pc130 = deactOnePC pc13
      let pc140 = deactOnePC pc14
      let pc150 = deactOnePC pc15
      let pc160 = deactOnePC pc16
      let pc170 = deactOnePC pc17
      let pc180 = deactOnePC pc18
      let pc190 = deactOnePC pc19
      let pc200 = deactOnePC pc20

      let pcs = [pc01,pc02,pc03,pc04,pc05,pc06,pc07,pc08,pc09,pc10,pc11,pc12,pc13,pc14,pc15,pc16,pc17,pc18,pc19,pc20]
      let pcs1 = quickSort [pc010,pc020,pc030,pc040,pc050,pc060,pc07,pc080,pc090,pc10,pc11,pc120,pc130,pc14,pc150,pc160,pc170,pc180,pc190,pc200,cp01,cp02,cp03,cp04,cp05,cp06,cp07,cp08]
      quickSort (newSpanPCs "+++++" pcs) `shouldBe` pcs1
    
    it "The result of parse \"+++++\" [((0,0),[(np,\"Desig\",\"Frank'\",\"DE\",True)],0), ((1,0),[((s\\.np)/.np,\"Desig\",\"loves'\",\"DE\",True)],1), ((2,0),[(np,\"Desig\",\"Mary'\",\"DE\",True)],2)] is [((0,0),[(np,\"Desig\",\"Frank'\",\"DE\",False],0), ((1,0),[((s\\.np)/.np,\"Desig\",\"loves'\",\"DE\",False],1), ((2,0),[(np,\"Desig\",\"Mary'\",\"DE\",False],2), ((1,1),[(s\\.np,\">\",\"loves' Mary'\",\"DE\",False],2), ((0,2),[(s,\"<\",\"(loves' Mary') Frank'\",\"DE\",True)]" $ do
      let c01 = npCate
      let c02 = getCateFromString "(s\\.np)/.np"
      let c03 = npCate
      let c11 = getCateFromString "s\\.np"
      let c21 = sCate
      let pc01 = createPhraCate 0 0 c01 "Desig" "Frank'" "DE" True 0
      let pc02 = createPhraCate 1 0 c02 "Desig" "loves'" "DE" True 1
      let pc03 = createPhraCate 2 0 c03 "Desig" "Mary'" "DE" True 2
      let pc010 = deactOnePC pc01
      let pc020 = deactOnePC pc02
      let pc030 = deactOnePC pc03
      let pc11 = createPhraCate 1 1 c11 ">" "loves' Mary'" "VO" False 2
      let pc21 = createPhraCate 0 2 c21 "<" "(loves' Mary') Frank'" "SP" True 1
      let pcs = [pc01,pc02,pc03]
      let pcClo = [pc020,pc030,pc010,pc11,pc21]
      parse "+++++" pcs `shouldBe` pcClo

    it "The result of parse \"+++++\" [((0,0),[(np/.np,\"Desig\",\"Brave'\"，True)],0), ((1,0),[(np,\"Desig\",\"Frank'\",\"DE\",True)],1), ((2,0),[(s\\*np,\"Desig\",\"wins'\",\"DE\",True)],2)] is [((0,0),[(np/.np,\"Desig\",\"Brave'\",\"DE\",True)],0), ((1,0),[(np,\"Desig\",\"Frank'\",\"DE\",True)],1), ((2,0),[(s\\*np,\"Desig\",\"wins'\",\"DE\",True)],2), ((0,1),[(np,\">\",\"Brave' Frank'\",\"DE\",True)],1), ((1,1),[(s,\"<\",\"wins' Frank'\",\"DE\",True),(np/.np,\"A/n->B\",\"Brave' Frank'\",\"DE\",True)],2), ((0,2),[(np,\"Np/s->\",\"Brave' (wins' Frank')\",\"DE\",True)],1), ((0,2),[(s,\"<\",\"wins' (Brave' Frank')\",\"DE\",True),(np,\"Np/v->\",\"(Brave' Frank') wins'\",\"DE\",True),(s,\"Np/a-<\",\"wins' (Brave' Frank')\",\"DE\",True)],2)], without considering element order." $ do
      let c01 = getCateFromString "np/.np"
      let c02 = npCate
      let c03 = getCateFromString "s\\.np"
      let c11 = npCate
      let c21 = sCate
      let pc01 = createPhraCate 0 0 c01 "Desig" "Brave'" "DE" True 0
      let pc02 = createPhraCate 1 0 c02 "Desig" "Frank'" "DE" True 1
      let pc03 = createPhraCate 2 0 c03 "Desig" "wins'" "DE" True 2
      let pc11 = createPhraCate 0 1 c11 ">" "Brave' Frank'" "AHn" False 1
      let pc21 = createPhraCate 0 2 c21 "<" "wins' (Brave' Frank')" "SP" True 2 
      let pcs = [pc01,pc02,pc03]
      let pc010 = deactOnePC pc01
      let pc020 = deactOnePC pc02
      let pc030 = deactOnePC pc03
      let pcClo = [pc010,pc020,pc11,pc030,pc21]
      parse "+++++" pcs `shouldBe` pcClo

    it "The result of parse \"+++++\"  [((0,0),[(np/.np,\"Desig\",\"Good'\",\"DE\",True)],0), ((1,0),[((s\\.np)/.np,\"Desig\",\"is_better_than'\",\"DE\",True)],1), ((2,0),[(np/.np,\"Desig\",\"bad'\",\"DE\",True)],2)] is [((0,0),[(np/.np,\"Desig\",\"Good'\",\"DE\",False],0), ((1,0),[((s\\.np)/.np,\"Desig\",\"is_better_than'\",\"DE\",False],1), ((2,0),[(np/.np,\"Desig\",\"bad'\",\"DE\",False],2), ((1,1),[(s\\.np,\"Np/a->\",\"is_better_than' bad'\",\"DE\",False],2),((0,2),[(s,\"Np/a-<\",\"(is_better_than' bad') Good'\",\"DE\",True)],2)]" $ do
      let c01 = getCateFromString "np/.np"
      let c02 = getCateFromString "(s\\.np)/.np"
      let c03 = getCateFromString "np/.np"
      let c11 = getCateFromString "s\\.np"
      let c21 = sCate
      let pc01 = createPhraCate 0 0 c01 "Desig" "Good'" "DE" True 0
      let pc02 = createPhraCate 1 0 c02 "Desig" "is_better_than'" "DE" True 1
      let pc03 = createPhraCate 2 0 c03 "Desig" "bad'" "DE" True 2
      let pc010 = deactOnePC pc01
      let pc020 = deactOnePC pc02
      let pc030 = deactOnePC pc03
      let pc11 = createPhraCate 1 1 c11 "Np/a->" "is_better_than' bad'" "VO" False 2
      let pc21 = createPhraCate 0 2 c21 "Np/a-<" "(is_better_than' bad') Good'" "SP" True 1
      let pcs = [pc01,pc02,pc03]
      let pcClo = [pc020,pc030,pc010,pc11,pc21]
      parse "+++++" pcs `shouldBe` pcClo


    it "The result of isPrior2 [((0,0),[(np/.np,\"Desig\",\"Brave'\",\"DE\",True)],0), ((1,0),[(np,\"Desig\",\"Frank'\",\"DE\",True)],1), ((2,0),[(s\\*np,\"Desig\",\"wins'\",\"DE\",True)],2), ((0,1),[(np,\">\",\"Brave' Frank'\",\"AHn\",True)],1), ((1,1),[(s,\"<\",\"wins' Frank'\",\"SP\",True)],2)] ((0,1),[(np,\">\",\"Brave' Frank'\",\"AHn\",True)],1) ((1,1),[(s,\"<\",\"wins' Frank'\",\"SP\",True)],2) is False" $ do
      let c01 = getCateFromString "np/.np"
      let c02 = npCate
      let c03 = getCateFromString "s\\*np"
      let c11 = npCate
      let c12 = sCate
      let pc01 = createPhraCate 0 0 c01 "Desig" "Brave'" "DE" False 0
      let pc02 = createPhraCate 1 0 c02 "Desig" "Frank'" "DE" False 1
      let pc03 = createPhraCate 2 0 c03 "Desig" "wins'" "DE" False 2
      let pc11 = createPhraCate 0 1 c11 ">" "Brave' Frank'" "AHn" True 1
      let pc12 = createPhraCate 1 1 c12 "<" "wins' Frank'" "SP" True 2
      let pcs = [pc01,pc02,pc03]
      let pcClo = pcs ++ [pc11,pc12]
      isPrior2 pcClo pc11 pc12 `shouldBe` True 

    it "The result of isPrior2  [((0,0),[(np/.np,\"Desig\",\"Good'\",\"DE\",False)],0), ((1,0),[((s\\.np)/.np,\"Desig\",\"is_better_than'\",\"DE\",True)],1), ((2,0),[(np/.np,\"Desig\",\"bad'\",\"DE\",True)],2), ((0,1),[(s/.np,\"Np/a->T->B\",\"Good' is_better_than'\",\"OE\",True)],1),((1,1),[((s\\.np)/.np,\">B\",\"is_better_than' bad'\",\"NR\",True)],2), ((1,1),[(s\\.np,\"Np/a->\",\"is_better_than' bad'\",\"VO\",True)],2)] ((0,1),[(s/.np,\"Np/a->T->B\",\"Good' is_better_than'\",\"NR\",True)],1),((1,1),[((s\\.np)/.np,\">B\",\"is_better_than' bad'\",\"NR\",True)],2) is False" $ do
      let c01 = getCateFromString "np/.np"
      let c02 = getCateFromString "(s\\.np)/.np"
      let c03 = getCateFromString "np/.np"
      let c11 = getCateFromString "s/.np"
      let c12 = getCateFromString "(s\\.np)/.np"
      let c13 = getCateFromString "s\\.np"
      let pc01 = createPhraCate 0 0 c01 "Desig" "Good'" "DE" True 0
      let pc02 = createPhraCate 1 0 c02 "Desig" "is_better_than'" "DE" True 1
      let pc03 = createPhraCate 2 0 c03 "Desig" "bad'" "DE" True 2
      let pc11 = createPhraCate 0 1 c11 "Np/a->T->B" "Good' is_better_than'" "NR" True 1
      let pc12 = createPhraCate 1 1 c12 ">B" "is_better_than' bad'" "NR" True 2
      let pc13 = createPhraCate 1 1 c13 "Np/a->" "is_better_than' bad'" "VO" True 2
      let pcClo = [pc01,pc02,pc03,pc11,pc12,pc13]
      isPrior2 [pc01,pc02,pc03,pc11,pc12,pc13] pc12 pc13 `shouldBe` False 
 
    it "The result of atomizePhraCate [((0,0),[(np,\"Desig\",\"Frank\")],1), ((1,1),[(s\\.np,\">\",\"loves' Mary'\"),((s/.np,\">T->B\",\"Tim' loves'\"))],2)] is [((0,0),[(np,\"Desig\",\"Frank\")],1), ((1,1),[(s\\.np,\">\",\"loves' Mary'\"),2)],2), ((1,1),[(s/.np,\">T->B\",\"Tim' loves'\")],2)]" $ do
      let pc1 = createPhraCate 0 0 npCate "Desig" "Frank" "DE" True 1
      let pc2 = createPhraCate2 1 1 [(getCateFromString "s\\.np",">","loves' Mary'","DE",True),(getCateFromString "s/.np",">T->B","Tom' loves'","DE",True)] 2
      let pc21 = createPhraCate 1 1 (getCateFromString "s\\.np") ">" "loves' Mary'" "DE" True 2
      let pc22 = createPhraCate 1 1 (getCateFromString "s/.np") ">T->B" "Tom' loves'" "DE" True 2
      atomizePhraCate [pc1,pc2] `shouldBe` [pc1,pc21,pc22]

    it "The result of findCate （0,-1) [((0,0),[(np,\"Desig\",\"Frank'\")],0), ((1,0),[(s\\.np,\"Desig\",\"smiles\")],1)] is []" $ do
      let c1 = npCate
      let c2 = getCateFromString "s\\.np"
      findCate (0,-1) [((0,0),[(c1,"Desig","Frank'","DE",True)],0), ((1,0),[(c2,"Desig","smiles'","DE",True)],1)] `shouldBe` []

    it "The result of findCate （0,0) [((0,0),[(np,\"Desig\",\"Frank'\")],0), ((1,0),[(s\\.np,\"Desig\",\"loves' Mary'\")],1)] is ((0,0),[(np,\"Desig\",\"Frank'\")],0)" $ do
      let c1 = npCate
      let c2 = getCateFromString "s\\.np"
      let pc1 = createPhraCate 0 0 c1 "Desig" "Frank'" "DE" True 0
      let pc2 = createPhraCate 1 0 c2 "Desig" "loves' Mary'" "DE" True 1
      findCate (0,0) [pc1, pc2] `shouldBe` [pc1]

    it "The result of findSplitCate ((1,0),[(np,\"Desig\",\"Frank'\")],1) [((0,0),[(np/.np,\"Desig\",\"Brave'\")],0), ((1,0),[(np,\"Desig\",\"Frank'\")],1), ((2,0),[(s\\*np,\"Desig\"),\"wins'\"],2), ((0,1),[(np,\">\",\"Brave' Frank'\")],1), ((1,1),[(s,\"<\",\"wins' Frank'\")],2), ((0,2),[np,\"Np/s->\",\"Brave' (wins' Frank')\"],1), ((0,2),[(s,\"<\",\"wins' (Brave' Frank')\")],2)] is []" $ do
      let c01 = getCateFromString "np/.np"
      let c02 = npCate
      let c03 = getCateFromString "s\\*np"
      let c11 = npCate
      let c12 = sCate
      let c21 = npCate
      let c22 = sCate
      let pc01 = createPhraCate 0 0 c01 "Desig" "Brave'" "DE" True 0
      let pc02 = createPhraCate 1 0 c02 "Desig" "Frank'" "DE" True 1
      let pc03 = createPhraCate 2 0 c03 "Desig" "wins'" "DE" True 2
      let pc11 = createPhraCate 0 1 c11 ">" "Brave' Frank'" "AHn" True 1
      let pc12 = createPhraCate 1 1 c12 "<" "wins' Frank'" "SP" True 2
      let pc21 = createPhraCate 0 2 c21 "Np/s->" "Brave' (wins' Frank')" "AHn" True 1
      let pc22 = createPhraCate 0 2 c22 "<" "wins' (Brave' Frank')" "SP" True 2 
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
      let pc01 = createPhraCate 0 0 c01 "Desig" "Brave'" "DE" True 0
      let pc02 = createPhraCate 1 0 c02 "Desig" "Frank'" "DE" True 1
      let pc03 = createPhraCate 2 0 c03 "Desig" "wins'" "DE" True 2
      let pc11 = createPhraCate 0 1 c11 ">" "Brave' Frank'" "AHn" True 1
      let pc12 = createPhraCate 1 1 c12 "<" "wins' Frank'" "SP" True 2
      let pc21 = createPhraCate 0 2 c21 "Np/s->" "Brave' (wins' Frank')" "AHn" True 1
      let pc22 = createPhraCate 0 2 c22 "<" "wins' (Brave' Frank')" "SP" True 2 
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
      let pc01 = createPhraCate 0 0 c01 "Desig" "Brave'" "DE" True 0
      let pc02 = createPhraCate 1 0 c02 "Desig" "Frank'" "DE" True 1
      let pc03 = createPhraCate 2 0 c03 "Desig" "wins'" "DE" True 2
      let pc11 = createPhraCate 0 1 c11 ">" "Brave' Frank'" "AHn" True 1
      let pc12 = createPhraCate 1 1 c12 "<" "wins' Frank'" "SP" True 2
      let pc21 = createPhraCate 0 2 c21 "Np/s->" "Brave' (wins' Frank')" "AHn" True 1
      let pc22 = createPhraCate 0 2 c22 "<" "wins' (Brave' Frank')" "SP" True 2 
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
      let pc01 = createPhraCate 0 0 c01 "Desig" "Brave'" "DE" True 0
      let pc02 = createPhraCate 1 0 c02 "Desig" "Frank'" "DE" True 1
      let pc03 = createPhraCate 2 0 c03 "Desig" "wins'" "DE" True 2
      let pc11 = createPhraCate 0 1 c11 ">" "Brave' Frank'" "AHn" True 1
      let pc12 = createPhraCate 1 1 c12 "<" "wins' Frank'" "SP" True 2
      let pc21 = createPhraCate 0 2 c21 "Np/s->" "Brave' (wins' Frank')" "AHn" True 1
      let pc22 = createPhraCate 0 2 c22 "<" "wins' (Brave' Frank')" "SP" True 2
      let pcs = [pc01,pc02,pc03]
      let pcClo = pcs ++ [pc11,pc12,pc21,pc22]
      findSplitCate "+++++" pc12 pcClo `shouldBe` [(pc02,pc03)]

    it "The result of findSplitCate ((0,2),[(s,\"Np/a-<\",\"(is_better_than' bad') Good'\",\"DE\",False],1)  [((0,0),[(np/.np,\"Desig\",\"Good'\",\"DE\",False],0), ((1,0),[((s\\.np)/.np,\"Desig\",\"is_better_than'\",\"DE\",False],1), ((2,0),[(np/.np,\"Desig\",\"bad'\",\"DE\",False],2), ((1,1),[(s\\.np,\"Np/a->\",\"is_better_than' bad'\"),False],2), ((0,2),[(s,\"Np/a-<\",\"(is_better_than' bad') Good'\",\"DE\",True)],1) is [(((0,0),[(np/.np,\"Desig\",\"Good'\",\"DE\",False),0),((1,1),[(s\\.np,\"Np/a->\",\"is_better_than' bad'\",\"DE\",False],2)]" $ do
      let c01 = getCateFromString "np/.np"
      let c02 = getCateFromString "(s\\.np)/.np"
      let c03 = getCateFromString "np/.np"
      let c11 = getCateFromString "s\\.np"
      let c21 = sCate
      let pc01 = createPhraCate 0 0 c01 "Desig" "Good'" "DE" False 0
      let pc02 = createPhraCate 1 0 c02 "Desig" "is_better_than'" "DE" False 1
      let pc03 = createPhraCate 2 0 c03 "Desig" "bad'" "DE" False 2
      let pc11 = createPhraCate 1 1 c11 "Np/a->" "is_better_than' bad'" "VO" False 2
      let pc21 = createPhraCate 0 2 c21 "Np/a-<" "(is_better_than' bad') Good'" "SP" True 1
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
      let pc01 = createPhraCate 0 0 c01 "Desig" "Brave'" "DE" True 0
      let pc02 = createPhraCate 1 0 c02 "Desig" "Frank'" "DE" True 1
      let pc03 = createPhraCate 2 0 c03 "Desig" "wins'" "DE" True 2
      let pc11 = createPhraCate 0 1 c11 ">" "Brave' Frank'" "AHn" True 1
      let pc12 = createPhraCate 1 1 c12 "<" "wins' Frank'" "SP" True 2
      let pc21 = createPhraCate 0 2 c21 "Np/s->" "Brave' (wins' Frank')" "AHn" True 1
      let pc22 = createPhraCate 0 2 c22 "<" "wins' (Brave' Frank')" "SP" True 2
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
      let pc01 = createPhraCate 0 0 c01 "Desig" "Brave'" "DE" True 0
      let pc02 = createPhraCate 1 0 c02 "Desig" "Frank'" "DE" True 1
      let pc03 = createPhraCate 2 0 c03 "Desig" "wins'" "DE" True 2
      let pc11 = createPhraCate 0 1 c11 ">" "Brave' Frank'" "AHn" True 1
      let pc12 = createPhraCate 1 1 c12 "<" "wins' Frank'" "SP" True 2
      let pc21 = createPhraCate 0 2 c21 "Np/s->" "Brave' (wins' Frank')" "AHn" True 1
      let pc22 = createPhraCate 0 2 c22 "<" "wins' (Brave' Frank')" "SP" True 2
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
      let pc01 = createPhraCate 0 0 c01 "Desig" "Good'" "DE" True 0
      let pc02 = createPhraCate 1 0 c02 "Desig" "is_better_than'" "DE" True 1
      let pc03 = createPhraCate 2 0 c03 "Desig" "bad'" "DE" True 2
      let pc11 = createPhraCate 0 1 c11 "Np/v->" "Good' is_better_than'" "AHn" True 1
      let pc12 = createPhraCate 0 1 c12 "Np/a->T->B" "Good' is_better_than'" "OE" True 1
      let pc13 = createPhraCate 1 1 c13 ">B" "is_better_than' bad'" "NR" True 2
      let pc14 = createPhraCate 1 1 c14 "Np/a->" "is_better_than' bad'" "VO" True 2
      let pc21 = createPhraCate 0 2 c21 "Np/v->" "Good' (is_better_than' bad')" "AHn" True 1
      let pc22 = createPhraCate 0 2 c22 "Np/a-<" "(is_better_than' bad') Good'" "SP" True 1
      let pc23 = createPhraCate 0 2 c23 "Np/a->T->B" "Good' (is_better_than' bad')" "OE" True 1
      let pc24 = createPhraCate 0 2 c24 ">B" "(Good' is_better_than') bad'" "NR" True 2
      let pc25 = createPhraCate 0 2 c25 "Np/a->" "(Good' is_better_than') bad'" "AHn" True 2
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
      let pc01 = createPhraCate 0 0 c01 "Desig" "Brave'" "DE" True 0
      let pc02 = createPhraCate 1 0 c02 "Desig" "Frank'" "DE" True 1
      let pc03 = createPhraCate 2 0 c03 "Desig" "wins'" "DE" True 2
      let pc11 = createPhraCate 0 1 c11 ">" "Brave' Frank'" "AHn" True 1
      let pc12 = createPhraCate 1 1 c12 "<" "wins' Frank'" "SP" True 2
      let pc21 = createPhraCate 0 2 c21 "Np/s->" "Brave' (wins' Frank')" "AHn" True 1
      let pc22 = createPhraCate 0 2 c22 "<" "wins' (Brave' Frank')" "SP" True 2
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
      let pc01 = createPhraCate 0 0 c01 "Desig" "Brave'" "DE" True 0
      let pc02 = createPhraCate 1 0 c02 "Desig" "Frank'" "DE" True 1
      let pc03 = createPhraCate 2 0 c03 "Desig" "wins'" "DE" True 2
      let pc11 = createPhraCate 0 1 c11 ">" "Brave' Frank'" "AHn" True 1
      let pc12 = createPhraCate 1 1 c12 "<" "wins' Frank'" "SP" True 2
      let pc21 = createPhraCate 0 2 c21 "Np/s->" "Brave' (wins' Frank')" "AHn" True 1
      let pc22 = createPhraCate 0 2 c22 "<" "wins' (Brave' Frank')" "SP" True 2
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
      let pc01 = createPhraCate 0 0 c01 "Desig" "Brave'" "DE" True 0
      let pc02 = createPhraCate 1 0 c02 "Desig" "Frank'" "DE" True 1
      let pc03 = createPhraCate 2 0 c03 "Desig" "wins'" "DE" True 2
      let pc11 = createPhraCate 0 1 c11 ">" "Brave' Frank'" "AHn" True 1
      let pc12 = createPhraCate 1 1 c12 "<" "wins' Frank'" "SP" True 2
      let pc21 = createPhraCate 0 2 c21 "Np/s->" "Brave' (wins' Frank')" "AHn" True 1
      let pc22 = createPhraCate 0 2 c22 "<" "wins' (Brave' Frank')" "SP" True 2
      let pcs = [pc01,pc02,pc03]
      let pcClo = pcs ++ [pc11,pc12,pc21,pc22]
      findCateBySpan 2 pcClo `shouldBe` [pc21,pc22]

    it "The result of divPhraCateBySpan [((0,0),[(np/.np,\"Desig\",\"Brave'\",\"DE\",True)],0), ((1,0),[(np,\"Desig\",\"Frank'\",\"DE\",True)],1), ((2,0),[(s\\*np,\"Desig\"),\"wins'\",\"DE\",True)],2), ((0,1),[(np,\">\",\"Brave' Frank'\",\"DE\",True)],1), ((0,2),[(s,\"<\",\"wins' (Brave' Frank')\",\"DE\",True)],2)] is [[((0,0),[(np/.np, \"Desig\",\"Brave'\",\"DE\",True)],0), ((1,0),[(np,\"Desig\",\"Frank'\",\"DE\",True)],1), ((2,0),[(s\\*np, \"Desig\",\"wins'\",\"DE\",True)],2)], [((0,1),[(np, \">\",\"Brave' Frank'\",\"DE\",True)],1)], [((0,2),[(s,\"<\",\"wins' (Brave' Frank')\",\"DE\",True)],2)]]" $ do
      let c01 = getCateFromString "np/.np"
      let c02 = npCate
      let c03 = getCateFromString "s\\*np"
      let c11 = npCate
      let c12 = sCate
      let c21 = npCate
      let c22 = sCate
      let pc01 = createPhraCate 0 0 c01 "Desig" "Brave'" "DE" True 0
      let pc02 = createPhraCate 1 0 c02 "Desig" "Frank'" "DE" True 1
      let pc03 = createPhraCate 2 0 c03 "Desig" "wins'" "DE" True 2
      let pc11 = createPhraCate 0 1 c11 ">" "Brave' Frank'" "AHn" True 1
      let pc12 = createPhraCate 1 1 c12 "<" "wins' Frank'" "SP" True 2
      let pc21 = createPhraCate 0 2 c21 "Np/s->" "Brave' (wins' Frank')" "AHn" True 1
      let pc22 = createPhraCate 0 2 c22 "<" "wins' (Brave' Frank')" "SP" True 2
      let pcClo = [pc01,pc02,pc03,pc11,pc22]
      divPhraCateBySpan pcClo `shouldBe` [[pc01,pc02,pc03],[pc11],[pc22]]

    it "The result of growTree \"+++++\" [((0,2),[(s, \">\",\"(loves' Frank') Mary'\",\"DE\",True)],2)] [((0,0),[(np, \"Desig\",\"Frank'\",\"DE\",False],0), ((1,0),[((s\\.np)/.np, \"Desig\",\"loves'\",\"DE\",False],1), ((2,0),[(np, \"Desig\",\"Mary'\",\"DE\",False],2), ((0,1),[(s/.np,\">T->B\",\"Frank' loves'\",\"DE\",False],1), ((1,1),[(s\\.np, \"<\",\"loves' Frank'\",\"DE\",False],2), ((0,2),[(s, \"<\",\"(loves' Mary') Frank'\",\"DE\",True)],1), ((0,2),[(s, \">\", \"(Frank' loves') Mary'\",\"DE\",True)],2) is [((0,0),[(np, \"Desig\",\"Frank'\",\"DE\",True)],0), ((1,0),[((s\\.np)/.np, \"Desig\",\"loves'\",\"DE\",True)],1), ((0,1),[(s/.np, \">T->B\",\"Frank' loves'\",\"DE\",True)],1), ((2,0),[(np, \"Desig\",\"Mary'\",\"DE\",True)],2), ((0,2),[(s, \">\",\"(Frank' loves') Mary'\",\"DE\",True)],2)]" $ do
      let c01 = npCate
      let c02 = getCateFromString "(s\\.np)/.np"
      let c03 = npCate
      let c11 = getCateFromString "s/.np"
      let c12 = getCateFromString "s\\.np"
      let c21 = sCate
      let c22 = sCate
      let pc01 = createPhraCate 0 0 c01 "Desig" "Frank'" "DE" False 0
      let pc02 = createPhraCate 1 0 c02 "Desig" "loves'" "DE" False 1
      let pc03 = createPhraCate 2 0 c03 "Desig" "Mary'" "DE" False 2
      let pc11 = createPhraCate 0 1 c11 ">T->B" "Frank' loves'" "OE" False 1 
      let pc12 = createPhraCate 1 1 c12 ">" "loves' Mary'" "VO" False 2
      let pc21 = createPhraCate 0 2 c21 "<" "(loves' Mary') Frank'" "SP" True 1
      let pc22 = createPhraCate 0 2 c22 ">" "(Frank' loves') Mary'" "NR" True 2 
      let pcClo = [pc01,pc02,pc03,pc11,pc12,pc21,pc22]
      growTree "+++++" [pc22] pcClo `shouldBe` [[pc01,pc02,pc11,pc03,pc22]]

    it "The result of growTree \"+++++\" [((0,2),[(s, \"<\", \"(loves' Mary') Frank'\",\"DE\",True)],1)]  [((0,0),[(np, \"Desig\",\"Frank'\",\"DE\",True)],0), ((1,0),[((s\\.np)/.np, \"Desig\",\"loves'\",\"DE\",True)],1), ((2,0),[(np, \"Desig\",\"Mary'\",\"DE\",True)],2), ((0,1),[(s/.np,\">T->B\",\"Frank' loves'\",\"DE\",True)],1), ((1,1),[(s\\.np, \"<\",\"loves' Frank'\",\"DE\",True)],2), ((0,2),[(s, \"<\",\"(loves' Mary') Frank'\",\"DE\",True)],1), ((0,2),[(s, \">\", \"(Frank' loves') Mary'\",\"DE\",True)],2) is [((1,0),[(s\\.np/.np, \"Desig\",\"loves'\",\"DE\",True)],1), ((2,0),[(np, \"Desig\",\"Mary'\",\"DE\",True)],2), ((0,0),[(np, \"Desig\",\"Frank'\",\"DE\",True)],0), ((1,1),[(s\\.np, \">\",\"loves' Mary'\",\"DE\",True)],2), ((0,2),[(s, \"<\", \"(loves' Mary') Frank'\",\"DE\",True)],1)]" $ do
      let c01 = npCate
      let c02 = getCateFromString "(s\\.np)/.np"
      let c03 = npCate
      let c11 = getCateFromString "s/.np"
      let c12 = getCateFromString "s\\.np"
      let c21 = sCate
      let c22 = sCate
      let pc01 = createPhraCate 0 0 c01 "Desig" "Frank'" "DE" True 0
      let pc02 = createPhraCate 1 0 c02 "Desig" "loves'" "DE" True 1
      let pc03 = createPhraCate 2 0 c03 "Desig" "Mary'" "DE" True 2
      let pc11 = createPhraCate 0 1 c11 ">T->B" "Frank' loves'" "OE" True 1
      let pc12 = createPhraCate 1 1 c12 ">" "loves' Mary'" "VO" True 2
      let pc21 = createPhraCate 0 2 c21 "<" "(loves' Mary') Frank'" "SP" True 1
      let pc22 = createPhraCate 0 2 c22 ">" "(Frank' loves') Mary'" "NR" True 2
      let pcClo = [pc01,pc02,pc03,pc11,pc12,pc21,pc22]
      growTree "+++++" [pc21] pcClo `shouldBe` [[pc02,pc03,pc01,pc12,pc21]]

    it "The result of growTree \"+++++\" ((0,2),[(s,\"Np/a-<\",\"(is_better_than' bad') Good'\",\"DE\",True)],1)  [((0,0),[(np/.np,\"Desig\",\"Good'\",\"DE\",False],0), ((1,0),[((s\\.np)/.np,\"Desig\",\"is_better_than'\",\"DE\",False],1), ((2,0),[(np/.np,\"Desig\",\"bad'\",\"DE\",False],2), ((1,1),[(s\\.np,\"Np/a->\",\"is_better_than' bad'\",\"DE\",False],2), ((0,2),[(s,\"Np/a-<\",\"(is_better_than' bad') Good'\",\"DE\",True)],1)] is [((1,0),[((s\\.np)/.np,\"Desig\",\"is_better_than'\",\"DE\",False],1), ((2,0),[(np/.np,\"Desig\",\"bad'\",\"DE\",False],2), ((0,0),[(np/.np,\"Desig\",\"Good'\",\"DE\",False],0), ((1,1),[(s\\.np,\"Np/a->\",\"is_better_than' bad'\",\"DE\",False],2), ((0,2),[(s,\"Np/a-<\",\"(is_better_than' bad') Good'\",\"DE\",True)],1)]" $ do
      let c01 = getCateFromString "np/.np"
      let c02 = getCateFromString "(s\\.np)/.np"
      let c03 = getCateFromString "np/.np"
      let c11 = getCateFromString "s\\.np"
      let c21 = sCate
      let pc01 = createPhraCate 0 0 c01 "Desig" "Good'" "DE" False 0
      let pc02 = createPhraCate 1 0 c02 "Desig" "is_better_than'" "DE" False 1
      let pc03 = createPhraCate 2 0 c03 "Desig" "bad'" "DE" False 2
      let pc11 = createPhraCate 1 1 c11 "Np/a->" "is_better_than' bad'" "VO" False 2
      let pc21 = createPhraCate 0 2 c21 "Np/a-<" "(is_better_than' bad') Good'" "SP" True 1
      let pcClo = [pc02,pc03,pc01,pc11,pc21]
      growTree "+++++" [pc21] pcClo `shouldBe` [pcClo]

    it "The result of growForest \"+++++\" [[((0,2),[(s, \"<\", \"(loves' Mary') Frank'\",\"DE\",True)],1)], [((0,2),[(s, \">\",\"(loves' Frank') Mary'\",\"DE\",True)],2)]]  [((0,0),[(np, \"Desig\",\"Frank'\",\"DE\",True)],0), ((1,0),[((s\\.np)/.np, \"Desig\",\"loves'\",\"DE\",True)],1), ((2,0),[(np, \"Desig\",\"Mary'\",\"DE\",True)],2), ((0,1),[(s/.np,\">T->B\",\"Frank' loves'\",\"DE\",True)],1), ((1,1),[(s\\.np, \"<\",\"loves' Frank'\",\"DE\",True)],2), ((0,2),[(s, \"<\",\"(loves' Mary') Frank'\",\"DE\",True)],1), ((0,2),[(s, \">\", \"(Frank' loves') Mary'\",\"DE\",True)],2) is [[((1,0),[(s\\.np/.np, \"Desig\",\"loves'\",\"DE\",True)],1), ((2,0),[(np, \"Desig\",\"Mary'\",\"DE\",True)],2), ((0,0),[(np, \"Desig\",\"Frank'\",\"DE\",True)],0), ((1,1),[(s\\.np, \">\",\"loves' Mary'\",\"DE\",True)],2), ((0,2),[(s, \"<\", \"(loves' Mary') Frank'\",\"DE\",True)],1)], [((0,0),[(np, \"Desig\",\"Frank'\",\"DE\",True)],0), ((1,0),[((s\\.np)/.np, \"Desig\",\"loves'\",\"DE\",True)],1), ((0,1),[(s/.np, \">T->B\",\"Frank' loves'\",\"DE\",True)],1), ((2,0),[(np, \"Desig\",\"Mary'\",\"DE\",True)],2), ((0,2),[(s, \">\",\"(Frank' loves') Mary'\",\"DE\",True)],2)]]" $ do
      let c01 = npCate
      let c02 = getCateFromString "(s\\.np)/.np"
      let c03 = npCate
      let c11 = getCateFromString "s/.np"
      let c12 = getCateFromString "s\\.np"
      let c21 = sCate
      let c22 = sCate
      let pc01 = createPhraCate 0 0 c01 "Desig" "Frank'" "DE" False 0
      let pc02 = createPhraCate 1 0 c02 "Desig" "loves'" "DE" False 1
      let pc03 = createPhraCate 2 0 c03 "Desig" "Mary'" "DE" False 2
      let pc11 = createPhraCate 0 1 c11 ">T->B" "Frank' loves'" "OE" True 1
      let pc12 = createPhraCate 1 1 c12 ">" "loves' Mary'" "VO" True 2
      let pc21 = createPhraCate 0 2 c21 "<" "(loves' Mary') Frank'" "SP" True 1
      let pc22 = createPhraCate 0 2 c22 ">" "(Frank' loves') Mary'" "NR" True 2
      let pcClo = [pc01,pc02,pc03,pc11,pc12,pc21,pc22]
      growForest "+++++" [[pc21],[pc22]] pcClo `shouldBe` [[pc02,pc03,pc01,pc12,pc21],[pc01,pc02,pc11,pc03,pc22]]

    it "The result of growForest \"+++++\" [[((0,2),[(np,\"Np/s->\",\"Brave' (wins' Frank')\",\"DE\",True],1)], [((0,2),[(s, \"<\", \"wins' (Brave' Frank')\",\"DE\",True)],2)] [((0,0),[(np/.np,\"Desig\",\"Brave'\",\"DE\",True)],0), ((1,0),[(np,\"Desig\",\"Frank'\",\"DE\",True)],1), ((2,0),[(s\\*np,\"Desig\"),\"wins'\",\"DE\",True],2), ((0,1),[(np,\">\",\"Brave' Frank'\",\"DE\",True)],1), ((1,1),[(s,\"<\",\"wins' Frank'\",\"DE\",True)],2), ((0,2),[np,\"Np/s->\",\"Brave' (wins' Frank')\",\"DE\",True)],1), ((0,2),[(s,\"<\",\"wins' (Brave' Frank')\",\"DE\",True)],2)] is [[((1,0),[(np, \"Desig\",\"Frank'\",\"DE\",True)],1), ((2,0),[(s\\*np, \"Desig\",\"wins'\",\"DE\",True)],2), ((0,0),[(np/.np, \"Desig\",\"Brave'\",\"DE\",True)],0), ((1,1),[(s, \"<\",\"wins' Frank'\",\"DE\",True)],2), ((0,2),[(np,\"Np/s->\",\"Brave' (wins' Frank')\",\"DE\",True)],1)], [((0,0),[(np/.np, \"Desig\",\"Brave'\",\"DE\",True)],0), ((1,0),[(np, \"Desig\",\"Frank'\",\"DE\",True)],1), ((0,1),[(np, \">\",\"Brave' Frank'\",\"DE\",True)],1), ((2,0),[(s\\*np, \"Desig\",\"wins'\",\"DE\",True)],2), ((0,2),[(s, \"<\",\"wins' (Brave' Frank')\",\"DE\",True)],2)" $ do
      let c01 = getCateFromString "np/.np"
      let c02 = npCate
      let c03 = getCateFromString "s\\*np"
      let c11 = npCate
      let c12 = sCate
      let c21 = npCate
      let c22 = sCate
      let pc01 = createPhraCate 0 0 c01 "Desig" "Brave'" "DE" True 0
      let pc02 = createPhraCate 1 0 c02 "Desig" "Frank'" "DE" True 1
      let pc03 = createPhraCate 2 0 c03 "Desig" "wins'" "DE" True 2
      let pc11 = createPhraCate 0 1 c11 ">" "Brave' Frank'" "AHn" True 1
      let pc12 = createPhraCate 1 1 c12 "<" "wins' Frank'" "SP" True 2
      let pc21 = createPhraCate 0 2 c21 "Np/s->" "Brave' (wins' Frank')" "AHn" True 1
      let pc22 = createPhraCate 0 2 c22 "<" "wins' (Brave' Frank')" "SP" True 2
      let pcClo = [pc01,pc02,pc03,pc11,pc12,pc21,pc22]
      growForest "+++++" [[pc21],[pc22]] pcClo `shouldBe` [[pc02,pc03,pc01,pc12,pc21],[pc01,pc02,pc11,pc03,pc22]]


