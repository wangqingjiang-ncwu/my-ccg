-- Copyright (c) 2019-2025 China University of Water Resources and Electric Power
-- All rights reserved.

module ParseSpec where

import Category
import Rule
import Phrase
import Parse
import Test.Hspec
import Utils

spec :: Spec
spec = do
  describe "Parse" $ do
    it "The result of cateComb [] ((0,0),[(np, \"Desig\", \"Frank'\",\"DE\",True)],0) ((1,0),[(s\\.np, \"Desig\", \"smiles\",\"DE\",True)],1) is ((0,1),[(s, \"<\", \"smiles' Frank'\",\"SP\",True)],1)" $ do
      let c1 = npCate
      let c2 = getCateFromString "s\\.np"
      let c3 = sCate
      let pc1 = createPhraCate 0 0 [(c1, "Desig", "Frank'", "DE", True)] 0
      let pc2 = createPhraCate 1 0 [(c2, "Desig", "smiles'", "DE", True)] 1
      let pc3 = createPhraCate 0 1 [(c3, "<", "(smiles' Frank')", "SP", True)] 1
      cateComb [] pc1 pc2 `shouldBe` pc3

    it "The result of cateComb [Caa] ((2,0),[(np/*np, \"Desig\", \"一百六十'\",\"DE\",True)],2) ((3,0),[(np/.np, \"Desig\", \"多\",\"DE\",True)],3) is ((2,1),[(np/*np, \"Ca/a-<\", \"多' 一百六十'\",\"HmC\",True)],3)" $ do
      let c1 = getCateFromString "np/*np"
      let c2 = getCateFromString "np/.np"
      let c3 = c1
      let pc1 = createPhraCate 2 0 [(c1, "Desig", "一百六十'", "DE", True)] 2
      let pc2 = createPhraCate 3 0 [(c2, "Desig", "多'", "DE", True)] 3
      let pc3 = createPhraCate 2 1 [(c3,"Ca/a-<", "(多' 一百六十')", "HmC", True)] 3
      cateComb [Caa] pc1 pc2 `shouldBe` pc3

    it "The result of cateComb [] ((0,0),[(np,\"Desig\",\"Frank'\",\"DE\",True)],0) ((1,0),[((s\\.np)/.np,\"Desig\",\"loves'\",\"DE\",True)],1) is ((0,1),[(s/.np,\">T->B\"),\"Frank' loves'\",\"OE\",True)],1)" $ do
      let c1 = npCate
      let c2 = getCateFromString "(s\\.np)/.np"
      let c3 = getCateFromString "s/.np"
      let pc1 = createPhraCate 0 0 [(c1,"Desig","Frank'", "DE", True)] 0
      let pc2 = createPhraCate 1 0 [(c2,"Desig","loves'", "DE", True)] 1
      let pc3 = createPhraCate 0 1 [(c3,">T->B", "((R Frank') loves')", "OE", True)] 1
      cateComb [] pc1 pc2 `shouldBe` pc3

    it "The result of cateComb [Ss] ((0,0),[(s,\"Desig\",\"Frank_cries'\",\"DE\",True)],0) ((1,0),[(s\\.np,\"Desig\",\"is_bad'\",\"DE\",True)],1) is ((0,1),[(s,\"S/s-<\",\"is_bad' Frank_cries'\",\"SP\",True)],1)" $ do
      let c1 = sCate
      let c2 = getCateFromString "s\\.np"
      let c3 = sCate
      let pc1 = createPhraCate 0 0 [(c1, "Desig", "Frank_cries'", "DE", True)] 0
      let pc2 = createPhraCate 1 0 [(c2, "Desig", "is_bad'", "DE", True)] 1
      let pc3 = createPhraCate 0 1 [(c3, "S/s-<", "(is_bad' Frank_cries')", "SP", True)] 1
      cateComb [Ss] pc1 pc2 `shouldBe` pc3

    it "The result of cateComb [Os] ((0,0),[((s\\.np)/.np,\"Desig\",\"loves'\",\"DE\",True)],0) ((1,0),[(s,\"Desig\",\"Frank_does_it'\",\"DE\",True)],1) is ((0,1),[(s\\.np,\"O/s->\",\"(loves' Frank_does_it')\",\"VO\",True)],1)" $ do
      let c1 = getCateFromString "(s\\.np)/.np"
      let c2 = sCate
      let c3 = getCateFromString "s\\.np"
      let pc1 = createPhraCate 0 0 [(c1, "Desig", "loves'", "DE", True)] 0
      let pc2 = createPhraCate 1 0 [(c2, "Desig", "Frank_does_it'", "DE", True)] 1
      let pc3 = createPhraCate 0 1 [(c3, "O/s->", "(loves' Frank_does_it')", "VO", True)] 1
      cateComb [Os] pc1 pc2 `shouldBe` pc3

    it "The result of cateComb [] ((0,0),[(np/.np,\"Desig\",\"Good'\",\"DE\",True)],0) ((1,1),[((s\\.np)/.np,\">B\",\"((B is_better_than') bad')\",\"DE\",True)],2) is ((0,2),[],1)" $ do
      let c1 = getCateFromString "np/.np"
      let c2 = getCateFromString "(s\\.np)/.np"
      let c31 = getCateFromString "s/.np"
      let pc1 = createPhraCate 0 0 [(c1, "Desig", "Good'", "DE", True)] 0
      let pc2 = createPhraCate 1 1 [(c2, ">B", "((B is_better_than') bad')", "DE", True)] 2
      cateComb [] pc1 pc2 `shouldBe` ((0,2),[],1)

    it "The result of cateComb [Sa] ((0,0),[(np/.np,\"Desig\",\"Good'\",\"DE\",True)],0) ((1,1),[((s\\.np)/.np,\">B\",\"((B is_better_than') bad')\",\"DE\",True)],2) is ((0,2),[(s/.np,\"S/a->T->B\",\"((R ((B is_better_than') bad')) Good')\",\"OE\",True)],1)" $ do
      let c1 = getCateFromString "np/.np"
      let c21 = getCateFromString "(s\\.np)/.np"
      let c31 = getCateFromString "s/.np"
      let pc1 = createPhraCate 0 0 [(c1, "Desig", "Good'", "DE", True)] 0
      let pc2 = createPhraCate 1 1 [(c21, ">B", "((B is_better_than') bad')", "DE", True)] 2
      let pc3 = createPhraCate 0 2 [(c31,"S/a->T->B","((R Good') ((B is_better_than') bad'))","OE",True)] 1
      cateComb [Sa] pc1 pc2 `shouldBe` pc3

    it "The result of [cateComb \"[]\" pc1 pc2 | pc1 <- trans, pc2 <- trans, pc1 /= pc2, (acOfCate pc1)!!0, (acOfCate pc2)!!0, pclt pc1 pc2] is [((0,1),[(s/.np,\">T->B\",\"loves' Mary'\",\"DE\",True)],1)，((-1,-1),[],-1), ((1,1),[(s\\.np,\">\",\"loves' Mary'\",\"DE\",True)],2)]" $ do
      let c01 = getCateFromString "np"
      let c02 = getCateFromString "(s\\.np)/.np"
      let c03 = getCateFromString "np"
      let c12 = getCateFromString "s\\.np"
      let c11 = getCateFromString "s/.np"
      let pc01 = createPhraCate 0 0 [(c01, "Desig", "Frank'", "DE", True)] 0
      let pc02 = createPhraCate 1 0 [(c02, "Desig", "loves'", "DE", True)] 1
      let pc03 = createPhraCate 2 0 [(c03, "Desig", "Mary'", "DE", True)] 2
      let pcs = [pc01, pc02, pc03]
      let pc12 = createPhraCate 1 1 [(c12, ">", "(loves' Mary')", "VO", True)] 2
      let pc11 = createPhraCate 0 1 [(c11, ">T->B", "((R Frank') loves')", "OE", True)] 1
      let null = ((-1,-1),[],-1)
      [cateComb [] pc1 pc2 | pc1 <- pcs, pc2 <- pcs, pc1 /= pc2, (acOfCate pc1)!!0, (acOfCate pc2)!!0, pclt pc1 pc2] `shouldBe` [pc11,null,pc12]

    it "The result of getOverType [] ((0,1),[(np,\"AHn\",\"Frank's uncle'\",\"DE\",True)],1) ((1,2),[(s\\.np,\"Desig\",\"smiles'\",\"DE\",True)],2) is 2" $ do
      let c1 = npCate
      let c2 = getCateFromString "s\\.np"
      let pc1 = createPhraCate 0 1 [(c1,"AHn","Frank' uncle'", "DE", True)] 1
      let pc2 = createPhraCate 1 2 [(c2,"Desig","smiles'", "DE", True)] 2
      getOverType [] pc1 pc2 `shouldBe` 2

    it "The result of getOverlap [((0,1),[(np,\"Desig\",\"Frank'\",\"DE\",True)],1), ((1,1),[((s\\.np)/.np,\"Desig\",\"loves'\",\"DE\",True)],2)] is [(((0,1),[(np,\"Desig\",\"Frank'\",\"DE\",True)],1), ((1,1),[(s\\.np)/.np,\"Desig\",\"loves'\",\"DE\",True)],2))]" $ do
      let c1 = npCate
      let c2 = getCateFromString "(s\\.np)/.np"
      let pc1 = createPhraCate 0 1 [(c1,"Desig","Frank'", "DE", True)] 1
      let pc2 = createPhraCate 1 1 [(c2,"Desig","loves'", "DE", True)] 2
      let pcs = [pc1,pc2]
      let res = [(pc1,pc2)]
      getOverlap pcs `shouldBe` res

    it "The result of getOverlap [((11,3),[(np,\"N/v-<\",\"(或' (以上' 及格')) 及格'\",\"XX\",True)], 12), ((11,3),[(np,\"N/v-<\",\"(或' (以上' 及格')) 及格'\",\"XX\",True)], 12)] is []" $ do
      let pc1 = createPhraCate 11 3 [(npCate,"N/v-<","(或' (以上' 及格')) 及格'","XX",True)] 12
      let pc2 = createPhraCate 11 3 [(npCate,"N/v-<","(或' (以上' 及格')) 及格'","XX",True)] 12
      getOverlap [pc1, pc2] `shouldBe` []

    it "The result of findSplitCate ((0,1),[(s\\.np,\">T->B\",\"((R Frank') loves')\",\"OE\",True)],1) [((0,0),[(np,\"Desig\",\"Frank'\")],0), ((1,0),[((s\\.np)/.np,\"Desig\",\"loves'\")],1), ((2,0),[(np,\"Desig\",\"Mary', True)],2), ((0,1),[(s/.np,\">T->B\",\"((R Frank') loves')\")],1), ((1,1),[(s\\.np,\">\",\"(loves' Mary')\")],2) is ((0,0),[(np,\"Desig\",\"Frank'\",\"DE\",True],0), ((1,0),[((s\\.np)/.np,\"Desig\",\"loves'\")],1)]" $ do
      let c01 = getCateFromString "np"
      let c02 = getCateFromString "(s\\.np)/.np"
      let c03 = getCateFromString "np"
      let c11 = getCateFromString "s/.np"
      let c12 = getCateFromString "s\\.np"
      let pc01 = createPhraCate 0 0 [(c01, "Desig", "Frank'", "DE", True)] 0
      let pc02 = createPhraCate 1 0 [(c02, "Desig", "loves'", "DE", True)] 1
      let pc03 = createPhraCate 2 0 [(c03, "Desig", "Mary'", "DE", True)] 2
      let pc11 = createPhraCate 0 1 [(c11, ">T->B", "((R Frank') loves')", "OE", True)] 1
      let pc12 = createPhraCate 1 1 [(c12, ">", "(loves' Mary')", "VO", True)] 2
      let pcs = [pc01,pc02,pc03]
      let pcClo = pcs ++ [pc11,pc12]
      findSplitCate pc11 pcClo `shouldBe` [(pc01,pc02)]

    it "The result of updateAct [((0,0),[(np,\"Desig\",\"Frank'\",\"DE\",True)],0), ((1,0),[((s\\.np)/.np,\"Desig\",\"loves'\",\"DE\",True)],1)] is [((0,0),[(np,\"Desig\",\"Frank'\",\"DE\",True)],0), ((1,0),[((s\\.np)/.np,\"Desig\",\"loves'\",\"DE\",True)],1)]" $ do
      let c01 = npCate
      let c02 = getCateFromString "(s\\.np)/.np"
      let pc01 = createPhraCate 0 0 [(c01, "Desig", "Frank'", "DE", True)] 0
      let pc02 = createPhraCate 1 0 [(c02, "Desig", "loves'", "DE", True)] 1
      let pcs = [pc01,pc02]
      updateAct pcs `shouldBe` pcs

    it "The result of updateAct [((0,0),[(np,\"Desig\",\"Frank'\",\"DE\",True)],0), ((1,0),[((s\\.np)/.np,\"Desig\",\"loves'\",\"DE\",True)],1), ((2,0),[(np,\"Desig\",\"Mary'\",\"DE\",True)],2), ((0,1),[(s/.np,\">T->B\",\"((R Frank') loves')\",\"OE\",True)],1), ((1,1),[(s\\.np,\">\",\"(loves' Mary')\",\"DE\",True)],2)] is [((0,0),[(np,\"Desig\",\"Frank'\",\"DE\",True)],0), ((1,0),[((s\\.np)/.np,\"Desig\",\"loves'\",\"DE\",False],1), ((2,0),[(np,\"Desig\",\"Mary'\",\"DE\",False],2), ((0,1),[(s/.np,\">T->B\",\"((R Frank') loves')\",\"OE\",True)],2), ((1,1),[(s\\.np,\">\",\"(loves' Mary')\",\"DE\",True)],2)]" $ do
      let c01 = npCate
      let c02 = getCateFromString "(s\\.np)/.np"
      let c03 = npCate
      let c11 = getCateFromString "s/.np"
      let c12 = getCateFromString "s\\.np"
      let pc01 = createPhraCate 0 0 [(c01, "Desig", "Frank'", "DE", True)] 0
      let pc02 = createPhraCate 1 0 [(c02, "Desig", "loves'", "DE", True)] 1
      let pc03 = createPhraCate 2 0 [(c03, "Desig", "Mary'", "DE", True)] 2
      let pc11 = createPhraCate 0 1 [(c11, ">T->B", "((R Frank') loves')", "OE", True)] 1
      let pc12 = createPhraCate 1 1 [(c12, ">", "(loves' Mary')", "VO", True)] 2
      let pc010 = deactOnePC pc01
      let pc020 = deactOnePC pc02
      let pc030 = deactOnePC pc03
      let pcs = [pc01,pc02,pc03]
      let pcClo = pcs ++ [pc11,pc12]
      let pcClo0 = [pc010,pc020,pc030,pc11,pc12]
      updateAct pcClo `shouldBe` pcClo0

    it "The result of getOverlap [((0,0),[(np,\"Desig\",\"Frank'\",\"DE\",True)],0), ((1,0),[(s\\.np)/.np,\"Desig\",\"loves'\",\"DE\",True)],1), ((0,1),[(s/.np,\">T->B\",\"Frank' loves'\",\"DE\",True)],1), ((1,1),[(s\\.np,\"<\",\"loves' Mary'\",\"DE\",True)],2)] is [(((0,1),[(s/.np,\">T->B\",\"Frank' loves'\",\"DE\",True)],1), ((1,1),[(s\\.np,\">\",\"loves' Mary'\",\"DE\",True)],2)]" $ do
      let c1 = npCate
      let c2 = getCateFromString "(s\\.np)/.np"
      let c3 = getCateFromString "s/.np"
      let c4 = getCateFromString "s\\.np"
      let pc1 = createPhraCate 0 0 [(c1, "Desig", "Frank'", "DE", True)] 0
      let pc2 = createPhraCate 1 0 [(c2, "Desig", "loves'", "DE", True)] 1
      let pc3 = createPhraCate 0 1 [(c3, ">T->B", "((R Frank') loves')", "OE", True)] 1
      let pc4 = createPhraCate 1 1 [(c4, ">", "(loves' Mary')", "VO", True)] 2
      let pcs = [pc1,pc2]
      let pcClo = pcs ++ [pc3,pc4]
      getOverlap pcClo `shouldBe` [(pc3,pc4)]

    it "The result of getOverlap [((0,0),[(np,\"Desig\",\"Frank'\",\"DE\",True)],0), ((0,0),[(np,\"Desig\",\"Frank'\",\"DE\",True)],0)] is []" $ do
        let pc1 = createPhraCate 0 0 [(npCate, "Desig", "Frank'", "DE", True)] 0
        let pc2 = createPhraCate 0 0 [(npCate, "Desig", "Frank'", "DE", True)] 0
        getOverlap [pc1,pc2] `shouldBe` []

    it "The result of findDescen ((0,1),[(s/.np,\">T->B\",\"((R Frank') loves')\",\"OE\",True)],1) [((0,0),[(np,\"Desig\",\"Frank'\",\"DE\",False],0), ((1,0),[((s\\.np)/.np,\"Desig\",\"loves'\",\"DE\",False],1), ((2,0),[(np,\"Desig\",\"Mary'\",\"DE\",False],2)，((0,1),[(s/.np,\">T->B\",\"((R Frank') loves')\",\"OE\",True), ((1,1),[(s\\.np,\">\",\"(loves' Mary')\",\"VO\",True)],2),((0,2),[(s,\">\",\"((loves' Mary') Frank')\",\"NR\",True)],2),((0,2),[(s,\">\",\"((loves' Mary') Frank')\",\"SP\",True)],1)] is []" $ do
      let c01 = npCate
      let c02 = getCateFromString "(s\\.np)/.np"
      let c03 = npCate
      let c11 = getCateFromString "s/.np"
      let c12 = getCateFromString "s\\.np"
      let c21 = sCate
      let pc01 = createPhraCate 0 0 [(c01, "Desig", "Frank'", "DE", False)] 0
      let pc02 = createPhraCate 1 0 [(c02, "Desig", "loves'", "DE", False)] 1
      let pc03 = createPhraCate 2 0 [(c03, "Desig", "Mary'", "DE", False)] 2
      let pc11 = createPhraCate 0 1 [(c11, ">T->B", "((R Frank') loves')", "OE", True)] 1
      let pc12 = createPhraCate 1 1 [(c12, ">", "(loves' Mary')", "VO", True)] 2
      let pc21 = createPhraCate 0 2 [(c21, ">", "((loves' Mary') Frank')", "NR", True)] 2
      let pc22 = createPhraCate 0 2 [(c21, "<", "((loves' Mary') Frank')", "SP", True)] 1
      let pcs = [pc01,pc02,pc03]
      let pcClo = pcs ++ [pc11,pc12,pc21,pc22]
      findDescen pc11 pcClo `shouldBe` []                                       -- Not recognized phrases, namely with syntactic structure 'NR', are not descendants of any phrase.

    it "The result of findDescen ((0,0),[(np,\"Desig\",\"Frank'\",\"DE\",False],0) [((0,0),[(np,\"Desig\",\"Frank'\",\"DE\",False],0), ((1,0),[((s\\.np)/.np,\"Desig\",\"loves'\",\"DE\",False],1), ((2,0),[(np,\"Desig\",\"Mary'\",\"DE\",False],2)，((0,1),[(s/.np,\">T->B\",\"((R Frank') loves')\",\"OE\",True), ((1,1),[(s\\.np,\">\",\"(loves' Mary')\",\"VO\",True)],2),((0,2),[(s,\">\",\"((loves' Mary') Frank')\",\"NR\",True)],2),((0,2),[(s,\">\",\"((loves' Mary') Frank')\",\"SP\",True)],1)] is [((0,1),[(s/.np,\">T->B\",\"((R Frank') loves')\",\"OE\",True), ((0,2),[(s,\"<\",\"((loves' Mary') Frank')\",\"SP\",True)],2)]" $ do
      let c01 = npCate
      let c02 = getCateFromString "(s\\.np)/.np"
      let c03 = npCate
      let c11 = getCateFromString "s/.np"
      let c12 = getCateFromString "s\\.np"
      let c21 = sCate
      let pc01 = createPhraCate 0 0 [(c01, "Desig", "Frank'", "DE", False)] 0
      let pc02 = createPhraCate 1 0 [(c02, "Desig", "loves'", "DE", False)] 1
      let pc03 = createPhraCate 2 0 [(c03, "Desig", "Mary'", "DE", False)] 2
      let pc11 = createPhraCate 0 1 [(c11, ">T->B", "((R Frank') loves')", "OE", True)] 1
      let pc12 = createPhraCate 1 1 [(c12, ">", "(loves' Mary')", "VO", True)] 2
      let pc21 = createPhraCate 0 2 [(c21, ">", "((loves' Mary') Frank')", "NR", True)] 2
      let pc22 = createPhraCate 0 2 [(c21, "<", "((loves' Mary') Frank')", "SP", True)] 1
      let pcs = [pc01,pc02,pc03]
      let pcClo = pcs ++ [pc11,pc12,pc21,pc22]
      let pcClo0 = [pc21]
      findDescen pc01 pcClo `shouldBe` [pc11,pc22]                              -- Phrase pc21 has syntactic structure 'NR', which is not a descendant of any phrase.

    it "The result of removeOnePC ((0,0),[(np,\"Desig\",\"Frank'\",\"DE\",False],0) [((0,0),[(np,\"Desig\",\"Frank'\",\"DE\",False],0), ((1,0),[((s\\.np)/.np,\"Desig\",\"loves'\",\"DE\",False],1), ((2,0),[(np,\"Desig\",\"Mary'\",\"DE\",False],2), ((0,1),[(s/.np,\">T->B\",\"((R Frank') loves')\",\"OE\",True), ((1,1),[(s\\.np,\">\",\"(loves' Mary')\",\"VO\",True)],2), ((0,2),[(s,\">\",\"((loves' Mary') Frank')\",\"SP\",True)],1)] is [((1,0),[((s\\.np)/.np,\"Desig\",\"loves'\",\"DE\",False],1), ((2,0),[(np,\"Desig\",\"Mary'\",\"DE\",False],2), ((1,1),[(s\\.np,\">\",\"(loves' Mary')\",\"VO\",True)],2)]" $ do
      let c01 = npCate
      let c02 = getCateFromString "(s\\.np)/.np"
      let c03 = npCate
      let c11 = getCateFromString "s/.np"
      let c12 = getCateFromString "s\\.np"
      let c21 = sCate
      let pc01 = createPhraCate 0 0 [(c01, "Desig", "Frank'", "DE", False)] 0
      let pc02 = createPhraCate 1 0 [(c02, "Desig", "loves'", "DE", False)] 1
      let pc03 = createPhraCate 2 0 [(c03, "Desig", "Mary'", "DE", False)] 2
      let pc11 = createPhraCate 0 1 [(c11, ">T->B", "((R Frank') loves')", "OE", True)] 1
      let pc12 = createPhraCate 1 1 [(c12, ">", "(loves' Mary')", "VO", True)] 2
      let pc21 = createPhraCate 0 2 [(c21, "<", "((loves' Mary') Frank')", "SP", True)] 1
      let pcs = [pc01,pc02,pc03]
      let pcClo = pcs ++ [pc11,pc12,pc21]
      let pcClo0 = [pc02,pc03,pc12]
      removeOnePC pc01 pcClo [] `shouldBe` pcClo0

    it "The result of trans [] [((0,0),[(np,\"Desig\",\"Frank'\",\"DE\",True)],0), ((1,0),[((s\\.np)/.np,\"Desig\",\"loves'\",\"DE\",True)],1), ((2,0),[(np,\"Desig\",\"Mary'\",\"DE\",True)],2)] [] is [((1,0),[((s\\.np)/.np,\"Desig\",\"loves'\",\"DE\",False],1), ((2,0),[(np,\"Desig\",\"Mary'\",\"DE\",False],2), ((0,0),[(np,\"Desig\",\"Frank'\",\"DE\",True)],0), ((1,1),[(s\\.np,\">\",\"(loves' Mary')\",\"DE\",True)],2)]" $ do
      let c01 = npCate
      let c02 = getCateFromString "(s\\.np)/.np"
      let c03 = npCate
      let c11 = getCateFromString "s/.np"
      let c12 = getCateFromString "s\\.np"
      let pc01 = createPhraCate 0 0 [(c01, "Desig", "Frank'", "DE", True)] 0
      let pc02 = createPhraCate 1 0 [(c02, "Desig", "loves'", "DE", True)] 1
      let pc03 = createPhraCate 2 0 [(c03, "Desig", "Mary'", "DE", True)] 2
      let pc11 = createPhraCate 0 1 [(c11, ">T->B", "((R Frank') loves')", "OE", True)] 1
      let pc12 = createPhraCate 1 1 [(c12, ">", "(loves' Mary')", "VO", True)] 2
      let pcs = [pc01,pc02,pc03]
--      let pc010 = deactOnePC pc01
--      let pc020 = deactOnePC pc02
--      let pc030 = deactOnePC pc03
      let pcClo = [pc01,pc02,pc03,pc11,pc12]
      trans [] pcs [] `shouldBe` pcClo

    it "The result of trans [] [((0,0),[((s\\.np)/.np,\"Desig\",\"贪污'\",\"DE\",True)],0), ((1,0),[(np,\"Desig\",\"现金'\",\"DE\",True)],1), ((2,0),[(np/*np,\"Desig\",\"一百六十'\",\"DE\",True)],2),((3,0),[(np/.np,\"Desig\",\"多'\",DE,True)],3),((4,0),[((np/*np)\\*(np/*np),\"Desig\",\"元'\",DE,True)],4)] [] is [((0,0),[((s\\.np)/.np,\"Desig\",\"贪污'\",\"DE\",False)],0), ((1,0),[(np,\"Desig\",\"现金'\",\"DE\",False)],1), ((2,0),[(np/*np,\"Desig\",\"一百六十'\",\"DE\",False)],2),((3,0),[(np/.np,\"Desig\",\"多'\",DE,False)],3),((4,0),[((np/*np)\\*(np/*np),\"Desig\",\"元'\",DE,True),((0,1),[(s\\.np,\">\",\"(贪污' 现金')\",\"VO\",True)],1),((2,1),[(np/*np,\"Ca/a-<\",\"(多' 一百六十')\",\"HmC\",True)],3)],4),((3,1),[(np,\"<\",\"(多' 元')\",\"PQ\",True)],4)]" $ do
      let c01 = getCateFromString "(s\\.np)/.np"
      let c02 = npCate
      let c03 = getCateFromString "np/*np"
      let c04 = getCateFromString "np/.np"
      let c05 = getCateFromString "(np/*np)\\*(np/*np)"
      let c11 = getCateFromString "s\\.np"
      let c12 = getCateFromString "np/*np"
      let c22 = getCateFromString "np/*np"
      let pc01 = createPhraCate 0 0 [(c01, "Desig", "贪污'", "DE", True)] 0
      let pc02 = createPhraCate 1 0 [(c02, "Desig", "现金'", "DE", True)] 1
      let pc03 = createPhraCate 2 0 [(c03, "Desig", "一百六十'", "DE", True)] 2
      let pc04 = createPhraCate 3 0 [(c04, "Desig", "多'", "DE", True)] 3
      let pc05 = createPhraCate 4 0 [(c05, "Desig", "元'", "DE", True)] 4
      let pc11 = createPhraCate 0 1 [(c11,">","(贪污' 现金')","VO",True)] 1
      let pc12 = createPhraCate 2 1 [(c12,"Ca/a-<","(多' 一百六十')","HmC",True)] 3
      let pc21 = createPhraCate 3 1 [(c22,"<","(元' 多')","PQ",True)] 4
      let pcs = [pc01,pc02,pc03,pc04,pc05]
      let pcClo = [pc01,pc02,pc03,pc04,pc05,pc11,pc12,pc21]
      trans [Caa] pcs [] `shouldBe` pcClo

    it "The result of findSplitCate ((1,0),[(np,\"Desig\",\"Frank'\",\"DE\",True)],1) [((0,0),[(np/.np,\"Desig\",\"Brave'\",\"DE\",True))],0), ((1,0),[(np,\"Desig\",\"Frank'\",\"DE\",True))],1), ((2,0),[(s\\*np,\"Desig\",\"wins'\",\"DE\",True)],2), ((0,1),[(np,\">\",\"(Brave' Frank')\",\"AHn\",True))],1), ((1,1),[(s,\"<\",\"(wins' Frank')\",\"SP\",True))],2), ((0,2),[np,\"N/s->\",\"(Brave' (wins' Frank'))\",\"AHn\",True)],1), ((0,2),[(s,\"<\",\"(wins' (Brave' Frank'))\",\"SP\",True))],2)] is []" $ do
      let c01 = getCateFromString "np/.np"
      let c02 = npCate
      let c03 = getCateFromString "s\\*np"
      let c11 = npCate
      let c12 = sCate
      let c21 = npCate
      let c22 = sCate
      let pc01 = createPhraCate 0 0 [(c01, "Desig", "Brave'", "DE", True)] 0
      let pc02 = createPhraCate 1 0 [(c02, "Desig", "Frank'", "DE", True)] 1
      let pc03 = createPhraCate 2 0 [(c03, "Desig", "wins'", "DE", True)] 2
      let pc11 = createPhraCate 0 1 [(c11, ">", "(Brave' Frank')", "AHn", True)] 1
      let pc12 = createPhraCate 1 1 [(c12, "<", "(wins' Frank')", "SP", True)] 2
      let pc21 = createPhraCate 0 2 [(c21, "N/s->", "Brave' (wins' Frank')", "AHn", True)] 1
      let pc22 = createPhraCate 0 2 [(c22, "<", "wins' (Brave' Frank')", "SP", True)] 2
      let pcs = [pc01,pc02,pc03]
      let pcClo = pcs ++ [pc11,pc12,pc21,pc22]
      findSplitCate pc02 pcClo `shouldBe` []

    it "The result of findSplitCate ((0,2),[(s,\"<\",\"(wins' (Brave' Frank'))\",\"SP\",True)],2) [((0,0),[(np/.np,\"Desig\",\"Brave'\",\"DE\",False))],0), ((1,0),[(np,\"Desig\",\"Frank'\",\"DE\",False))],1), ((2,0),[(s\\*np,\"Desig\",\"wins'\",\"DE\",False)],2), ((0,1),[(np,\">\",\"(Brave' Frank')\",\"AHn\",True))],1), ((1,1),[(s,\"<\",\"(wins' Frank')\",\"SP\",True))],2), ((0,2),[np,\"N/s->\",\"(Brave' (wins' Frank'))\",\"AHn\",True)],1), ((0,2),[(s,\"<\",\"(wins' (Brave' Frank'))\",\"SP\",True))],2)] is [((0,1),[(np,\">\",\"(Brave' Frank')\",\"AHn\",True))],1), ((2,0),[(s\\*np,\"Desig\",\"wins'\",\"DE\",True)],2)]" $ do
      let c01 = getCateFromString "np/.np"
      let c02 = npCate
      let c03 = getCateFromString "s\\*np"
      let c11 = npCate
      let c12 = sCate
      let c21 = npCate
      let c22 = sCate
      let pc01 = createPhraCate 0 0 [(c01, "Desig", "Brave'", "DE", False)] 0
      let pc02 = createPhraCate 1 0 [(c02, "Desig", "Frank'", "DE", False)] 1
      let pc03 = createPhraCate 2 0 [(c03, "Desig", "wins'", "DE", False)] 2
      let pc11 = createPhraCate 0 1 [(c11, ">", "(Brave' Frank')", "AHn", True)] 1
      let pc12 = createPhraCate 1 1 [(c12, "<", "(wins' Frank')", "SP", True)] 2
      let pc21 = createPhraCate 0 2 [(c21, "N/s->", "(Brave' (wins' Frank'))", "AHn", True)] 1
      let pc22 = createPhraCate 0 2 [(c22, "<", "(wins' (Brave' Frank'))", "SP", True)] 2
      let pcs = [pc01,pc02,pc03]
      let pcClo = pcs ++ [pc11,pc12,pc21,pc22]
      findSplitCate pc22 pcClo `shouldBe` [(pc11,pc03)]

    it "The result of findSplitCate ((1,1),[(s,\"<\",\"(wins' Frank')\",\"SP\",True))],2) [((0,0),[(np/.np,\"Desig\",\"Brave'\",\"DE\",False))],0), ((1,0),[(np,\"Desig\",\"Frank'\",\"DE\",False))],1), ((2,0),[(s\\*np,\"Desig\",\"wins'\",\"DE\",False)],2), ((0,1),[(np,\">\",\"(Brave' Frank')\",\"AHn\",True))],1), ((1,1),[(s,\"<\",\"(wins' Frank')\",\"SP\",True))],2), ((0,2),[np,\"N/s->\",\"(Brave' (wins' Frank'))\",\"AHn\",True)],1), ((0,2),[(s,\"<\",\"(wins' (Brave' Frank'))\",\"SP\",True))],2)] is [((1,0),[(np,\"Desig\",\"Frank'\",\"DE\",False))],1), ((2,0),[(s\\*np,\"Desig\",\"wins'\",\"DE\",False)],2)]" $ do
      let c01 = getCateFromString "np/.np"
      let c02 = npCate
      let c03 = getCateFromString "s\\*np"
      let c11 = npCate
      let c12 = sCate
      let c21 = npCate
      let c22 = sCate
      let pc01 = createPhraCate 0 0 [(c01, "Desig", "Brave'", "DE", False)] 0
      let pc02 = createPhraCate 1 0 [(c02, "Desig", "Frank'", "DE", False)] 1
      let pc03 = createPhraCate 2 0 [(c03, "Desig", "wins'", "DE", False)] 2
      let pc11 = createPhraCate 0 1 [(c11, ">", "(Brave' Frank')", "AHn", True)] 1
      let pc12 = createPhraCate 1 1 [(c12, "<", "(wins' Frank')", "SP", True)] 2
      let pc21 = createPhraCate 0 2 [(c21, "N/s->", "(Brave' (wins' Frank'))", "AHn", True)] 1
      let pc22 = createPhraCate 0 2 [(c22, "<", "(wins' (Brave' Frank'))", "SP", True)] 2
      let pcs = [pc01,pc02,pc03]
      let pcClo = pcs ++ [pc11,pc12,pc21,pc22]
      findSplitCate pc12 pcClo `shouldBe` [(pc02,pc03)]

    it "The result of findSplitCate ((0,2),[(s,\"S/a-<\",\"((战胜' 怯懦') 勇猛')\",\"SP\",True],1)  [((0,0),[(np/.np,\"Desig\",\"勇猛'\",\"DE\",False],0), ((1,0),[((s\\.np)/.np,\"Desig\",\"战胜'\",\"DE\",False],1), ((2,0),[(np/.np,\"Desig\",\"怯懦'\",\"DE\",False],2), ((1,1),[(s\\.np,\"O/a->\",\"(战胜' 怯懦')\",\"VO\",False)],2), ((0,2),[(s,\"S/a-<\",\"((战胜' 怯懦') 勇猛')\",\"SP\",True)],1) is [(((0,0),[(np/.np,\"Desig\",\"勇猛'\",\"DE\",False),0),((1,1),[(s\\.np,\"O/a->\",\"(战胜' 怯懦')\",\"VO\",False],2)]" $ do
      let c01 = getCateFromString "np/.np"
      let c02 = getCateFromString "(s\\.np)/.np"
      let c03 = getCateFromString "np/.np"
      let c11 = getCateFromString "s\\.np"
      let c21 = sCate
      let pc01 = createPhraCate 0 0 [(c01, "Desig", "勇猛'", "DE", False)] 0
      let pc02 = createPhraCate 1 0 [(c02, "Desig", "战胜'", "DE", False)] 1
      let pc03 = createPhraCate 2 0 [(c03, "Desig", "怯懦'", "DE", False)] 2
      let pc11 = createPhraCate 1 1 [(c11, "O/a->", "(战胜' 怯懦')", "VO", False)] 2
      let pc21 = createPhraCate 0 2 [(c21, "S/a-<", "((战胜' 怯懦') 勇猛')", "SP", True)] 1
      let pcClo = [pc01,pc02,pc03,pc11,pc21]
      findSplitCate pc21 pcClo `shouldBe` [(pc01,pc11)]

    it "The result of findSplitCate ((17,1),[(s\\.np,Cv/a-<,(二十九' 分之)',HvC,True)],18) [((16,0),[(np/*np,Desig,百',DE,False)],16), ((17,0),[(s\\.np,Desig,分之',DE,False)],17), ((18,0),[(np/*np,Desig,二十九',DE,False)],18), ((17,1),[(s\\.np,Cv/a-<,二十九' 分之',HvC,True)],18)] is [((17,0),[(s\\.np,Desig,分之',DE,False)],17), ((18,0),[(np/*np,Desig,二十九',DE,False)],18)]" $ do
      let pc16 = createPhraCate 16 0 [(numeralCate, "Desig", "百'", "DE", False)] 16
      let pc17 = createPhraCate 17 0 [(predCate, "Desig", "分之'", "DE", False)] 17
      let pc18 = createPhraCate 18 0 [(numeralCate, "Desig", "二十九'", "DE", False)] 18
      let pc17_1 = createPhraCate 17 1 [(predCate, "Cv/a-<", "(二十九' 分之')", "HvC", True)] 18
      let pcClo = [pc16,pc17,pc18,pc17_1]
      findSplitCate pc17_1 pcClo `shouldBe` [(pc17,pc18)]

    it "The result of findSplitCate ((14,4),[(s\\.np,Cv/d-<,((到' ((二十九' 分之') 百')) 提高'),HvC,True)],15) [((14,0),[(s\\.np,Desig,提高',DE,False)],14), ((15,0),[(((s\\.np)/#(s\\.np))/*np,Desig,到',DE,False)],15), ((16,0),[(np/*np,Desig,百',DE,False)],16), ((17,0),[(s\\.np,Desig,分之',DE,False)],17), ((18,0),[(np/*np,Desig,二十九',DE,False)],18), ((17,1),[(s\\.np,Cv/a-<,(二十九' 分之'),HvC,False)],18), ((16,2),[(s,S/a-<,((二十九' 分之') 百'),SP,False)],17), ((15,3),[((s\\.np)/#(s\\.np),N/s->,到' ((二十九' 分之') 百'),PO,True)],16), ((14,4),[(s\\.np,Cv/d-<,((到' ((二十九' 分之') 百')) 提高'),HvC,True)],15)] is [((14,0),[(s\\.np,Desig,提高',DE,False)],14), ((15,3),[((s\\.np)/#(s\\.np),N/s->,(到' ((二十九' 分之') 百')),PO,True)],16)]" $ do
      let pc14 = createPhraCate 14 0 [(predCate, "Desig", "提高'", "DE", False)] 14
      let pc15 = createPhraCate 15 0 [(prep2AdvCate, "Desig", "到'", "DE", False)] 15
      let pc16 = createPhraCate 16 0 [(numeralCate, "Desig", "百'", "DE", False)] 16
      let pc17 = createPhraCate 17 0 [(predCate, "Desig", "分之'", "DE", False)] 17
      let pc18 = createPhraCate 18 0 [(numeralCate, "Desig", "二十九'", "DE", False)] 18
      let pc17_1 = createPhraCate 17 1 [(predCate, "Cv/a-<", "(二十九' 分之')", "HvC", False)] 18
      let pc16_2 = createPhraCate 16 2 [(sCate, "S/a-<", "((二十九' 分之') 百')", "SP", False)] 17
      let pc15_3 = createPhraCate 15 3 [(advCate, "N/s->", "(到' ((二十九' 分之') 百'))", "PO", True)] 16
      let pc14_4 = createPhraCate 14 4 [(predCate, "Cv/d-<", "((到' ((二十九' 分之') 百')) 提高')", "HvC", True)] 15
      let pcClo = [pc14,pc15,pc16,pc17,pc17_1,pc16_2,pc15_3,pc14_4]
      findSplitCate pc14_4 pcClo `shouldBe` [(pc14,pc15_3)]

    it "The result of findDescen ((15,3),[((s\\.np)/#(s\\.np),N/s->,到' ((二十九' 分之') 百'),PO,True)],16) [((14,0),[(s\\.np,Desig,提高',DE,False)],14), ((15,0),[(((s\\.np)/#(s\\.np))/*np,Desig,到',DE,False)],15), ((16,0),[(np/*np,Desig,百',DE,False)],16), ((17,0),[(s\\.np,Desig,分之',DE,False)],17), ((18,0),[(np/*np,Desig,二十九',DE,False)],18), ((17,1),[(s\\.np,Cv/a-<,(二十九' 分之'),HvC,False)],18), ((16,2),[(s,S/a-<,((二十九' 分之') 百'),SP,False)],17), ((15,3),[((s\\.np)/#(s\\.np),N/s->,(到' ((二十九' 分之') 百')),PO,True)],16), ((14,4),[(s\\.np,Cv/d-<,((到' ((二十九' 分之') 百')) 提高'),HvC,True)],15)] is ((14,4),[(s\\.np,Cv/d-<,((到' ((二十九' 分之') 百')) 提高'),HvC,True)],15)"  $ do
      let pc14 = createPhraCate 14 0 [(predCate, "Desig", "提高'", "DE", False)] 14
      let pc15 = createPhraCate 15 0 [(prep2AdvCate, "Desig", "到'", "DE", False)] 15
      let pc16 = createPhraCate 16 0 [(numeralCate, "Desig", "百'", "DE", False)] 16
      let pc17 = createPhraCate 17 0 [(predCate, "Desig", "分之'", "DE", False)] 17
      let pc18 = createPhraCate 18 0 [(numeralCate, "Desig", "二十九'", "DE", False)] 18
      let pc17_1 = createPhraCate 17 1 [(predCate, "Cv/a-<", "(二十九' 分之')", "HvC", False)] 18
      let pc16_2 = createPhraCate 16 2 [(sCate, "S/a-<", "((二十九' 分之') 百')", "SP", False)] 17
      let pc15_3 = createPhraCate 15 3 [(advCate, "N/s->", "(到' ((二十九' 分之') 百'))", "PO", True)] 16
      let pc14_4 = createPhraCate 14 4 [(predCate, "Cv/d-<", "((到' ((二十九' 分之') 百')) 提高')", "HvC", True)] 15
      let pcClo = [pc14,pc15,pc16,pc17,pc17_1,pc16_2,pc15_3,pc14_4]
      findDescen pc15_3 pcClo `shouldBe` [pc14_4]
