-- Copyright China University of Water Resources and Electric Power (c) 2019
-- All rights reserved.

module CorpusSpec where

import Category
import Phrase
import Corpus
import Test.Hspec
import Utils

spec :: Spec
spec = do
  describe "Corpus" $ do
    it "The result of getTreeDepth [((0,0),[(np/.np,\"Desig\",\"Brave'\",\"DE\",True)],0), ((1,0),[(np,\"Desig\",\"Frank'\",\"DE\",True)],1), ((2,0),[(s\\*np,\"Desig\"),\"wins'\",\"DE\",True)],2), ((0,1),[(np,\">\",\"Brave' Frank'\",\"DE\",True)],1), ((0,2),[(s,\"<\",\"wins' (Brave' Frank')\",\"DE\",True)],2)] is 3" $ do
      let c0 = getCateFromString "np/.np"
      let c1 = npCate
      let c2 = getCateFromString "s\\*np"
      let c01 = npCate
      let c02 = sCate
      let pc0 = createPhraCate 0 0 [(c0, "Desig", "Brave'", "DE", False)] 0
      let pc1 = createPhraCate 1 0 [(c1, "Desig", "Frank'", "DE", False)] 1
      let pc2 = createPhraCate 2 0 [(c2, "Desig", "wins'", "DE", False)] 2
      let pc01 = createPhraCate 0 1 [(c01, ">", "Brave' Frank'", "AHn", False)] 1
      let pc02 = createPhraCate 0 2 [(c02, "<", "wins' (Brave' Frank')", "SP", True)] 2
      let pcClo = [pc0,pc1,pc2,pc01,pc02]
      getTreeDepth pcClo `shouldBe` 3

    it "The result of getTreeDepth [((0,0),[(np/.np,\"Desig\",\"Brave'\",\"DE\",True)],0), ((1,0),[(np,\"Desig\",\"Frank'\",\"DE\",True)],1), ((2,0),[(s\\*np,\"Desig\"),\"wins'\",\"DE\",True)],2), ((0,1),[(np,\">\",\"Brave' Frank'\",\"DE\",True)],1)] is -1" $ do
      let c0 = getCateFromString "np/.np"
      let c1 = npCate
      let c2 = getCateFromString "s\\*np"
      let c01 = npCate
      let pc0 = createPhraCate 0 0 [(c0, "Desig", "Brave'", "DE", False)] 0
      let pc1 = createPhraCate 1 0 [(c1, "Desig", "Frank'", "DE", False)] 1
      let pc2 = createPhraCate 2 0 [(c2, "Desig", "wins'", "DE", False)] 2
      let pc01 = createPhraCate 0 1 [(c01, ">", "Brave' Frank'", "AHn", False)] 1
      let pcClo = [pc0,pc1,pc2,pc01]
      getTreeDepth pcClo `shouldBe` -1

    it "The result of getTreeDepth [((0,0),[(np/.np,\"Desig\",\"Brave'\",\"DE\",True)],0), ((1,0),[(np,\"Desig\",\"Frank'\",\"DE\",True)],1), ((0,1),[(np,\">\",\"Brave' Frank'\",\"DE\",True)],1)] is 2" $ do
      let c0 = getCateFromString "np/.np"
      let c1 = npCate
      let c01 = npCate
      let pc0 = createPhraCate 0 0 [(c0, "Desig", "Brave'", "DE", False)] 0
      let pc1 = createPhraCate 1 0 [(c1, "Desig", "Frank'", "DE", False)] 1
      let pc01 = createPhraCate 0 1 [(c01, ">", "Brave' Frank'", "AHn", False)] 1
      let pcClo = [pc0,pc1,pc01]
      getTreeDepth pcClo `shouldBe` 2

    it "The result of getTreeDepth [((0,0),[(np/.np,\"Desig\",\"Brave'\",\"DE\",True)],0)] is 1" $ do
      let c0 = getCateFromString "np/.np"
      let pc0 = createPhraCate 0 0 [(c0, "Desig", "Brave'", "DE", False)] 0
      getTreeDepth [pc0] `shouldBe` 1

    it "The result of getTreeDepth [] is 0" $ do
      getTreeDepth [] `shouldBe` 0

    it "The result of leavesNum for [((0,0),[(np/.np,\"Desig\",\"Brave'\",\"DE\",True)],0), ((1,0),[(np,\"Desig\",\"Frank'\",\"DE\",True)],1), ((2,0),[(s\\*np,\"Desig\"),\"wins'\",\"DE\",True)],2), ((0,1),[(np,\">\",\"Brave' Frank'\",\"DE\",True)],1), ((0,2),[(s,\"<\",\"wins' (Brave' Frank')\",\"DE\",True)],2)] is 3" $ do
        let c0 = getCateFromString "np/.np"
        let c1 = npCate
        let c2 = getCateFromString "s\\*np"
        let c01 = npCate
        let c02 = sCate
        let pc0 = createPhraCate 0 0 [(c0, "Desig", "Brave'", "DE", False)] 0
        let pc1 = createPhraCate 1 0 [(c1, "Desig", "Frank'", "DE", False)] 1
        let pc2 = createPhraCate 2 0 [(c2, "Desig", "wins'", "DE", False)] 2
        let pc01 = createPhraCate 0 1 [(c01, ">", "Brave' Frank'", "AHn", False)] 1
        let pc02 = createPhraCate 0 2 [(c02, "<", "wins' (Brave' Frank')", "SP", True)] 2
        let pcClo = [pc0,pc1,pc2,pc01,pc02]
        length [x | x <- pcClo, spOfCate x == 0] `shouldBe` 3

    it "The secStart result of root for [((0,0),[(np/.np,\"Desig\",\"Brave'\",\"DE\",True)],0), ((1,0),[(np,\"Desig\",\"Frank'\",\"DE\",True)],1), ((2,0),[(s\\*np,\"Desig\"),\"wins'\",\"DE\",True)],2), ((0,1),[(np,\">\",\"Brave' Frank'\",\"DE\",True)],1), ((0,2),[(s,\"<\",\"wins' (Brave' Frank')\",\"DE\",True)],2)] is 2" $ do
        let c0 = getCateFromString "np/.np"
        let c1 = npCate
        let c2 = getCateFromString "s\\*np"
        let c01 = npCate
        let c02 = sCate
        let pc0 = createPhraCate 0 0 [(c0, "Desig", "Brave'", "DE", False)] 0
        let pc1 = createPhraCate 1 0 [(c1, "Desig", "Frank'", "DE", False)] 1
        let pc2 = createPhraCate 2 0 [(c2, "Desig", "wins'", "DE", False)] 2
        let pc01 = createPhraCate 0 1 [(c01, ">", "Brave' Frank'", "AHn", False)] 1
        let pc02 = createPhraCate 0 2 [(c02, "<", "wins' (Brave' Frank')", "SP", True)] 2
        let pcClo = [pc0,pc1,pc2,pc01,pc02]
        ssOfCate ([x | x <- pcClo, spOfCate x == 2]!!0) `shouldBe` 2

    it "The treeToString result of (0,[((0,0),[(np/.np,\"Desig\",\"Brave'\",\"DE\",True)],0)]) is (0,[((0,0),[(np/.np,\"Desig\",\"Brave'\",\"DE\",True)],0)])" $ do
        let c0 = getCateFromString "np/.np"
        let pc0 = createPhraCate 0 0 [(c0, "Desig", "Brave'", "DE", True)] 0
        let tree = (1,[pc0])
        treeToString tree `shouldBe` "(1,[((0,0),[(np/.np,Desig,Brave',DE,True)],0)])"
