-- Copyright (c) 2019-2025 China University of Water Resources and Electric Power,
-- All rights reserved.

module ClusteringSpec where

import Category
import Phrase
import Clustering
import AmbiResol
import Test.Hspec
import qualified Data.Map as Map

spec :: Spec
spec = do
  describe "Clustering" $ do
    it "The result of distPhraSynByIdentity (np, \">\", \"AHn\", 1)) ((s\\.np)/.np, \"<B\", \"AHn\", 2) is 0.75" $ do
      let c1 = getCateFromString "np"
      let c2 = getCateFromString "(s\\.np)/.np"
      let t1 = ">"
      let t2 = "<B"
      let p1 = "AHn"
      let p2 = "AHn"
      let s1 = 1
      let s2 = 2
      distPhraSynByIdentity (c1,t1,p1,s1) (c2,t2,p2,s2) `shouldBe` (0.75 :: Double)

-- sim(p1,q1) = 0.5, sim(p1,q2) = 0.25, sim(p2,q1) = 0.0, sim(p2,q2) = 0.25. So sim([p1,p2],[q1,q2]) = (sim(p1,q1) + sim(p2,q2)) / 2.
    it "The result of distPhraSynSetByIdentity [(np, \">\", \"AHn\", 2), (s, \">\", \"DHv\", 3)] [(s\\.np, \"<B\", \"AHn\", 2) (np, \">B\", \"DHv\", 4)] is 0.375" $ do
      let p1 = (npCate, ">", "AHn", 2)
      let p2 = (sCate, ">", "DHv", 3)
      let q1 = (predCate, "<B", "AHn", 2)
      let q2 = (npCate, ">B", "DHv", 4)
      distPhraSynSetByIdentity [p1,p2] [q1,q2] `shouldBe` (0.375 :: Double)

    it "The result of distPhraSynSetByIdentity nPCs1 nPCs2 is 0.0" $ do
      let nPCs1 = getPhraCateListFromString $ "[((0,0),[(np,Desig,丁伟',DE,False)],0),((1,0),[((s\\.np)/.np,Desig,是',DE,False)],1),((2,0),[(np/*np,Desig,七二',DE,False)],2),((3,0),[(np,Desig,届',DE,False)],3),((4,0),[(np,Desig,学生',DE,True)],4),((0,1),[(s/.np,>T->B,是' 丁伟',OE,True)],1),((2,1),[(np,>,七二' 届',AHn,True)],3)]"
      let nPCs2 = getPhraCateListFromString $ "[((3,0),[(np,Desig,届',DE,False)],3),((0,1),[(s/.np,>T->B,是' 丁伟',OE,True)],1),((4,0),[(np,Desig,学生',DE,True)],4),((2,1),[(np,>,七二' 届',AHn,True)],3),((0,0),[(np,Desig,丁伟',DE,False)],0),((1,0),[((s\\.np)/.np,Desig,是',DE,False)],1),((2,0),[(np/*np,Desig,七二',DE,False)],2)]"
      let ctpsOfnPCs1 = ctpsOfCateList' nPCs1
      let ctpsOfnPCs2 = ctpsOfCateList' nPCs2
      distPhraSynSetByIdentity ctpsOfnPCs1 ctpsOfnPCs2 `shouldBe` (0.0 :: Double)

    it "The result of distPhraSynSetByIdentity sStub nPCs is 7.5 / 8.0" $ do
      let sStub = [(numeralCate, "Desig", "DE", 0), (npCate, "Desig", "DE", 0), (npCate, "Desig", "DE", 0), (npCate, ">", "AHn", 1), (numeralCate, "Desig", "DE", 0), (verbCate, "Desig", "DE", 0), (npCate, "A/n->", "AHn", 2)]
      let nPCs = [(verbCate, "Desig", "DE", 0), (numeralCate, "Desig", "DE", 0), (npCate, "Desig", "DE", 0), (numeralCate, "Desig", "DE", 0), (npCate, "Desig", "DE", 0), (npCate, ">", "AHn", 1), (npCate, ">", "AHn", 1)]
      abs (distPhraSynSetByIdentity sStub nPCs - (7.5 / 8.0 :: Double)) < 1e-10 `shouldBe` True

    it "The result of distVect4StruGeneByIdentity ([(np, \">\", \"HnC\", 2)], (s, \">B\", \"DHv\", 2), (np, \"<B\", \"AHn\", 2), [(np, \">B\", \"SP\", 2)]) ([(s/.np, \"<\", \"AHn\", 3)], (np, \">\", \"AHn\", 3), (np, \"<B\", \"AHn\", 2), [(s\\.np, \"<B\", \"AHn\", 3)], 2, Lp) is [1.0, 1.0, 0.0, 1.0, 1.0, 0.0]" $ do
      let sg1 = ([(getCateFromString "np", ">", "HnC", 2)], (getCateFromString "s", ">B", "DHv", 2), (getCateFromString "np","<B","AHn",2), [(getCateFromString "np",">B","SP",2)], 1, Lp)
      let sg2 = ([(getCateFromString "s/.np", "<", "AHn", 3)], (getCateFromString "np", ">", "AHn", 3), (getCateFromString "np","<B","AHn",2), [(getCateFromString "s\\.np","<B","AHn",3)], 2, Lp)
      distVect4StruGeneByIdentity sg1 sg2 `shouldBe` ([1.0, 1.0, 0.0, 1.0, 1.0, 0.0] :: [Double])

    it "The result of dist4StruGeneByWeightSum ([(getCateFromString \"np\", \">B\", \"HnC\")], (getCateFromString \"s\", \">\", \"DHv\"), (getCateFromString \"np\",\"<B\",\"AHn\"), [(getCateFromString \"np\",\">B\",\"SP\")]), 1, Lp) ([(getCateFromString \"s/.np\", \"<\", \"AHn\")], (getCateFromString \"np\", \">\", \"AHn\"), (getCateFromString \"np\",\"<B\",\"AHn\"), [(getCateFromString \"s\\.np\",\"<B\",\"AHn\")], 2, Lp) [1.0, 1.0, 1.0, 1.0, 1.0, 1.0] is 0.6666666666666666" $ do
      let sg1 = ([(getCateFromString "np", ">", "HnC", 2)], (getCateFromString "s", ">B", "DHv", 3), (getCateFromString "np","<B","AHn",2), [(getCateFromString "np",">B","SP",2)], 1, Lp)
      let sg2 = ([(getCateFromString "s/.np", "<", "AHn", 3)], (getCateFromString "np", ">", "AHn", 2), (getCateFromString "np","<B","AHn",2), [(getCateFromString "s\\.np","<B","AHn",3)], 2, Lp)
      let weigthList = [1, 1, 1, 1, 1, 1]
      dist4StruGeneByWeightSum sg1 sg2 weigthList `shouldBe` (4.0 / 6.0 :: Double)

    it "The result of dist4StruGeneByArithMean ([(getCateFromString \"np\", \">\", \"HnC\", 2)], (getCateFromString \"s\", \">B\", \"DHv\", 3), (getCateFromString \"np\",\"<B\",\"AHn\",2), [(getCateFromString \"np\",\">B\",\"SP\",2)], 1, Lp) ([(getCateFromString \"s/.np\", \"<\", \"AHn\", 3)], (getCateFromString \"np\", \">\", \"AHn\", 2), (getCateFromString \"np\",\"<B\",\"AHn\", 2), [(getCateFromString \"s\\.np\",\"<B\",\"AHn\",3)], 2, Lp) is 4.0/6.0" $ do
      let sg1 = ([(getCateFromString "np",">","HnC",2)], (getCateFromString "s", ">B", "DHv", 3), (getCateFromString "np","<B","AHn",2), [(getCateFromString "np",">B","SP",2)], 1, Lp)
      let sg2 = ([(getCateFromString "s/.np","<","AHn",3)], (getCateFromString "np", ">", "AHn", 2), (getCateFromString "np","<B","AHn",2), [(getCateFromString "s\\.np","<B","AHn",3)], 2, Lp)
      dist4StruGeneByArithMean sg1 sg2 `shouldBe` (4.0 / 6.0 :: Double)

{-    it "The result of minValueList  [readStruGeneFromStr \"([(np,Desig,DE)],(np\\*np,>,XX),(s,<,SP),[(((s\\.np)\\x(s\\.np))/*np,Desig,DE),((s\\.np)\\x(s\\.np),>,PO)],2,Lp)\", \
              \readStruGeneFromStr \"([(s\\.np,Desig,DE),(s,<,SP)],((s\\.np)\\x(s\\.np),>,PO),(np,<,HnC),[(np,Desig,DE)],2,Rp)\", \
              \readStruGeneFromStr \"([(s\\.np,Desig,DE)],((s\\.np)\\x(s\\.np),>,PO),(np,<,HnC),[(np,Desig,DE)],2,Rp)\", \
              \readStruGeneFromStr \"([(((s\\.np)/#(s\\.np))/*np,Desig,DE)],(np,>,AHn),(np,<,XX),[(s\\.np,Desig,DE)],2,Rp)\", \
              \readStruGeneFromStr \"([((np/*np)\\*(np/.np),Desig,DE),(np/*np,<,U1P)],(np,>,AHn),(np,<,HnC),[],2,Lp)\", \
              \readStruGeneFromStr \"([((np/*np)\\*(np/.np),Desig,DE),(np/*np,<,U1P)],(np,>,AHn),(np,>,AHn),[],3,Lp)\", \
              \readStruGeneFromStr \"([(((s\\.np)/#(s\\.np))/*np,Desig,DE)],(np,>,AHn),(s,<,SP),[(((s\\.np)\\x(s\\.np))/*np,Desig,DE),((s\\.np)\\x(s\\.np),>,PO)],2,Lp)\", \
              \readStruGeneFromStr \"([(np,Desig,DE)],(np,>,AHn),(np,<,HnC),[],2,Lp)\", \
              \readStruGeneFromStr \"([((np/.np)\\*(np/.np),Desig,DE),(np/.np,<,MQ)],(s,<,SP),(s\\.np,<,HvC),[(np,Desig,DE)],2,Lp)\"] \
              \[readStruGeneFromStr \"([],(np,A/n->,AHn),(s/#(s\\.np),>T->B,NR),[(((s\\.np)/#(s\\.np))/*np,Desig,DE)],2,Lp)\"] is [11.0, 11.0, 11.0, 9.0, 10.0, 11.0, 9.0, 10.0, 10.0]" $ do
      let sn = [readStruGeneFromStr "([(np,Desig,DE)],(np\\*np,>,XX),(s,<,SP),[(((s\\.np)\\x(s\\.np))/*np,Desig,DE),((s\\.np)\\x(s\\.np),>,PO)],2,Lp)",
                readStruGeneFromStr "([(s\\.np,Desig,DE),(s,<,SP)],((s\\.np)\\x(s\\.np),>,PO),(np,<,HnC),[(np,Desig,DE)],2,Rp)",
                readStruGeneFromStr "([(s\\.np,Desig,DE)],((s\\.np)\\x(s\\.np),>,PO),(np,<,HnC),[(np,Desig,DE)],2,Rp)",
                readStruGeneFromStr "([(((s\\.np)/#(s\\.np))/*np,Desig,DE)],(np,>,AHn),(np,<,XX),[(s\\.np,Desig,DE)],2,Rp)",
                readStruGeneFromStr "([((np/*np)\\*(np/.np),Desig,DE),(np/*np,<,U1P)],(np,>,AHn),(np,<,HnC),[],2,Lp)",
                readStruGeneFromStr "([((np/*np)\\*(np/.np),Desig,DE),(np/*np,<,U1P)],(np,>,AHn),(np,>,AHn),[],3,Lp)",
                readStruGeneFromStr "([(((s\\.np)/#(s\\.np))/*np,Desig,DE)],(np,>,AHn),(s,<,SP),[(((s\\.np)\\x(s\\.np))/*np,Desig,DE),((s\\.np)\\x(s\\.np),>,PO)],2,Lp)",
                readStruGeneFromStr "([(np,Desig,DE)],(np,>,AHn),(np,<,HnC),[],2,Lp)",
                readStruGeneFromStr "([((np/.np)\\*(np/.np),Desig,DE),(np/.np,<,MQ)],(s,<,SP),(s\\.np,<,HvC),[(np,Desig,DE)],2,Lp)"]
      let sg = [readStruGeneFromStr "([],(np,A/n->,AHn),(s/#(s\\.np),>T->B,NR),[(((s\\.np)/#(s\\.np))/*np,Desig,DE)],2,Lp)"]
      minValueList sn sg [] `shouldBe` ([11.0, 11.0, 11.0, 9.0, 10.0, 11.0, 9.0, 10.0, 10.0] :: [Float])
-}

    it "The result of findDistMinValOfTwoTupleLists [(2, 3.0), (3, 7.5), (6, 9.3), (5, 4.2)] [(3, 4.3), (6, 7.5), (2, 6.2), (5, 4.2)] [] is [(2, 3.0), (3, 4.3), (6, 9.3), (5, 4.2)]" $ do
      let l1 = [(2, 3.0), (3, 7.5), (6, 9.3), (5, 4.2)]
      let l2 = [(3, 4.3), (6, 7.5), (2, 6.2), (5, 4.2)]
      findDistMinValOfTwoTupleLists l1 l2 [] `shouldBe` ([(2, 3.0), (3, 4.3), (6, 7.5), (5, 4.2)] :: SIdxDistList)

    it "The result of readPhraSynListFromStr \"[(np,Desig,DE,1),(np\\*np,>,XX,2),(s,<,SP,2)]\" is [(np,Desig,DE,1),(np\\*np,>,XX,2),(s,<,SP,2)] " $ do
      let pSyn1 = readPhraSynFromStr "(np,\"Desig\",\"DE\",1)"
      let pSyn2 = readPhraSynFromStr "(np\\*np,\">\",\"XX\",2)"
      let pSyn3 = readPhraSynFromStr "(s,\"<\",\"SP\",2)"
      readPhraSynListFromStr "[(np,\"Desig\",\"DE\",1),(np\\*np,\">\",\"XX\",2),(s,\"<\",\"SP\",2)]" `shouldBe` ([pSyn1, pSyn2, pSyn3] :: [PhraSyn])

{-    it "The result of minValueList [readStruGeneFromStr \"([(np,Desig,DE)],(np\\*np,>,XX),(s,<,SP),[(((s\\.np)\\x(s\\.np))/*np,Desig,DE),((s\\.np)\\x(s\\.np),>,PO)],2,Lp)\", readStruGeneFromStr \"([(s\\.np,Desig,DE),(s,<,SP)],((s\\.np)\\x(s\\.np),>,PO),(np,<,HnC),[(np,Desig,DE)],2,Rp)\"] [readStruGeneFromStr \"([],(np,A/n->,AHn),(s/#(s\\.np),>T->B,NR),[(((s\\.np)/#(s\\.np))/*np,Desig,DE)],2,Lp)\"] is [11.0, 11.0]" $ do
      let sn = [readStruGeneFromStr "([(np,Desig,DE)],(np\\*np,>,XX),(s,<,SP),[(((s\\.np)\\x(s\\.np))/*np,Desig,DE),((s\\.np)\\x(s\\.np),>,PO)],2,Lp)",
                readStruGeneFromStr "([(s\\.np,Desig,DE),(s,<,SP)],((s\\.np)\\x(s\\.np),>,PO),(np,<,HnC),[(np,Desig,DE)],2,Rp)"]
      let sg = [readStruGeneFromStr "([],(np,A/n->,AHn),(s/#(s\\.np),>T->B,NR),[(((s\\.np)/#(s\\.np))/*np,Desig,DE)],2,Lp)"]
      minValueList sn sg [] `shouldBe` ([11.0, 11.0] :: [Float])
-}

    it "The result of getFreqMap [Lp, Lp, Noth, Rp, Noth, Rp, Rp] Map.empty is [(Lp, 2), (Noth,2), (Rp, 3)] " $ do
      let list = [Lp, Lp, Noth, Rp, Noth, Rp, Rp]
      getFreqMap list Map.empty `shouldBe` (Map.fromList [(Lp, 2), (Noth, 2), (Rp, 3)])

    it "The result of getMode4List [Lp, Lp, Noth, Rp, Noth, Rp, Rp] Map.fromList [(Lp, 2), (Noth, 2), (Rp, 3)] is Rp " $ do
      let list = [Lp, Lp, Noth, Rp, Noth, Rp, Rp]
      let kFreqMap = Map.fromList [(Lp, 2), (Noth, 2), (Rp, 3)]
      getMode4List list kFreqMap `shouldBe` (Rp :: Prior)
