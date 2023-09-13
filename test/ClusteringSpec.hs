-- Copyright China University of Water Resources and Electric Power (c) 2019-2023
-- All rights reserved.

module ClusteringSpec where

import Category
import Clustering
import AmbiResol
import Test.Hspec
import qualified Data.Map as Map

spec :: Spec
spec = do
  describe "Clustering" $ do
    it "The result of distPhraSyn (getCateFromString \"np\", \">\", \"AHn\")) (getCateFromString \"(s\\.np)/.np\", \"<B\", \"AHn\") is 2" $ do
      let c1 = getCateFromString "np"
      let c2 = getCateFromString "(s\\.np)/.np"
      let t1 = ">"
      let t2 = "<B"
      let p1 = "AHn"
      let p2 = "AHn"
      distPhraSyn (c1,t1,p1) (c2,t2,p2) `shouldBe` (2 :: Int)

    it "The result of distPhraSynSet [(getCateFromString \"np\", \">\", \"AHn\"), (getCateFromString \"s\", \">\", \"DHv\")] [(getCateFromString \"s\\.np\", \"<B\", \"AHn\") (getCateFromString \"np\", \">B\", \"SP\")] is 2.5" $ do
      let p1 = (getCateFromString "np", ">", "AHn")
      let p2 = (getCateFromString "s", ">", "DHv")
      let q1 = (getCateFromString "s\\.np","<B","AHn")
      let q2 = (getCateFromString "np",">B","SP")
      distPhraSynSet [p1,p2] [q1,q2] `shouldBe` (2.5 :: Float)

    it "The result of distVect4StruGene ([(getCateFromString \"np\", \">\", \"AHn\")], (getCateFromString \"s\", \">\", \"DHv\"), (getCateFromString \"s\\.np\",\"<B\",\"AHn\") ([(getCateFromString \"s/.np\", \"<\", \"AHn\")], (getCateFromString \"np\", \">\", \"AHn\"), (getCateFromString \"np\",\"<B\",\"AHn\"), [(getCateFromString \"s\\.np\",\"<B\",\"AHn\")], 2, Lp) is [2.0, 2.0, 1.0, 3.0, 1.0, 0.0]" $ do
      let sg1 = ([(getCateFromString "np", ">", "AHn")], (getCateFromString "s", ">", "DHv"), (getCateFromString "s\\.np","<B","AHn"), [(getCateFromString "np",">B","SP")], 1, Lp)
      let sg2 = ([(getCateFromString "s/.np", "<", "AHn")], (getCateFromString "np", ">", "AHn"), (getCateFromString "np","<B","AHn"), [(getCateFromString "s\\.np","<B","AHn")], 2, Lp)
      distVect4StruGene sg1 sg2 `shouldBe` ([2.0, 2.0, 1.0, 3.0, 1.0, 0.0] :: [Float])

    it "The result of dist4StruGeneByArithAdd ([(getCateFromString \"np\", \">\", \"AHn\")], (getCateFromString \"s\", \">\", \"DHv\"), (getCateFromString \"s\\.np\",\"<B\",\"AHn\") ([(getCateFromString \"s/.np\", \"<\", \"AHn\")], 1, Lp) ([(getCateFromString \"s/.np\", \"<\", \"AHn\")], (getCateFromString \"np\", \">\", \"AHn\"), (getCateFromString \"np\",\"<B\",\"AHn\"), [(getCateFromString \"s\\.np\",\"<B\",\"AHn\")], 2, Lp) [1.0, 1.0, 1.0, 1.0, 1.0, 1.0] is 9.0" $ do
      let sg1 = ([(getCateFromString "np", ">", "AHn")], (getCateFromString "s", ">", "DHv"), (getCateFromString "s\\.np","<B","AHn"), [(getCateFromString "np",">B","SP")], 1, Lp)
      let sg2 = ([(getCateFromString "s/.np", "<", "AHn")], (getCateFromString "np", ">", "AHn"), (getCateFromString "np","<B","AHn"), [(getCateFromString "s\\.np","<B","AHn")], 2, Lp)
      let weigthList = [1, 1, 1, 1, 1, 1]
      dist4StruGeneByArithAdd sg1 sg2 weigthList `shouldBe` (9.0 :: Float)

    it "The result of dist4StruGeneByNormArithMean ([(getCateFromString \"np\", \">\", \"AHn\")], (getCateFromString \"s\", \">\", \"DHv\"), (getCateFromString \"s\\.np\",\"<B\",\"AHn\"), [(getCateFromString \"s/.np\", \"<\", \"AHn\")], 1, Lp) ([(getCateFromString \"s/.np\", \"<\", \"AHn\")], (getCateFromString \"np\", \">\", \"AHn\"), (getCateFromString \"np\",\"<B\",\"AHn\"), [(getCateFromString \"s\\.np\",\"<B\",\"AHn\")], 2, Lp) is 11.0/18.0" $ do
      let sg1 = ([(getCateFromString "np", ">", "AHn")], (getCateFromString "s", ">", "DHv"), (getCateFromString "s\\.np","<B","AHn"), [(getCateFromString "np",">B","SP")], 1, Lp)
      let sg2 = ([(getCateFromString "s/.np", "<", "AHn")], (getCateFromString "np", ">", "AHn"), (getCateFromString "np","<B","AHn"), [(getCateFromString "s\\.np","<B","AHn")], 2, Lp)
      dist4StruGeneByNormArithMean sg1 sg2 `shouldBe` (11.0/18.0 :: Float)

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

    it "The result of readPhraSynListFromStr \"[(np,Desig,DE),(np\\*np,>,XX),(s,<,SP)]\" is [(np,Desig,DE),(np\\*np,>,XX),(s,<,SP)] " $ do
      let pSyn1 = readPhraSynFromStr "(np,Desig,DE)"
      let pSyn2 = readPhraSynFromStr "(np\\*np,>,XX)"
      let pSyn3 = readPhraSynFromStr "(s,<,SP)"
      readPhraSynListFromStr "[(np,Desig,DE),(np\\*np,>,XX),(s,<,SP)]" `shouldBe` ([pSyn1, pSyn2, pSyn3] :: [PhraSyn])

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
