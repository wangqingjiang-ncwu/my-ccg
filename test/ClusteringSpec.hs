-- Copyright China University of Water Resources and Electric Power (c) 2019-2023
-- All rights reserved.

module ClusteringSpec where

import Category
import Clustering
import AmbiResol (Prior(..))
import Test.Hspec

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

    it "The result of dist4StruGeneByArithAdd ([(getCateFromString \"np\", \">\", \"AHn\")], (getCateFromString \"s\", \">\", \"DHv\"), (getCateFromString \"s\\.np\",\"<B\",\"AHn\") ([(getCateFromString \"s/.np\", \"<\", \"AHn\")], 1, Lp) ([(getCateFromString \"s/.np\", \"<\", \"AHn\")], (getCateFromString \"np\", \">\", \"AHn\"), (getCateFromString \"np\",\"<B\",\"AHn\"), [(getCateFromString \"s\\.np\",\"<B\",\"AHn\")], 2, Lp) is 9.0" $ do
      let sg1 = ([(getCateFromString "np", ">", "AHn")], (getCateFromString "s", ">", "DHv"), (getCateFromString "s\\.np","<B","AHn"), [(getCateFromString "np",">B","SP")], 1, Lp)
      let sg2 = ([(getCateFromString "s/.np", "<", "AHn")], (getCateFromString "np", ">", "AHn"), (getCateFromString "np","<B","AHn"), [(getCateFromString "s\\.np","<B","AHn")], 2, Lp)
      dist4StruGeneByArithAdd sg1 sg2 `shouldBe` (9.0 :: Float)

    it "The result of dist4StruGeneByNormArithMean ([(getCateFromString \"np\", \">\", \"AHn\")], (getCateFromString \"s\", \">\", \"DHv\"), (getCateFromString \"s\\.np\",\"<B\",\"AHn\"), [(getCateFromString \"s/.np\", \"<\", \"AHn\")], 1, Lp) ([(getCateFromString \"s/.np\", \"<\", \"AHn\")], (getCateFromString \"np\", \">\", \"AHn\"), (getCateFromString \"np\",\"<B\",\"AHn\"), [(getCateFromString \"s\\.np\",\"<B\",\"AHn\")], 2, Lp) is 11.0/18.0" $ do
      let sg1 = ([(getCateFromString "np", ">", "AHn")], (getCateFromString "s", ">", "DHv"), (getCateFromString "s\\.np","<B","AHn"), [(getCateFromString "np",">B","SP")], 1, Lp)
      let sg2 = ([(getCateFromString "s/.np", "<", "AHn")], (getCateFromString "np", ">", "AHn"), (getCateFromString "np","<B","AHn"), [(getCateFromString "s\\.np","<B","AHn")], 2, Lp)
      dist4StruGeneByNormArithMean sg1 sg2 `shouldBe` (11.0/18.0 :: Float)
