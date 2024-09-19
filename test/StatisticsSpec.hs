-- Copyright China University of Water Resources and Electric Power (c) 2019-2024
-- All rights reserved.

module StatisticsSpec where

import Category
import Phrase
import Corpus
import Statistics
import Data.Map (Map)
import qualified Data.Map as Map
import Test.Hspec

spec :: Spec
spec = do
  describe "Statistics" $ do
    it "The result of hasTagInTree D/n->T->B (1,[((1,1),[(s/#(s\\.np),D/n->T->B,教学计划' 学校',PE,True)],2)]) is True." $ do
      let c1 = getCateFromString "s/#(s\\.np)"
      let pc1 = createPhraCate 1 1 [(c1,"D/n->T->B","教学计划' 学校'","PE",True)] 2
      let t1 = (1,[pc1])
      hasTagInTree "D/n->T->B" t1 `shouldBe` True

-- filterTagInTrees :: Tag -> [Tree] -> [Tree]
    it "The result of filterTagInTrees D/n->T->B [(1, [((1,1),[(s/#(s\\.np),D/n->T->B,教学计划' 学校',PE,True)],2)]), (2, [((2,1),[(s/.np,>T->B,规定' 教学计划',OE,True)],3)])] is [(1, [((1,1),[(s/#(s\\.np),D/n->T->B,教学计划' 学校',PE,True)],2)])." $ do
      let c1 = getCateFromString "s/#(s\\.np)"
      let pc1 = createPhraCate 1 1 [(c1,"D/n->T->B","教学计划' 学校'","PE",True)] 2
      let c2 = getCateFromString "s/.np"
      let pc2 = createPhraCate 2 1 [(c2,">T->B","规定' 教学计划'","OE",True)] 3
      let ts1 = [(1,[pc1]),(2,[pc2])]
      let ts1' = [(1,[pc1])]
      filterTagInTrees "D/n->T->B" ts1 `shouldBe` ts1'

-- filterTagInSentTrees :: Tag -> [(SentIdx, [Tree])] -> [(SentIdx, [Tree])]
    it "The result of filterTagInSentTrees D/n->T->B [(1, [(1, [((1,1),[(s/#(s\\.np),D/n->T->B,教学计划' 学校',PE,True)],2)])]), (2, (2, [((2,1),[(s/.np,>T->B,规定' 教学计划',OE,True)],3)]))] is [(1, (1, [((1,1),[(s/#(s\\.np),D/n->T->B,教学计划' 学校',PE,True)],2)]))]." $ do
      let c1 = getCateFromString "s/#(s\\.np)"
      let pc1 = createPhraCate 1 1 [(c1,"D/n->T->B","教学计划' 学校'","PE",True)] 2
      let c2 = getCateFromString "s/.np"
      let pc2 = createPhraCate 2 1 [(c2,">T->B","规定' 教学计划'","OE",True)] 3
      let sts1 = [(1,[(1,[pc1])]),(2,[(2,[pc2])])]
      let sts1' = [(1,[(1,[pc1])])]
      filterTagInSentTrees "D/n->T->B" sts1 `shouldBe` sts1'

-- insertPhraList2TtsFreqMap' :: [PhraCate] -> Map (Category, Tag, PhraStru) Int -> Map (Category, Tag, PhraStru) Int
    it "The result of insertPhraList2TtsFreqMap' [((1,1),[(s/#(s\\.np),D/n->T->B,教学计划' 学校',PE,True)],2), ((2,1),[(s/#(s\\.np),D/n->T->B,规定' 教学计划',PE,True)],3)] Map.empty is [((s/#(s\\.np),D/n->T->B,PE),2)]." $ do
      let c1 = getCateFromString "s/#(s\\.np)"
      let pc1 = createPhraCate 1 1 [(c1,"D/n->T->B","教学计划' 学校'","PE",True)] 2
      let pc2 = createPhraCate 2 1 [(c1,"D/n->T->B","规定' 教学计划'","PE",True)] 3
      let ttsNumList = Map.toList $ insertPhraList2TtsFreqMap' [pc1,pc2] Map.empty
      ttsNumList `shouldBe` [((c1,"D/n->T->B","PE"),2)]


-- toTypeTagStru2FreqMap' :: [[[PhraCate]]] -> Map (Category, Tag, PhraStru) Int -> Map (Category, Tag, PhraStru) Int
    it "The result of toTypeTagStru2FreqMap' [[((1,1),[(s/#(s\\.np),D/n->T->B,教学计划' 学校',PE,True)],2), ((2,1),[(s/.np,>T->B,规定' 教学计划',OE,True)],3)]] Map.empty is [((s/.np,>T->B,OE),1),((s/#(s\\.np),D/n->T->B,PE),1)]." $ do
      let c1 = getCateFromString "s/#(s\\.np)"
      let pc1 = createPhraCate 1 1 [(c1,"D/n->T->B","教学计划' 学校'","PE",True)] 2
      let c2 = getCateFromString "s/.np"
      let pc2 = createPhraCate 2 1 [(c2,">T->B","规定' 教学计划'","OE",True)] 3
      let ttsNumList = Map.toList $ toTypeTagStru2FreqMap' [[[pc1,pc2]]] Map.empty
      ttsNumList `shouldBe` [((c2,">T->B","OE"),1),((c1,"D/n->T->B","PE"),1)]

-- toTypeTagStru2FreqMap' :: [[[PhraCate]]] -> Map (Category, Tag, PhraStru) Int -> Map (Category, Tag, PhraStru) Int
    it "The result of toTypeTagStru2FreqMap' [[((1,1),[(s/#(s\\.np),D/n->T->B,教学计划' 学校',PE,True)],2)], ((2,1),[(s/.np,>T->B,规定' 教学计划',OE,True)],3)],[((1,1),[(s/#(s\\.np),D/n->T->B,教学计划' 学校',PE,True)],2)], ((2,1),[(s/.np,>T->B,规定' 教学计划',OE,True)],3)]] Map.empty is [((s/.np,>T->B,OE),2),((s/#(s\\.np),D/n->T->B,PE),2)]." $ do
      let c1 = getCateFromString "s/#(s\\.np)"
      let pc1 = createPhraCate 1 1 [(c1,"D/n->T->B","教学计划' 学校'","PE",True)] 2
      let c2 = getCateFromString "s/.np"
      let pc2 = createPhraCate 2 1 [(c2,">T->B","规定' 教学计划'","OE",True)] 3
      let ttsNumList = Map.toList $ toTypeTagStru2FreqMap' [[[pc1,pc2],[pc1,pc2]]] Map.empty
      ttsNumList `shouldBe` [((c2,">T->B","OE"),2),((c1,"D/n->T->B","PE"),2)]

    it "The result of (==) (np,>,AHn) (np,>,AHn) is True." $ do
      (==) (npCate,">","AHn") (npCate,">","AHn") `shouldBe` True

    it "The result of (<) (s/.np,>T->B,OE) (s/#(s\\.np),D/n->T->B,PE) is False." $ do
      let c1 = getCateFromString "s/#(s\\.np)"
      let c2 = getCateFromString "s/.np"
      (<) (c1,">T->B","OE") (c2,"D/n->T->B","PE") `shouldBe` False

    it "The result of (>) (s\\.np,>,VO) (s\\.np,<,HvC) is True." $ do
      let c1 = getCateFromString "s\\.np"
      let c2 = c1
      (>) (c1,">","VO") (c2,"<","HvC") `shouldBe` True

    it "The result of Map.unionWith [((predCate,>,VO),1), ((verbCate,>T->B,DHv),2)] [((predCate,>,VO),2)] is [((predCate,>,VO),3), ((verbCate,>T->B,DHv),2)]." $ do
      let map1 = Map.insert (predCate,">","VO") 1 Map.empty
      let map2 = Map.insert (predCate,">","VO") 2 Map.empty
      let map4 = Map.insert (verbCate,">T->B","DHv") 2 Map.empty
      let map11 = Map.insert (verbCate,">T->B","DHv") 2 map1
      let map12 = Map.insert (predCate,">","VO") 3 map11
      Map.unionWith (+) map11 map2 `shouldBe` map12

    it "The result of quickSort4PhraSyn  [(s\\.np,>,VO), (np,>,VO), (np/.np,>,VO)] is [(np,>,VO), (np/.np,>,VO), (s\\.np,>,VO)]." $ do
      let gt1 = (predCate,">","VO")
      let gt2 = (npCate,">","VO")
      let gt3 = (adjCate,">","VO")
      quickSort4PhraSyn  [gt1,gt2,gt3] `shouldBe` [gt2,gt3,gt1]
