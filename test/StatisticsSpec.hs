-- Copyright China University of Water Resources and Electric Power (c) 2019-2024
-- All rights reserved.

module StatisticsSpec where

import Category
import Phrase
import Corpus
import Statistics
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
