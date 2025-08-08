-- Copyright (c) 2019-2025 China University of Water Resources and Electric Power
-- All rights reserved.

module PhraseSpec where

import Category
import Rule
import Phrase
import Parse
import Test.Hspec
import Utils

spec :: Spec
spec = do
  describe "Phrase" $ do
    it "The result of createPhraCate 0 1 [(s, \"<\", \"smiles' Frank'\", \"SP\", True)] 1 is ((0,1),[(s, \"<\", \"smiles' Frank'\",\"SP\",True)],1)" $ do
      createPhraCate 0 1 [(sCate, "<", "smiles' Frank'", "SP", True)] 1 `shouldBe` (((0,1),[(sCate,"<","smiles' Frank'","SP",True)],1)::PhraCate)

    it "The result of applying func removeDup to [((0,0),[(np,\"Desig\",\"Frank'\",\"DE\",True)],0), ((0,0),[(np,\"Desig\",\"Frank'\",\"DE\",True)],0)] is [((0,0),[(np,\"Desig\",\"Frank'\",\"DE\",True)],0)]" $ do
      let c1 = npCate
      let c2 = getCateFromString "(s\\.np)/.np"
      let pc1 = createPhraCate 0 0 [(c1,"Desig","Frank'","DE",True)] 0
      let pc2 = createPhraCate 1 0 [(c2,"Desig","loves'","DE",True)] 1
      removeDup [pc1, pc1] `shouldBe` [pc1]

    it "The result of applying func pclt to ((0,0),[(np,\"Desig\",\"Frank'\",\"DE\",True)],0) and ((1,0),[((s\\.np)/.np,\"Desig\",\"loves'\",\"DE\",True)],1) is True" $ do
      let c1 = npCate
      let c2 = getCateFromString "(s\\.np)/.np"
      let pc1 = createPhraCate 0 0 [(c1,"Desig","Frank'","DE",True)] 0
      let pc2 = createPhraCate 1 0 [(c2,"Desig","loves'","DE",True)] 1
      pclt pc1 pc2 `shouldBe` (True :: Bool)

    it "The result of applying func pclt to ((0,0),[(np,\"Desig\",\"Frank'\",\"DE\",True)],0) and ((1,1),[(s/.np,\">\",\"(loves' Mary')\",\"VO\",True)],0) is False" $ do
      let c1 = npCate
      let c2 = getCateFromString "s/.np"
      let pc1 = createPhraCate 0 0 [(c1,"Desig","Frank'","DE",True)] 0
      let pc2 = createPhraCate 1 1 [(c2,">","(loves' Mary')","VO",True)] 1
      pclt pc2 pc1 `shouldBe` (False :: Bool)

    it "The result of ctspaOfCate ((1,0),[((s\\.np)/.np, \"Desig\", \"likes'\",\"DE\",True),(s/.np, \"Desig\", \"likes'\",\"DE\",True)],1) is [\"(s\\.np)/.np\", \"Desig\", \"likes'\", True),(\"s/.np\", \"Desig\", \"likes'\", True)]" $ do
      let pc = createPhraCate 1 0 [(getCateFromString "(s\\.np)/.np", "Desig", "likes'", "DE", True),(getCateFromString "s/.np", "Desig", "likes'", "DE", True)] 1
      let ctspa = [(getCateFromString "(s\\.np)/.np", "Desig", "likes'", "DE", True),(getCateFromString "s/.np", "Desig", "likes'", "DE", True)]
      ctspaOfCate pc `shouldBe` ctspa

    it "The result of ctspOfCate ((1,0),[((s\\.np)/.np, \"Desig\", \"likes'\",\"DE\",True),(s/.np, \"Desig\", \"likes'\",\"DE\",True)],1) is [(\"(s\\.np)/.np\", \"Desig\", \"likes'\"),(\"s/.np\", \"Desig\", \"likes'\")]" $ do
      let pc = createPhraCate 1 0 [(getCateFromString "(s\\.np)/.np", "Desig", "likes'", "DE", True),(getCateFromString "s/.np", "Desig", "likes'", "DE", True)] 1
      let ctsp = [(getCateFromString "(s\\.np)/.np", "Desig", "likes'","DE"),(getCateFromString "s/.np", "Desig", "likes'","DE")]
      ctspOfCate pc `shouldBe` ctsp

    it "The result of ctpsOfCate ((1,1),[(s\\.np, \">\", \"likes' reading'\",\"VO\",True)],2) is [(\"s\\.np\", \">\", \"likes' reading'\", 1)]" $ do
      let pc = createPhraCate 1 1 [(predCate, ">", "likes' reading'", "VO", True)] 2
      let ctps = [(predCate, ">", "VO", 1)]
      ctpsOfCate pc `shouldBe` ctps

    it "The result of csOfActCate ((1,0),[((s\\.np)/.np, \"Desig\", \"likes'\",\"DE\",True),(s/.np, \"Desig\", \"likes'\",\"DE\",False],1) is [(\"(s\\.np)/.np\", \"likes'\"),(\"s/.np\",\"likes'\")]" $ do
      let pc = createPhraCate 1 0 [(getCateFromString "(s\\.np)/.np", "Desig", "likes'", "DE", True),(getCateFromString "s/.np", "Desig", "likes'", "DE",False)] 1
      let cs = [(getCateFromString "(s\\.np)/.np", "likes'")]
      csOfActCate pc `shouldBe` cs

    it "The result of caOfActCate ((1,0),[((s\\.np)/.np, \"Desig\", \"likes'\",\"DE\",True),(s/.np, \"Desig\", \"likes'\",\"DE\",False],1) is [\"(s\\.np)/.np\",\"s/.np\"]" $ do
      let pc = createPhraCate 1 0 [(getCateFromString "(s\\.np)/.np", "Desig", "likes'","DE",True),(getCateFromString "s/.np", "Desig", "likes'", "DE",False)] 1
      let ca = [getCateFromString "(s\\.np)/.np"]
      caOfActCate pc `shouldBe` ca

    it "The result of pcBelong ((1,0),[(s\\.np)/.np, \"Desig\", \"likes'\",\"DE\",True)],1) ((1,0),[(s\\.np)/.np, \"Desig\", \"likes'\",\"DE\",True),(np, \">\",\"book'\",\"DE\",True)],1) is True" $ do
      let cate = getCateFromString "(s\\.np)/.np"
      let pc1 = createPhraCate 1 0 [(cate,"Desig","likes'","DE",True)] 1
      let pc2 = createPhraCate 1 0 [(cate,"Desig","likes'","DE",True),(npCate,">","book'","DE",True)] 1
      pcBelong pc1 pc2 `shouldBe` True

    it "The result of pcBelong ((1,0),[(s\\.np)/.np, \"Desig\", \"likes'\",\"DE\",True),(np, \">\",\"book'\",\"DE\",True)],1) ((1,0),[(s\\.np)/.np, \"Desig\", \"likes'\",\"DE\",True)],1) is False" $ do
      let cate = getCateFromString "(s\\.np)/.np"
      let pc1 = createPhraCate 1 0 [(cate, "Desig", "likes'", "DE", True)] 1
      let pc2 = createPhraCate 1 0 [(cate, "Desig", "likes'", "DE", True),(npCate,">","book'","DE",True)] 1
      pcBelong pc2 pc1 `shouldBe` False

    it "The result of pcBelong' ((1,0),[(s\\.np)/.np, \"Desig\", \"likes'\",\"DE\",False],1) ((1,0),[(s\\.np)/.np, \"Desig\", \"likes'\",\"DE\",True),(np, \">\",\"book'\",\"DE\",True)],1) is False" $ do
      let cate = getCateFromString "(s\\.np)/.np"
      let pc1 = createPhraCate 1 0 [(cate, "Desig", "likes'", "DE", False)] 1
      let pc2 = createPhraCate 1 0 [(cate, "Desig", "likes'", "DE", True), (npCate,">","book'","DE",True)] 1
      pcBelong' pc1 pc2 `shouldBe` True

    it "The result of stOfCate ((1,0),[(s\\.np, \"Desig\", \"smiles'\",\"DE\",True)],1) is 1" $ do
      let c = getCateFromString "s\\.np"
      let pc = createPhraCate 1 0 [(c,"Desig","smiles'","DE",True)] 1
      stOfCate pc `shouldBe` 1

    it "The result of spOfCate ((1,0),[(s\\.np, \"Desig\", \"smiles'\",\"DE\",True)],1) is 0" $ do
      let c = getCateFromString "s\\.np"
      let pc = createPhraCate 1 0 [(c,"Desig","smiles'","DE",True)] 1
      spOfCate pc `shouldBe` 0

    it "The result of ssOfCate ((1,0),[(s\\.np, \"Desig\", \"smiles'\",\"DE\",True)],1) is 1" $ do
      let c = getCateFromString "s\\.np"
      let pc = createPhraCate 1 0 [(c,"Desig","smiles'","DE",True)] 1
      ssOfCate pc `shouldBe` 1

    it "The result of ctspaOfCate ((1,0),[(s\\.np, \"Desig\", \"smiles'\",\"DE\",True)],1) is [(s\\.np, \"Desig\", \"smiles'\",\"DE\",True)]" $ do
      let c = getCateFromString "s\\.np"
      let pc = createPhraCate 1 0 [(c,"Desig","smiles'","DE",True)] 1
      ctspaOfCate pc `shouldBe` [(c,"Desig","smiles'","DE",True)]

    it "The result of caOfCate ((1,0),[(s\\.np, \"Desig\", \"smiles'\",\"DE\",True)],1) is [s\\.np]" $ do
      let c = getCateFromString "s\\.np"
      let pc = createPhraCate 1 0 [(c,"Desig","smiles'","DE",True)] 1
      caOfCate pc `shouldBe` [c]

    it "The result of cateComb \"[]\" ((0,0),[(np, \"Desig\", \"Frank'\",\"DE\",True)],0) ((1,0),[(s\\.np, \"Desig\", \"smiles\",\"DE\",True)],1) is ((0,1),[(s, \"<\", \"(smiles' Frank')\",\"SP\",True)],1)" $ do
      let c1 = npCate
      let c2 = getCateFromString "s\\.np"
      let c3 = sCate
      let pc1 = createPhraCate 0 0 [(c1, "Desig", "Frank'", "DE", True)] 0
      let pc2 = createPhraCate 1 0 [(c2, "Desig", "smiles'", "DE", True)] 1
      let pc3 = createPhraCate 0 1 [(c3, "<", "(smiles' Frank')", "SP", True)] 1
      cateComb [] pc1 pc2 `shouldBe` pc3

    it "The result of atomizePhraCateList [cateComb [Sv,Ov] ((0,0),[((s\\.np)/.np,\"Desig\",\"loving'\",\"DE\",True)],0) ((1,0),[(s\\.np,\"Desig\",\"is_forever'\",\"DE\",True)],1)] is [((0,1),[(s,\"S/v-<\",\"(is_forever' loving')\",\"SP\",True),(s\\.np,\"O/v->\",\"(loving' is_forever')\",\"VO\",True)],1)]" $ do
      let c1 = getCateFromString "(s\\.np)/.np"
      let c2 = getCateFromString "s\\.np"
      let c3 = sCate
      let c4 = getCateFromString "s\\.np"
      let pc1 = createPhraCate 0 0 [(c1,"Desig","loving'","DE",True)] 0
      let pc2 = createPhraCate 1 0 [(c2,"Desig","is_forever'","DE",True)] 1
      let pc3 = createPhraCate 0 1 [(c3,"S/v-<","(is_forever' loving')","SP",True)] 1
      let pc4 = createPhraCate 0 1 [(c4,"O/v->","(loving' is_forever')","VO",True)] 1
      atomizePhraCateList [cateComb [Sv,Ov] pc1 pc2] `shouldBe` [pc3,pc4]

    it "The result of deactOnePC ((0,0),[(np,\"Desig\",\"Frank'\",\"DE\",True)],0) is ((0,0),[(np,\"Desig\",\"Frank'\",\"DE\",False],0)" $ do
      let c1 = npCate
      let pc1 = createPhraCate 0 0 [(c1,"Desig","Frank'", "DE", True)] 0
      let pc2 = createPhraCate 0 0 [(c1,"Desig","Frank'", "DE", False)] 0
      deactOnePC pc1 `shouldBe` pc2

    it "The result of actOnePC ((0,0),[(np,\"Desig\",\"Frank'\",\"DE\",False],0) is ((0,0),[(np,\"Desig\",\"Frank'\",\"DE\",True)],0)" $ do
      let c1 = npCate
      let pc1 = createPhraCate 0 0 [(c1,"Desig","Frank'", "DE", False)] 0
      let pc2 = createPhraCate 0 0 [(c1,"Desig","Frank'", "DE", True)] 0
      actOnePC pc1 `shouldBe` pc2

    it "The result of atomizePhraCateList [((0,0),[(np,\"Desig\",\"Frank\")],1), ((1,1),[(s\\.np,\">\",\"loves' Mary'\"),((s/.np,\">T->B\",\"Tim' loves'\"))],2)] is [((0,0),[(np,\"Desig\",\"Frank\")],1), ((1,1),[(s\\.np,\">\",\"loves' Mary'\"),2)],2), ((1,1),[(s/.np,\">T->B\",\"Tim' loves'\")],2)]" $ do
      let pc1 = createPhraCate 0 0 [(npCate, "Desig", "Frank", "DE", True)] 1
      let pc2 = createPhraCate 1 1 [(getCateFromString "s\\.np",">","loves' Mary'","DE",True),(getCateFromString "s/.np",">T->B","Tom' loves'","DE",True)] 2
      let pc21 = createPhraCate 1 1 [(getCateFromString "s\\.np", ">", "loves' Mary'", "DE", True)] 2
      let pc22 = createPhraCate 1 1 [(getCateFromString "s/.np", ">T->B", "Tom' loves'", "DE", True)] 2
      atomizePhraCateList [pc1,pc2] `shouldBe` [pc1,pc21,pc22]

    it "The result of getPhraBySpan 1 [((0,0),[(np/.np,\"Desig\",\"Brave'\")],0), ((1,0),[(np,\"Desig\",\"Frank'\")],1), ((2,0),[(s\\*np,\"Desig\"),\"wins'\"],2), ((0,1),[(np,\">\",\"(Brave' Frank')\")],1), ((1,1),[(s,\"<\",\"(wins' Frank')\")],2), ((0,2),[np,\"Np/s->\",\"Brave' (wins' Frank')\"],1), ((0,2),[(s,\"<\",\"wins' (Brave' Frank')\")],2)] is [((0,1),[(np,\">\",\"Brave' Frank'\")],1), ((1,1),[(s,\"<\",\"wins' Frank'\")],2)]" $ do
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
      let pc11 = createPhraCate 0 1 [(c11, ">", "Brave' Frank'", "AHn", True)] 1
      let pc12 = createPhraCate 1 1 [(c12, "<", "wins' Frank'", "SP", True)] 2
      let pc21 = createPhraCate 0 2 [(c21, "Np/s->", "Brave' (wins' Frank')", "AHn", True)] 1
      let pc22 = createPhraCate 0 2 [(c22, "<", "wins' (Brave' Frank')", "SP", True)] 2
      let pcs = [pc01,pc02,pc03]
      let pcClo = pcs ++ [pc11,pc12,pc21,pc22]
      getPhraBySpan 1 pcClo `shouldBe` [pc11,pc12]

    it "The result of divPhraCateBySpan [((0,0),[(np/.np,\"Desig\",\"Brave'\",\"DE\",True)],0), ((1,0),[(np,\"Desig\",\"Frank'\",\"DE\",True)],1), ((2,0),[(s\\*np,\"Desig\"),\"wins'\",\"DE\",True)],2), ((0,1),[(np,\">\",\"(Brave' Frank')\",\"DE\",True)],1), ((0,2),[(s,\"<\",\"(wins' (Brave' Frank'))\",\"DE\",True)],2)] is [[((0,0),[(np/.np, \"Desig\",\"Brave'\",\"DE\",True)],0), ((1,0),[(np,\"Desig\",\"Frank'\",\"DE\",True)],1), ((2,0),[(s\\*np, \"Desig\",\"wins'\",\"DE\",True)],2)], [((0,1),[(np, \">\",\"(Brave' Frank')\",\"DE\",True)],1)], [((0,2),[(s,\"<\",\"(wins' (Brave' Frank'))\",\"DE\",True)],2)]]" $ do
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
      let pc21 = createPhraCate 0 2 [(c21, "Np/s->", "(Brave' (wins' Frank'))", "AHn", True)] 1
      let pc22 = createPhraCate 0 2 [(c22, "<", "(wins' (Brave' Frank'))", "SP", True)] 2
      let pcClo = [pc01,pc02,pc03,pc11,pc22]
      divPhraCateBySpan pcClo `shouldBe` [[pc01,pc02,pc03],[pc11],[pc22]]

    it "The result of elem4Phrase ((0,0),[(np/.np,\"Desig\",\"Brave'\",\"DE\",True)],0), [((0,0),[(np/.np,\"Desig\",\"Brave'\",\"DE\",True)],0), ((1,0),[(np,\"Desig\",\"Frank'\",\"DE\",True)],1)] is True." $ do
      let c01 = getCateFromString "np/.np"
      let c02 = npCate
      let pc01 = createPhraCate 0 0 [(c01, "Desig", "Brave'", "DE", True)] 0
      let pc02 = createPhraCate 1 0 [(c02, "Desig", "Frank'", "DE", True)] 1
      let pcClo = [pc01,pc02]
      elem4Phrase pc01 pcClo `shouldBe` True

    it "The result of elem4Phrase ((1,0),[(np,\"Desig\",\"Frank'\",\"DE\",False)],1), [((0,0),[(np/.np,\"Desig\",\"Brave'\",\"DE\",True)],0), ((1,0),[(np,\"Desig\",\"Frank'\",\"DE\",True)],1)] is True." $ do
      let c01 = getCateFromString "np/.np"
      let c02 = npCate
      let pc01 = createPhraCate 0 0 [(c01, "Desig", "Brave'", "DE", True)] 0
      let pc02 = createPhraCate 1 0 [(c02, "Desig", "Frank'", "DE", False)] 1
      let pcClo = [pc01,pc02]
      elem4Phrase pc02 pcClo `shouldBe` True

    it "The result of elem4Phrase ((2,0),[(np,\"Desig\",\"Frank'\",\"DE\",False)],2), [((0,0),[(np/.np,\"Desig\",\"Brave'\",\"DE\",True)],0), ((1,0),[(np,\"Desig\",\"Frank'\",\"DE\",True)],1)] is False." $ do
      let c01 = getCateFromString "np/.np"
      let c02 = npCate
      let pc01 = createPhraCate 0 0 [(c01, "Desig", "Brave'", "DE", True)] 0
      let pc02 = createPhraCate 1 0 [(c02, "Desig", "Frank'", "DE", False)] 1
      let pc02' = createPhraCate 2 0 [(c02, "Desig", "Frank'", "DE", False)] 2
      let pcClo = [pc01,pc02]
      elem4Phrase pc02' pcClo `shouldBe` False

    it "The result of notelem4Phrase ((2,0),[(np,\"Desig\",\"Frank'\",\"DE\",False)],2), [((0,0),[(np/.np,\"Desig\",\"Brave'\",\"DE\",True)],0), ((1,0),[(np,\"Desig\",\"Frank'\",\"DE\",True)],1)] is True." $ do
      let c01 = getCateFromString "np/.np"
      let c02 = npCate
      let pc01 = createPhraCate 0 0 [(c01, "Desig", "Brave'", "DE", True)] 0
      let pc02 = createPhraCate 1 0 [(c02, "Desig", "Frank'", "DE", False)] 1
      let pc02' = createPhraCate 2 0 [(c02, "Desig", "Frank'", "DE", False)] 2
      let pcClo = [pc01,pc02]
      notElem4Phrase pc02' pcClo `shouldBe` True
