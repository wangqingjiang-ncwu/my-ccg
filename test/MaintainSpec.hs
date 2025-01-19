-- Copyright (c) 2019-2025 China University of Water Resources and Electric Power
-- All rights reserved.

module MaintainSpec where

import Maintain
import Category
import Phrase
import Test.Hspec

spec :: Spec
spec = do
-- addHX2Phrase :: PhraCate -> PhraCate
    describe "addHX2Phrase" $ do
      it "addHX2Phrase ((0,0),[(np\\*np,\">\",\"and' you'\",\"XX\",True)],0) is ((0,0),[((np\\*np,\">\",\"and' you'\",\"HX\",True)],0)." $ do
        let c0 = getCateFromString "np\\*np"
        let p0 = createPhraCate 0 0 [(c0, ">", "and' you'", "XX", True)] 0
        let p0' = createPhraCate 0 0 [(c0, ">", "and' you'", "HX", True)] 0
        addHX2Phrase p0 `shouldBe` p0'

      it "addHX2Phrase ((2,1),[(np\\*np,\">\",\"和' 文管'\",\"XX\",True)],3) is ((2,1),[(np\\*np,\">\",\"和' 文管'\",\"HX\",True)],3)." $ do
        let c0 = getCateFromString "np\\*np"
        let p0 = createPhraCate 2 1 [(c0, ">", "和' 文管'", "XX", True)] 3
        let p0' = createPhraCate 2 1 [(c0, ">", "和' 文管'", "HX", True)] 3
        addHX2Phrase p0 `shouldBe` p0'
