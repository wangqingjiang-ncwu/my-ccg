-- Copyright China University of Water Resources and Electric Power (c) 2019
-- All rights reserved.

module OutputSpec where

import Category
import Parse
import Phrase
import Output
import Test.Hspec

spec :: Spec
spec = do
    describe "Output" $ do
      it "getCateWidth' ((0,0),[((X\\*X)/*X,\"Desig\",\"and'\",\"DE\",True)],0) [] is 15." $ do
        let c00 = getCateFromString "(X\\*X)/*X"
        let p00 = createPhraCate 0 0 [(c00, "Desig", "and'", "DE", True)] 0
        getCateWidth' p00 [] `shouldBe` 15

      it "getCateWidth' ((0,0),[(((np/.np)/*(np/.np))\\*((np/.np)/*(np/.np)),\">\"),\"and' bravely'\",\"XX\",False)],1) [] is 47." $ do
        let c00 = getCateFromString "((np/.np)/*(np/.np))\\*((np/.np)/*(np/.np))"
        let p00 = createPhraCate 0 0 [(c00, "Desig", "and'", "DE", True)] 0
        getCateWidth' p00 [] `shouldBe` 47

      it "getCateWidth' ((0,1),[(((np/.np)/*(np/.np))\\*((np/.np)/*(np/.np)),\">\"),\"and' bravely'\",\"XX\",False)],1) [[((0,0),[((X\\*X)/*X,\"Desig\",\"and'\",\"DE\",True)],0), ((1,0),[((np/.np)/*(np/.np),\"Desig\",\"bravely'\",\"DE\",True)],1)], [((0,1),[(((np/.np)/*(np/.np))\\*((np/.np)/*(np/.np)),\">\"),\"and' bravely'\",\"XX\",False)],1)]] is 39." $ do
        let c00 = getCateFromString "(X\\*X)/*X"
        let c10 = getCateFromString "(np/.np)/*(np/.np)"
        let c01 = getCateFromString "((np/.np)/*(np/.np))\\*((np/.np)/*(np/.np))"
        let p00 = createPhraCate 0 0 [(c00, "Desig", "and'", "DE", True)] 0
        let p10 = createPhraCate 1 0 [(c10, "Desig", "bravely'", "DE", True)] 1
        let p01 = createPhraCate 0 1 [(c01, ">", "and' bravely'", "XX", False)] 1
        getCateWidth' p01 [[p00,p10],[p01]] `shouldBe` 39

      it "getCateWidth ((0,0),[((X\\*X)/*X,\"Desig\",\"and'\",\"DE\",True)],0) [[((0,0),[((X\\*X)/*X,\"Desig\",\"and'\",\"DE\",True)],0), ((1,0),[((np/.np)/*(np/.np),\"Desig\",\"bravely'\",\"DE\",True)],1)], [((0,1),[(((np/.np)/*(np/.np))\\*((np/.np)/*(np/.np)),\">\"),\"and' bravely'\",\"XX\",False)],1)]] is 23." $ do
        let c00 = getCateFromString "(X\\*X)/*X"
        let c10 = getCateFromString "(np/.np)/*(np/.np)"
        let c01 = getCateFromString "((np/.np)/*(np/.np))\\*((np/.np)/*(np/.np))"
        let p00 = createPhraCate 0 0 [(c00, "Desig", "and'", "DE", True)] 0
        let p10 = createPhraCate 1 0 [(c10, "Desig", "bravely'", "DE", True)] 1
        let p01 = createPhraCate 0 1 [(c01, ">", "and' bravely'", "XX", False)] 1
        getCateWidth p00 [[p00,p10],[p01]] `shouldBe` 23
