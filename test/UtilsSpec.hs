-- Copyright (c) 2019-2025 China University of Water Resources and Electric Power
-- All rights reserved.

module UtilsSpec where

import Utils
import Phrase
import Test.Hspec

spec :: Spec
spec = do
  describe "Utils" $ do
    it "The result of show Empty is ()" $ do
      show (Empty :: BiTree PhraCate) `shouldBe`  "()"
    it "The result of show (Node 1 (Node 2 Empty Empty) Empty) is (1,(2,(),()),())" $ do
      show (Node 1 (Node 2 Empty Empty) Empty) `shouldBe` "(1,(2,(),()),())"
    it "The result of Empty < (Node 1 Empty Empty) is True" $ do
      Empty < (Node 1 Empty Empty) `shouldBe` True
    it "The result of getLeftSub (Node 1 Empty Empty) is Empty" $ do
      getLeftSub (Node 1 Empty Empty) `shouldBe` Empty
    it "The result of getLeftSub (Node 1 (Node 2 Empty Empty) Empty) is (Node 2 Empty Empty)" $ do
      getLeftSub (Node 1 (Node 2 Empty Empty) Empty) `shouldBe` (Node 2 Empty Empty)
    it "The result of setRightSub Empty (Node 1 Empty (Node 2 Empty Empty)) is (Node 1 Empty Empty)" $ do
      setRightSub Empty (Node 1 Empty (Node 2 Empty Empty)) `shouldBe` (Node 1 Empty Empty)
    it "The result of traverseBiTree (Empty :: BiTree Int) is []" $ do
      traverseBiTree (Empty :: BiTree Int) `shouldBe` []
    it "The result of traverseBiTree ((Node 1 Empty (Node 2 Empty Empty)) :: BiTree Int) is [1,2] :: [Int]" $ do
      traverseBiTree ((Node 1 Empty (Node 2 Empty Empty)) :: BiTree Int) `shouldBe` [1,2]
    it "The result of nodePairsBetwTwOBiTree ((Node 3 (Node 4 Empty Empty) (Node 5 Empty Empty)) :: BiTree Int) ((Node 1 Empty (Node 2 Empty Empty)) :: BiTree Int) is [(3,1),(5,2)] :: [(Int,Int)]" $ do
      nodePairsBetwTwOBiTree ((Node 3 (Node 4 Empty Empty) (Node 5 Empty Empty)) :: BiTree Int) ((Node 1 Empty (Node 2 Empty Empty)) :: BiTree Int) `shouldBe` ([(3,1),(5,2)] :: [(Int,Int)])
    it "The result of forest2BiTree [] is Empty" $ do
      forest2BiTree [] `shouldBe` (Empty :: BiTree Int)
    it "The result of forest2BiTree [Node 3 Empty Empty, Node 5 Empty Empty] is Node 3 Empty (Node 5 Empty Empty)" $ do
      forest2BiTree [Node 3 Empty Empty, Node 5 Empty Empty] `shouldBe` Node 3 Empty (Node 5 Empty Empty)
    it "The result of forest2BiTree ([Node 3 (Node 4 Empty Empty) (Node 5 Empty Empty), Node 6 Empty Empty] :: BiTree Int) is Node 3 (Node 4 Empty (Node 5 Empty Empty)) (Node 6 Empty Empty)" $ do
      forest2BiTree [Node 3 (Node 4 Empty Empty) (Node 5 Empty Empty), Node 6 Empty Empty] `shouldBe` Node 3 (Node 4 Empty (Node 5 Empty Empty)) (Node 6 Empty Empty)
{-
    it "The result of stringToBiTree () is Empty" $ do
      stringToBiTree () `shouldBe` Empty
    it "The result of stringToBiTree "(1,(),(2,(),()))" is (Node 1 Empty (Node 2 Empty Empty))" $ do
      stringToBiTree "(1,(),(2,(),()))" `shouldBe` (Node 1 Empty (Node 2 Empty Empty) :: BiTree Int)
 -}
    it "The result of jaccardSimIndex [] [] is 1.0" $ do
      jaccardSimIndex [] ([] :: [Int]) `shouldBe` 1.0
    it "The result of jaccardSimIndex [] (['a'] :: [Char]) is 0.0" $ do
      jaccardSimIndex [] (['a'] :: [Char]) `shouldBe` 0.0
    it "The result of jaccardSimIndex [] [] is 0.0" $ do
      jaccardSimIndex (['a'] :: [Char]) [] `shouldBe` 0.0
    it "The result of jaccardSimIndex ['a'] [] is 0.25" $ do
      jaccardSimIndex (['a','b'] :: [Char]) ['a','c','d'] `shouldBe` 0.25
    it "The result of jaccardSimIndex' ['a'] [['a','b'],['a','b','c','d']] is 0.375" $ do
      jaccardSimIndex'  ['a'] [['a','b'],['a','b','c','d']] `shouldBe` 0.375
    it "The result of jaccardSimIndex' ['a'] [[]] is 0.0" $ do
      jaccardSimIndex' ['a'] [[]] `shouldBe` 0.0
    it "The result of jaccardSimIndex' [] [[]] is 1.0" $ do
      jaccardSimIndex' ([] :: [Int]) [[]] `shouldBe` 1.0
