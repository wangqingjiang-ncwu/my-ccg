{-# LANGUAGE OverloadedStrings, LambdaCase #-}

-- Copyright (c) 2019-2023 China University of Water Resources and Electric Power
-- All rights reserved.

module Clustering (
    PhraSyn,           -- (Category, Tag, PhraStru)
    distPhraSyn,       -- PhraSyn -> PhraSyn -> Int
    distPhraSynSet,    -- [PhraSyn] -> [PhraSyn] -> Float
    distVect4StruGene,         -- StruGene -> StruGene -> [Float]
    dist4StruGeneByArithAdd,   -- StruGene -> StruGene -> Float
    dist4StruGeneByNormArithMean       -- StruGene -> StruGene -> Float
    ) where

import Category
import Phrase
import AmbiResol
import Utils
import Data.Tuple.Utils

-- Syntactic attribues of a phrase, including its syntactic category, tag of rule by which the phrase is obtained, and structural type of the phrase.
type PhraSyn = (Category, Tag, PhraStru)

-- The distance between two phrases
distPhraSyn :: PhraSyn -> PhraSyn -> Int
distPhraSyn p1 p2 = foldl (+) 0 [v1, v2, v3]
    where
    ca1 = fst3 p1
    ta1 = snd3 p1
    ps1 = thd3 p1
    ca2 = fst3 p2
    ta2 = snd3 p2
    ps2 = thd3 p2
    v1 = case ca1==ca2 of
           True -> 0
           False -> 1
    v2 = case ta1==ta2 of
           True -> 0
           False -> 1
    v3 = case ps1==ps2 of
           True -> 0
           False -> 1

{- The distance between two phrase sets.
 - Phrase set 1 = [p1, p2, ..., pn], phrase set 2 = [q1, q2, ..., qm]
 - dij = distPhraSyn pi qj
 - return the average dij among i<-[1..n] and j<-[1..m].
 -}
distPhraSynSet :: [PhraSyn] -> [PhraSyn] -> Float
distPhraSynSet ps qs = fromIntegral distSum / fromIntegral distNum              -- fromIntegral :: Int -> Float
    where
    distSet = [distPhraSyn pi qj | pi <- ps, qj <- qs]
    distSum = foldl (+) 0 distSet
    distNum = length distSet

{- The distance vector between two StruGenes.
 - For ambiguity model StruGene = (LeftExtend, LeftOver, RightOver, RightExtend, OverType, Prior), the distance vector is obtained by following function.
 -}
distVect4StruGene :: StruGene -> StruGene -> [Float]
distVect4StruGene s1 s2 = [d1, d2, d3, d4, d5, d6]
    where
    d1 = distPhraSynSet (fst6 s1) (fst6 s2)
    d2 = fromIntegral $ distPhraSyn (snd6 s1) (snd6 s2)
    d3 = fromIntegral $ distPhraSyn (thd6 s1) (thd6 s2)
    d4 = distPhraSynSet (fth6 s1) (fth6 s2)
    d5 = case fif6 s1 == fif6 s2 of
            True -> 0.0
            False -> 1.0
    d6 = case sth6 s1 == sth6 s2 of
            True -> 0.0
            False -> 1.0

{- Arithmetically added distance between two StruGenes.
 -}
dist4StruGeneByArithAdd :: StruGene -> StruGene -> Float
dist4StruGeneByArithAdd s1 s2 = foldl (+) 0.0 $ distVect4StruGene s1 s2

{- Normalized arithmetically meaned distance between two StruGenes.
 -}
dist4StruGeneByNormArithMean :: StruGene -> StruGene -> Float
dist4StruGeneByNormArithMean s1 s2 = (\x-> x / 6.0) $ foldl (+) 0.0 $ map (\x -> fst x / snd x) $ zip (distVect4StruGene s1 s2) [3.0, 3.0, 3.0, 3.0, 1.0, 1.0]
