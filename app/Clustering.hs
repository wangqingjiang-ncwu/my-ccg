{-# LANGUAGE OverloadedStrings, LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

-- Copyright (c) 2019-2025 China University of Water Resources and Electric Power
-- All rights reserved.
-- This module was originally written by Qian-qian WANG at 2023. The original goal was to cluster the samples of ambiguity resolution.
-- To complete clustering, the distances between phrases shoule be defined firstly. Unfortunately it is not a simple problem.
-- To evaluate similarity between two values of every grammatic attribute, mutual explanation was proposed by Qing-jiang WANG at 2024 autumn.

module Clustering (
    distPhraSynByIdentity,     -- PhraSyn -> PhraSyn -> Double
    distPhraSynSetByIdentity,  -- [PhraSyn] -> [PhraSyn] -> Double
    distVect4StruGeneByIdentity,         -- StruGene -> StruGene -> [Double]
    distVect4ContextOfSGByIdentity,      -- ContextOfSG -> ContextOfSG -> [Double]
    DistWeiRatioList,          -- [Int], namely [wle, wlo, wro, wre, wot, wpr]
    dist4StruGeneByWeightSum,  -- StruGene -> StruGene -> DistWeiRatioList -> Double
    dist4ContextOfSGByWeightSum,         -- ContextOfSG -> ContextOfSG -> DistWeiRatioList -> Double
    dist4StruGeneByArithMean,  -- StruGene -> StruGene -> Double
    StruGene,                  -- (LeftExtend, LeftOver, RightOver, RightExtend, OverType, Prior)
    getKModeByMaxMinPoint,     -- [StruGeneSample] -> [StruGeneSample] -> Map.Map SIdx Double -> Int -> [StruGene]
    Dist,                      -- Double
    DistAlgo,                  -- String
    SIdxDistTuple,             -- (SIdx, Dist)
    SIdxDistList,              -- [SIdxDistTuple]
    findDistMinValOfTwoTupleLists,       -- SIdxDistList -> SIdxDistList -> SIdxDistList -> SIdxDistList
    SampleClusterMark,                   -- (StruGeneSample,DMin,CIdx,StruGene,INo)
    findCluster4ASampleByArithAdd,       -- StruGene -> [StruGene] -> Int -> DistWeiRatioList -> SampleClusterMark
    findCluster4AllSamplesByArithAdd,    -- [StruGene] -> [StruGene] -> KVal -> INo -> [SampleClusterMark] -> DistWeiRatioList -> [SampleClusterMark]
--  findCluster4ASampleByNormArithMean,  -- StruGene -> [StruGene] -> Int -> SampleClusterMark
    divideSameCluster,                   -- [SampleClusterMark] -> Map.Map CIdx [StruGene] -> Map.Map CIdx [StruGene]
    updateCentre4ACluster,               -- [StruGene] -> StruGene
    getMode4List,                        -- Ord a => [a] -> Map.Map a Freq -> a
    getFreqMap,                          -- Ord a => [a] -> Map.Map a Freq -> Map.Map a Freq
    distTotalByClust,                    -- [StruGene] -> [StruGene] -> Double -> Double
    DistMean,                            -- Double
    SIdx,                                -- Int
    ClusterMap,                          -- Map CIdx [StruGene]
    findFinalCluster4AllSamplesByArithAdd,    -- TableName -> ClusterMap -> CentreList -> KVal -> INo -> DistTotal -> DistWeiRatioList -> IO (INo, Dist)
    readStreamByInt324TextInt8Text,      -- [StruGeneSample] -> S.InputStream [MySQLValue] -> IO [StruGeneSample]
    storeOneIterationResult,             -- TableName -> INo -> [SampleClusterMark] -> [StruGene] -> DistMean -> IO ()
    storeClusterTime,                    -- TimeTableName -> KVal -> SNum -> NominalDiffTime -> NominalDiffTime -> Int -> Double -> IO ()
    autoRunClustByChangeKValSNum,        -- String -> String -> Int -> KValRange -> SNumRange -> DistWeiRatioList -> IO ()
    autoRunGetAmbiResolAccuracyOfAllClustRes, -- String -> String -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> IO ()
    getRandomsList,                      -- Int -> Int -> Int -> [Int] -> IO [Int]
    getAmbiResolAccuracyOfAClustRes,     -- IO Double
    getStruGeneSamples,                  -- IO [StruGeneSample]
    getStruGene2Samples,                 -- IO [StruGene2Sample]

    clusteringAnalysis,      -- Int -> IO ()
    SentClauPhraList,        -- [[[PhraCate]]]
    SimDeg,                  -- Double
    getTypePair2SimFromSCPL,         -- SentClauPhraList -> (NumOfPhraSyn, NumOfCate, NumOfCatePair, [((Category, Category), SimDeg)])
    getTypePair2SimFromPSL,          -- [PhraSyn] -> (NumOfPhraSyn, NumOfCate, NumOfCatePair, [((Category, Category), SimDeg)])
    getTagPair2SimFromSCPL,          -- SentClauPhraList -> (NumOfPhraSyn, NumOfTag, NumOfTagPair, [((Tag, Tag), SimDeg)])
    getTagPair2SimFromPSL,           -- [PhraSyn] -> (NumOfPhraSyn, NumOfTag, NumOfTagPair, [((Tag, Tag), SimDeg)])
    getStruPair2SimFromSCPL,         -- SentClauPhraList -> (NumOfPhraSyn, NumOfPhraStru, NumOfStruPair, [((PhraStru, PhraStru), SimDeg)])
    getStruPair2SimFromPSL,          -- [PhraSyn] -> (NumOfPhraSyn, NumOfPhraStru, NumOfStruPair, [((PhraStru, PhraStru), SimDeg)])
    getSpanPair2SimFromSCPL,         -- SentClauPhraList -> (NumOfPhraSyn, NumOfSpan, NumOfSpanPair, [((Span, Span), SimDeg)])
    getSpanPair2SimFromPSL,          -- [PhraSyn] -> (NumOfPhraSyn, NumOfSpan, NumOfSpanPair, [((Span, Span), SimDeg)])
    getPhraSynFromSents,             -- SentClauPhraList -> [PhraSyn] -> [PhraSyn]
    stringToPhraSyn,                 -- String -> PhraSyn
    getPhraSynPairSimFromSCPLBySVD,  -- [(PhraSyn, PhraSyn)] -> IO ([(PhraSyn, PhraSyn)], Matrix Double, Matrix Double, Matrix Double, [((PhraSyn, PhraSyn), SimDeg)])
    getPhraSynPairSimFromSCPL,       -- SentClauPhraList -> IO ([(PhraSyn, PhraSyn)], Matrix Double, [((PhraSyn, PhraSyn), SimDeg)])
    getPhraSynPairSimBySVD,   -- DistAlgo -> [(PhraSyn, PhraSyn)] -> IO ([(PhraSyn, PhraSyn)], Matrix Double, Matrix Double, Matrix Double, [((PhraSyn, PhraSyn), SimDeg)])
    getPhraSynPairSim,        -- DistAlgo -> [(PhraSyn, PhraSyn)] -> IO ([(PhraSyn, PhraSyn)], Matrix Double, [((PhraSyn, PhraSyn), SimDeg)])
    getPhraSynPair2SimFromTreebank,  -- SentIdx -> SentIdx -> IO (Map (PhraSyn, PhraSyn) SimDeg)
    getPhraSynPair2SimFromCOT,       -- DistAlgo -> [ContextOfOT] -> IO (Map (PhraSyn, PhraSyn) SimDeg)
    getPhraSynPair2SimFromCSG,       -- DistAlgo ->[ContextOfSG] -> IO (Map (PhraSyn, PhraSyn) SimDeg)
    getPhraSynSetSim,                -- [PhraSyn] -> [PhraSyn] -> Map (PhraSyn, PhraSyn) SimDeg -> SimDeg
    toPhraSynPair2Sim,               -- Map (PhraSyn, PhraSyn) SimDeg -> (PhraSyn, PhraSyn) -> ((PhraSyn, PhraSyn), SimDeg)
    getSentClauPhraList,             -- SentIdx -> SentIdx -> IO SentClauPhraList

    getContext2OverTypeBase,         -- SIdx -> SIdx -> IO Context2OverTypeBase
    getContextOfOTPairSimBySVD,      -- Context2OverTypeBase -> ([(ContextOfOT, ContextOfOT)], Matrix Double, Matrix Double, [((ContextOfOT, ContextOfOT), SimDeg)])
    getContextOfOTPairSim,           -- Context2OverTypeBase -> IO ([(SimDeg,SimDeg,SimDeg,SimDeg)], [((ContextOfOT, ContextOfOT), SimDeg)])
    getOverTypePair2Sim,             -- SIdx -> SIdx -> [((OverType, OverType), SimDeg)]
    getContext2ClauTagPriorBase,     -- SIdx -> SIdx -> IO [Context2ClauTagPrior]
    getSIdx2ContextOfSGBase,         -- SIdx -> SIdx -> IO [(SIdx, ContextOfSG)]
    getContextOfSGPairSimBySVD,      -- Context2ClauTagPriorBase -> IO ([(ContextOfSG, ContextOfSG)], Matrix Double, Matrix Double, [((ContextOfSG, ContextOfSG), SimDeg)])
    getContextOfSGPairSim,           -- Context2ClauTagPriorBase -> IO ([(SimDeg, SimDeg, SimDeg, SimDeg, SimDeg)], [((ContextOfSG, ContextOfSG), SimDeg)])
    getOneContextOfSGPairSim,        -- ContextOfSG -> ContextOfSG -> Map (PhraSyn, PhraSyn) SimDeg -> IO SimDeg
    getContextOfSGPairSimWithStaticOT,     -- Context2ClauTagPriorBase -> IO ([(SimDeg, SimDeg, SimDeg, SimDeg, SimDeg)], [((ContextOfSG, ContextOfSG), SimDeg)])
    getOneToAllContextOfSGSim,       -- ContextOfSG -> Context2ClauTagPriorBase -> IO ([(SimDeg, SimDeg, SimDeg, SimDeg, SimDeg)], [((ContextOfSG, ContextOfSG), SimDeg)])
    getOneToAllContextOfSGSim',      -- ContextOfSG -> Context2ClauTagPriorBase -> IO ([(SimDeg, SimDeg, SimDeg, SimDeg, SimDeg)], [((ContextOfSG, ContextOfSG), SimDeg)])
    findStruGeneSampleByMaxContextSim,     -- ContextOfSG -> Context2ClauTagPriorBase -> IO (SIdx, SimDeg, Context2ClauTagPrior)
    findWhereSim1HappenAmongStruGeneSamples,   -- SIdx -> Int -> [Context2ClauTagPrior] -> IO ()

    readStaticTypePair2Sim,     -- IO [((Category, Category), SimDeg)]
    readStaticTagPair2Sim,      -- IO [((Tag, Tag), SimDeg)]
    readStaticStruPair2Sim,     -- IO [((Stru, Stru), SimDeg)]
    readStaticSpanPair2Sim,     -- IO [((Span, Span), SimDeg)]
    readStaticPhraSynPair2Sim,  -- IO [((PhraSyn, PhraSyn), SimDeg)]
    ) where

import Category
import Phrase
import AmbiResol
import qualified AmbiResol as AR
import Corpus
import Utils
import Output
import Data.Tuple (swap)
import Data.Tuple.Utils
import Data.List
import Data.Time.Clock
import Database
import Database.MySQL.Base
import System.IO
import Text.Printf
import qualified System.IO.Streams as S
import qualified Data.String as DS
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import System.Random
import Control.Monad
import Numeric.LinearAlgebra.Data hiding (find)
import Numeric.LinearAlgebra as LA hiding (find)
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import Data.Time.Clock

{- The distance between two phrases, where only grammatical factors are considered,
 - and the correlations between different values of an identical factor and between different factors are both neglected.
 - Actually, some categories such 'np' and 's\.np' may be converted for some syntactic requirements, and phrasal
 - structures such as 'AHn' and 'HnC' have same grammatical nature, which means there may exist correlations bewteen
 - the different values of a same factor. Meanwhile, phrasal structure 'AHn' must have category 'np', so there exist
 - correlations between some different factors.
 - The distance is normalized to [0, 1].
 -}
distPhraSynByIdentity :: PhraSyn -> PhraSyn -> Double
distPhraSynByIdentity (ca1, ta1, ps1, sp1) (ca2, ta2, ps2, sp2) = sum [v1, v2, v3, v4] / 4.0
    where
    v1 = case ca1 == ca2 of
           True -> 0
           False -> 1
    v2 = case ta1 == ta2 of
           True -> 0
           False -> 1
    v3 = case ps1 == ps2 of
           True -> 0
           False -> 1
    v4 = case sp1 == sp2 of
           True -> 0
           False -> 1

{- Calculate the distance between two phrase sets by element pairing between two sets.
 - Phrase set P = [p1, p2, ..., pn], phrase set Q = [q1, q2, ..., qm]
 - sim(pi, pj) = 1 - (distPhraSynByIdentity pi qj)
 - From high to low, select sim(pi, pj) where i<-[1..n] and j<-[1..m], namely pi and qj are a pair of elements.
 - Without loss of generality, if set P has a bigger cardinality than set Q, the redundant elements in set P pairing with null elements.
 - Calculate the mean of similarity degrees of all phrasal pairs, then convert into the final distance.
 -}
distPhraSynSetByIdentity :: [PhraSyn] -> [PhraSyn] -> Double
distPhraSynSetByIdentity ps qs = 1 - (getPhraSynSetSim ps qs phraSynPair2SimMap)
    where
    phraSynPair2SimMap = Map.fromList [((pi, qj), 1 - (distPhraSynByIdentity pi qj)) | pi <- ps, qj <- qs]  -- [((pi,qj),sim(pi,qj))]

{- The distance vector between two samples of model StruGene.
 - For ambiguity model StruGene = (LeftExtend, LeftOver, RightOver, RightExtend, OverType, Prior), the distance vector is obtained by following function.
 - Elements in distance vector all are in [0, 1].
 -}
distVect4StruGeneByIdentity :: StruGene -> StruGene -> [Double]
distVect4StruGeneByIdentity s1 s2 = [d1, d2, d3, d4, d5, d6]
    where
    d1 = distPhraSynSetByIdentity (fst6 s1) (fst6 s2)
    d2 = distPhraSynByIdentity (snd6 s1) (snd6 s2)
    d3 = distPhraSynByIdentity (thd6 s1) (thd6 s2)
    d4 = distPhraSynSetByIdentity (fth6 s1) (fth6 s2)
    d5 = case fif6 s1 == fif6 s2 of
            True -> 0.0
            False -> 1.0
    d6 = case sth6 s1 == sth6 s2 of
            True -> 0.0
            False -> 1.0

{- The distance vector between two StruGene contexts or two StruGene2 contexts.
 - ContextOfSG :: (LeftExtend, LeftOver, RightOver, RightExtend, OverType), the distance vector is obtained by following function.
 - Elements in distance vector all are in [0, 1].
 -}
distVect4ContextOfSGByIdentity :: ContextOfSG -> ContextOfSG -> [Double]
distVect4ContextOfSGByIdentity c1 c2 = [d1, d2, d3, d4, d5]
    where
    d1 = distPhraSynSetByIdentity (fst5 c1) (fst5 c2)
    d2 = distPhraSynByIdentity (snd5 c1) (snd5 c2)
    d3 = distPhraSynByIdentity (thd5 c1) (thd5 c2)
    d4 = distPhraSynSetByIdentity (fth5 c1) (fth5 c2)
    d5 = case fif5 c1 == fif5 c2 of
            True -> 0.0
            False -> 1.0

-- Weigth ratio list of distances on StruGene elements between two StruGene samples.
type DistWeiRatioList = [Int]     -- [wle, wlo, wro, wre, wot, wpr]

{- Calculate distance between two StruGenes.
 - (1) Weightedly sum distances on all StruGene components;
 - (2) Normalize to range [0, 1].
 - This function is obsoleted.
 -}
dist4StruGeneByWeightSum :: StruGene -> StruGene -> DistWeiRatioList -> Double
dist4StruGeneByWeightSum s1 s2 distWeiRatioList = sum distList / fromIntegral ratioTotal
    where
    distList = map (\x -> fromIntegral (fst x) * snd x) $ zip distWeiRatioList $ distVect4StruGeneByIdentity s1 s2
    ratioTotal = sum distWeiRatioList

{- Calculate distance between contexts of two StruGene samples.
 - (1) Weightedly sum distances only on StruGene context components;
 - (2) Normalize to range [0, 1].
 -}
dist4ContextOfSGByWeightSum :: ContextOfSG -> ContextOfSG -> DistWeiRatioList -> Double
dist4ContextOfSGByWeightSum c1 c2 distWeiRatioList = sum distList / fromIntegral ratioTotal
    where
    distList = map (\x -> fromIntegral (fst x) * snd x) $ zip distWeiRatioList $ distVect4ContextOfSGByIdentity c1 c2
    ratioTotal = sum distWeiRatioList

{- Arithmetically meaned distance between two StruGenes.
 -}
dist4StruGeneByArithMean :: StruGene -> StruGene -> Double
dist4StruGeneByArithMean s1 s2 = (\x-> x / 6.0) $ sum (distVect4StruGeneByIdentity s1 s2)

{- 初始化随机选取1个点,初始定k=200

type StruGene' = ([LeftExtend], [LeftOver], [RightOver], [RightExtend], [OverType], [Prior])
type StruGene1 = StruGene'
--data sumClust = 200
-}

{- 计算每个样本到各簇心的距离的最小值mvList。
   样本：(x:xs)
   给定的簇心列表：sg

minValueList :: [StruGene] -> [StruGene] -> [Double] -> [Double]
minValueList [] _ mvList = mvList
minValueList _ [] mvList = mvList
minValueList (x:xs) sg mvList = minValueList xs sg mvList1
    where
    mi = minimum $ map (\x -> dist4StruGeneByWeightSum (fst x) (snd x) ) $ zip (replicate (length sg) x) sg
    mvList1 = mvList ++ [mi]

-- type initPointSet = ([StruGene],[StruGene],[Double])

  初始化选取k个众心点，用maxmin算法.
   找出最小值列表中最大的值，并记录序号；sgs中该序号的元素即为新找的中心点，
   从sgs删除该点，向ms添加该点。
   要求ms的初值是含有一个众心的列表。


getKModeByMaxMinPoint :: [StruGene] -> [StruGene] -> Int -> [StruGene]
getKModeByMaxMinPoint sgs ms kVal
   | length ms2 == kVal = ms2
   | otherwise = getKModeByMaxMinPoint sgs2 ms2 kVal
    where
    mvList = minValueList sgs ms []
    ma = maximum mvList                                      -- mvList is not empty.
    idx = elemIndex ma mvList
    idx' = case idx of
             Just x -> x
             Nothing -> -1                                    -- It is not impossible.
    ge = sgs!!idx'
    sgs2 = delete ge sgs
    ms2 =  ms ++ [ge]
-}

{- Find inital modes.
 - sgs: sample set.
 - oldModeList: original modes.
 - origSIdxMinValueMap: original map from samples to minimal distances.
 - kVal: the number of clusters.
 - distWeiRatioList: weigth list of distances on StruGene elements between two StruGene samples.
 -}
getKModeByMaxMinPoint :: [StruGeneSample] -> [StruGeneSample] -> Map.Map SIdx Double -> Int -> DistWeiRatioList -> [StruGene]
getKModeByMaxMinPoint sgs oldModeList origSIdxMinValueMap kVal distWeiRatioList
    | length oldModeList == kVal = map (\x -> (snd7 x, thd7 x, fth7 x, fif7 x, sth7 x, svt7 x)) oldModeList
    | otherwise = getKModeByMaxMinPoint sgs' newModeList newSIdxMinValueMap kVal distWeiRatioList
    where
    lastMode = last oldModeList
    lastModeSIdx = fst7 (lastMode)
    lastModeStruGene = (snd7 lastMode, thd7 lastMode, fth7 lastMode, fif7 lastMode, sth7 lastMode, svt7 lastMode)
    li = Map.toList $ Map.delete lastModeSIdx origSIdxMinValueMap
    distListOfSamplesToLastMode = map (\x -> (fst7 x, dist4StruGeneByWeightSum lastModeStruGene (snd7 x, thd7 x, fth7 x, fif7 x, sth7 x, svt7 x) distWeiRatioList)) sgs
    newSIdxMinValueList = findDistMinValOfTwoTupleLists distListOfSamplesToLastMode li []
    newSIdxMinValueMap = Map.fromList newSIdxMinValueList
    newModeSIdx = snd $ Map.findMax $ Map.fromList $ map (\x ->(snd x, fst x)) newSIdxMinValueList
    newMode = find (\x -> fst7 x == newModeSIdx) sgs
    newMode' = case newMode of
                 Just x -> x
                 Nothing -> nullStruGeneSample
    sgs' = delete newMode' sgs
    newModeList = oldModeList ++ [newMode']                                     -- Actually, newMode' is not Null.

type Dist = Double
type DistAlgo = String                  -- Distance algorithm name, such as "Euclidean", "Manhattan", and so on.
type SIdxDistTuple = (SIdx, Dist)
type SIdxDistList = [SIdxDistTuple]

{- find the shorter distance of every sample
   sdv1 : sample distance vector
   sdv2 : sample distance vector
   sIdxMinValuList : sample distance vector storing the smaller distances
-}
findDistMinValOfTwoTupleLists :: SIdxDistList -> SIdxDistList -> SIdxDistList -> SIdxDistList
findDistMinValOfTwoTupleLists [] _ sIdxMinValuList = sIdxMinValuList
findDistMinValOfTwoTupleLists sdv1 [] sIdxMinValuList = sdv1
findDistMinValOfTwoTupleLists sdv1 sdv2 sIdxMinValuList = findDistMinValOfTwoTupleLists sdv1' sdv2 sIdxMinValuList'
    where
    sIdxSdv1FirstElem = fst (head sdv1)
    distSdv1FirstElem = snd (head sdv1)
    sdv1' = tail sdv1
    sdv2Dist4SameSIdx = lookup sIdxSdv1FirstElem sdv2
    sdv2Dist4SameSIdx' = case sdv2Dist4SameSIdx of
                    Just x -> x
                    Nothing -> 1000              -- Maximum distance from sample to mode
    minValue = min distSdv1FirstElem sdv2Dist4SameSIdx'
--    if distL1FirstElem > l2Dist4SameSIdx'
--       then let minValue = l2Dist4SameSIdx'
--       else let minValue = distL1FirstElem
    sIdxMinValuList' = sIdxMinValuList ++ [(sIdxSdv1FirstElem, minValue)]

type DMin = Double                                        -- Minimum distance of a sample to all cluster centres.
type CIdx = Int                                          -- Index of a cluster which a sample is attributed to.
type INo= Int                                            -- The order number of an iteration.
type SampleClusterMark = (StruGeneSample,DMin,CIdx,StruGene,INo)        -- The result of clustering for a sample.
type KVal = Int                                          -- The number of clusters.

{- 最小距离（算术加）聚类：计算一个点（包括聚类中心）和k个中心点的距离，聚类到最近的一簇,
 sp :一个样本(sample)
 ccl: 簇心列表(cluster centre list)
 kVal: 簇的个数
 distWeiRatioList: weigth list of distances on StruGene elements between two StruGene samples.
 elemIndex d cs：查找cs表中值为d的索引值
 -}
findCluster4ASampleByArithAdd :: StruGeneSample -> [StruGene] -> KVal -> INo -> DistWeiRatioList -> SampleClusterMark
findCluster4ASampleByArithAdd sp ccl kVal iNo distWeiRatioList = (sp, dMin, cIdx', ge, iNo)
    where
    sps = (snd7 sp, thd7 sp, fth7 sp, fif7 sp, sth7 sp, svt7 sp)
    ds = map (\x -> dist4StruGeneByWeightSum sps x distWeiRatioList) ccl
    dMin = minimum ds
    cIdx = elemIndex dMin ds
    cIdx' = case cIdx of
              Just x -> x
              Nothing -> -1        -- Impossible
    ge = ccl!!cIdx'

{- 最小距离（算术加）聚类：计算每个点（包括聚类中心）和k个中心点的距离，将其归入到距离它最近的那一簇,
 (x:xs)所有样本
 ccl: 簇心列表(cluster centre list)
 kVal: 簇的个数
 iNo:迭代序号
 distWeiRatioList: weigth list of distances on StruGene elements between two StruGene samples.
 -}
findCluster4AllSamplesByArithAdd :: [StruGeneSample] -> [StruGene] -> KVal -> INo -> [SampleClusterMark] -> DistWeiRatioList -> [SampleClusterMark]
findCluster4AllSamplesByArithAdd [] _ _ _ origClusterMark _ = origClusterMark
findCluster4AllSamplesByArithAdd (x:xs) ccl kVal iNo origClusterMark distWeiRatioList = findCluster4AllSamplesByArithAdd xs ccl kVal iNo newClusterMark distWeiRatioList
    where
    xCM = findCluster4ASampleByArithAdd x ccl kVal iNo distWeiRatioList
    newClusterMark = origClusterMark ++ [xCM]

{- 最小距离（标准平均距离）聚类：计算每个点（包括聚类中心）和k个中心点的距离，聚类到最近的一簇,k是一个列表

findCluster4ASampleByNormArithMean :: StruGeneSample -> [StruGene] -> Int -> SampleClusterMark
findCluster4ASampleByNormArithMean sp ccl kVal = (sp, dMin, cIdx', ge, 0)
    where
    ds = map (\x -> dist4StruGeneByArithMean (fst x) (snd x)) $ zip (replicate kVal sp) ccl
    dMin = minimum ds
    cIdx = elemIndex dMin ds
    cIdx' = case cIdx of
           Just x -> x
           Nothing -> -1        -- Impossible
    ge = ccl!!cIdx'
 -}

{- 按簇号CIdx把所有样本划分成簇
 SampleClusterMark : (StruGeneSample,DMin,CIdx,StruGene,INo)
-}
divideSameCluster :: [SampleClusterMark] -> Map.Map CIdx [StruGeneSample] -> Map.Map CIdx [StruGeneSample]
divideSameCluster [] origMap = origMap
divideSameCluster (x:xs) origMap = divideSameCluster xs newMap
    where
    cIdxSp = (thd5 x, fst5 x)
    newMap = Map.insertWith (++) (fst cIdxSp) [snd cIdxSp] origMap

-- type Stru_Gene1 = ([LeftExtend], [LeftOver], [RightOver], [RightExtend], [OverType], [Prior])

{- 更新每簇的中心点(即众心)
   (x:xs)表示该簇的所有样本（包括中心点）
   (lE,lO,rO,rE,oT,pr)开始时每个元素为空

-}
{- updateCentre4ACluster :: [StruGene] -> StruGene1 -> StruGene
updateCentre4ACluster [] _ = nullStruGene                     -- No sample in this cluster.
updateCentre4ACluster [x] _ = x                               -- Only one sample in this cluster.
updateCentre4ACluster (x:xs) (lE,lO,rO,rE,oT,pr)              -- There are more than two samples.
    | length xs > 1 = updateCentre4ACluster xs (lE1,lO1,rO1,rE1,oT1,pr1)
    | otherwise = (leMode, loMode, roMode, reMode, otMode, prMode)
    where
    hxs = head xs
    lE1 = (fst6 x) : lE
    lO1 = (snd6 x) : lO
    rO1 = (thd6 x) : rO
    rE1 = (fth6 x) : rE
    oT1 = (fif6 x) : oT
    pr1 = (sth6 x) : pr
    leSetList = fromListList2SetList ((fst6 hxs):lE1)
    leMode = Set.toDescList $ getMode4PhraSynSetList leSetList (getFreqMap leSetList Map.empty)
    loList = ((snd6 hxs):lO1)
    loMode = getMode4PhraSynList loList (getFreqMap loList Map.empty)
    roList = ((thd6 hxs):rO1)
    roMode = getMode4PhraSynList roList (getFreqMap roList Map.empty)
    reSetList = fromListList2SetList ((fth6 hxs):rE1)
    reMode = Set.toDescList $ getMode4PhraSynSetList reSetList (getFreqMap reSetList Map.empty)
    otList = ((fif6 hxs):oT1)
    otMode = getMode4OTList otList (getFreqMap otList Map.empty)
    prList = ((sth6 hxs):pr1)
    prMode = getMode4List prList (getFreqMap prList Map.empty)
-}

updateCentre4ACluster :: [StruGene] -> StruGene
updateCentre4ACluster [] = nullStruGene                     -- No sample in this cluster.
updateCentre4ACluster [x] = x                               -- Only one sample in this cluster.
updateCentre4ACluster sgs = (leMode, loMode, roMode, reMode, otMode, prMode)
    where
    lEList = fromListList2SetList (map fst6 sgs)
    lOList = map snd6 sgs
    rOList = map thd6 sgs
    rEList = fromListList2SetList (map fth6 sgs)
    oTList = map fif6 sgs
    prList = map sth6 sgs
    leMode = Set.toDescList $ getMode4PhraSynSetList lEList (getFreqMap lEList Map.empty)
    loMode = getMode4PhraSynList lOList (getFreqMap lOList Map.empty)
    roMode = getMode4PhraSynList rOList (getFreqMap rOList Map.empty)
    reMode = Set.toDescList $ getMode4PhraSynSetList rEList (getFreqMap rEList Map.empty)
    otMode = getMode4OTList oTList (getFreqMap oTList Map.empty)
    prMode = getMode4List prList (getFreqMap prList Map.empty)

type AClusterSamples = [StruGeneSample]
type CentreList = [StruGene]

{- 更新所有簇的中心点(即众心)
   (x:xs)表示所有簇的样本，x表示一个簇的所有样本
   (lE,lO,rO,rE,oT,pr)开始时每个元素为空
-}
updateCentre4AllCluster :: [AClusterSamples] -> CentreList -> CentreList
updateCentre4AllCluster [] origCentreList = origCentreList
updateCentre4AllCluster (x:xs) origCentreList = updateCentre4AllCluster xs newCentreList
    where
    xSg = map (\y -> (snd7 y, thd7 y, fth7 y, fif7 y, sth7 y, svt7 y)) x
    xCentre = updateCentre4ACluster xSg
    newCentreList = origCentreList ++ [xCentre]

{- FreOfPhraSyn: PhraSyn三元组的频次的集合
-}
type Freq = Int      -- 频次
type PhraSynSet = Set.Set PhraSyn
type LeftExtend = PhraSynSet
type RightExtend = PhraSynSet
type FreqOfPhraSynSetMap = Map.Map (Set.Set PhraSyn) Freq
type FreqOfPhraSynMap = Map.Map PhraSyn Freq
type FreqOfOTMap = Map.Map OverType Freq

{-计算三元组集合的列表的众数
-}
getMode4PhraSynSetList :: [PhraSynSet] -> FreqOfPhraSynSetMap -> PhraSynSet
getMode4PhraSynSetList [] _ = Set.empty
getMode4PhraSynSetList [x] _ = x
getMode4PhraSynSetList phraSynSetList freqOfPhraSynSetMap = mode
    where
    freqOfPhraSynSetList = Map.toList freqOfPhraSynSetMap
    freqList = map snd freqOfPhraSynSetList
    maxFreq = maximum freqList
    idx = elemIndex maxFreq freqList
    idx' = maybe (-1) (+0) idx
    mode = fst (freqOfPhraSynSetList!!idx')

{- The following function converts a list of lists to a list of sets,
 - especially used to convert a value with type [[PhraSyn]] to a value with type [PhraSynSet].
 -}
fromListList2SetList :: Ord a => [[a]] -> [Set.Set a]
fromListList2SetList [] = []
fromListList2SetList (x:xs) = (Set.fromList x):(fromListList2SetList xs)

{-求PhraSyn三元组列表的一个众心（可能有多个）.
  PhraSynList: PhraSyn列表
  freqOfPhraSynMap: 短语句法三元组到频次的映射字典.
-}
getMode4PhraSynList :: [PhraSyn] -> FreqOfPhraSynMap -> PhraSyn
getMode4PhraSynList [] _ = nullPhraSyn
getMode4PhraSynList [x] _ = x
getMode4PhraSynList phraSynList freqOfPhraSynMap = mode
    where
    freqOfPhraSynList = Map.toList freqOfPhraSynMap
    freqList = map snd freqOfPhraSynList
    maxFreq = maximum freqList
    idx = elemIndex maxFreq freqList
    idx' = maybe (-1) (+0) idx               -- For idx == Just i, return i; For idx == Nothing, return -1.
    mode = fst (freqOfPhraSynList!!idx')

{-求列表的一个众数（可能有多个）.
  list: 列表[k]
  kFreqMap: 频次字典
-}
getMode4List :: Ord a => [a] -> Map.Map a Freq -> a
getMode4List [] _ = error "getMode4List: List is empty."
getMode4List [x] _ = x
getMode4List list kFreqMap = mode
    where
    kFreqList = Map.toList kFreqMap
    freqList = map snd kFreqList
    maxFreq = maximum freqList
    idx = elemIndex maxFreq freqList
    idx' = maybe (-1) (+0) idx
    mode = fst (kFreqList!!idx')

{- 求PhraSyn三元组的频次
 - (x:xs): PhraSyn列表
 - origFreqOfPhraSyn: 原PhraSyn的频次“字典”
 -}
getPhraSynFreqMap :: [PhraSyn] -> Map.Map PhraSyn Freq -> Map.Map PhraSyn Freq
getPhraSynFreqMap [] origFreqOfPhraSyn = origFreqOfPhraSyn
getPhraSynFreqMap (x:xs) origFreqOfPhraSyn = getPhraSynFreqMap xs newFreqOfPhraSyn
    where
    newFreqOfPhraSyn = Map.insertWith (+) x 1 origFreqOfPhraSyn

{- 求类型K的值出现的频次，即建立一个Map K V.
 - (x:xs): a值的列表
 - origMap: 原Map a Freq
 -}
getFreqMap :: Ord a => [a] -> Map.Map a Freq -> Map.Map a Freq
getFreqMap [] origMap = origMap
getFreqMap (x:xs) origMap = getFreqMap xs newMap
    where
    newMap = Map.insertWith (+) x 1 origMap


{- 删除列表中的所有x

deleteAllxInList :: PhraSyn -> [PhraSyn] -> [PhraSyn]
deleteAllxInList x [] finList = finList
deleteAllxInList x [] finList
deleteAllxInList x list finList = deleteAllxInList x list' finList
    where
    list' = tail list
    finList = case x==(head list) of
          True -> finList
          False -> finList ++ [list!!0]
-}

getMode4OTList :: [OverType] ->  FreqOfOTMap -> OverType
getMode4OTList [] _ = -1                                -- No mode
getMode4OTList [x] _ = x
getMode4OTList oTList freqOfOTMap = mode
    where
    freqOfOTList = Map.toList freqOfOTMap
    freqList = map snd freqOfOTList
    maxFreq = maximum freqList
    idx = elemIndex maxFreq freqList
    idx' = maybe (-1) (+0) idx
    mode = fst (freqOfOTList!!idx')

{- 求OverType的频次
 - (x:xs): OverType列表
 - origFreqOfOT: 原OverType到频次的映射“字典”
 -}
getOverTypeFreqMap :: [OverType] -> Map.Map OverType Freq -> Map.Map OverType Freq
getOverTypeFreqMap [] origFreqOfOT = origFreqOfOT
getOverTypeFreqMap (x:xs) origFreqOfOT = getOverTypeFreqMap xs newFreqOfOT
    where
    newFreqOfOT = Map.insertWith (+) x 1 origFreqOfOT

{-
getxCountInList :: Int -> [Int] -> Int
getxCountInList x [] _ = 0
getxCountInList x [] count = count
getxCountInList x list count = getxCountInList x list1 count1
    where
    count1 = case x==(head list) of
           True -> count + 1
           False -> count
    list1 = tail list
-}

{- 计算总距离(各个簇中样本到其所在簇的众心的距离之和)
   (x:xs): 表示除众心点外的其他样本点
 -}
distTotalByClust :: [SampleClusterMark] -> Double -> Double
distTotalByClust [] origDist = origDist
distTotalByClust (x:xs) origDist = distTotalByClust xs newDist
    where
    newDist = origDist + (snd5 x)

type DistTotal = Double                                   -- Sum of distances between all samples and their cluster centres.
type DistMean = Double
type ClusterMap = Map.Map CIdx [StruGeneSample]          -- Partition of sample set after one time of clustering iteration.

{- 发现所有样本的聚类结果
 -}
findFinalCluster4AllSamplesByArithAdd :: TableName -> ClusterMap -> CentreList -> KVal -> INo -> DistTotal -> DistWeiRatioList -> IO (INo, Dist)
findFinalCluster4AllSamplesByArithAdd tblName clusterMap origCentreList kVal iNo origDistTotal distWeiRatioList = do
    let newCentreList = updateCentre4AllCluster (Map.elems clusterMap) []
    let struGeneSampleList = Map.foldr (++) [] clusterMap                             -- recover original samples.
    let scml = findCluster4AllSamplesByArithAdd struGeneSampleList newCentreList kVal iNo [] distWeiRatioList
    let newClusterMap = divideSameCluster scml Map.empty
    let newDistTotal = distTotalByClust scml 0.0
    let distMean = newDistTotal / fromIntegral (length scml)
    storeOneIterationResult tblName iNo scml newCentreList distMean
    putStrLn $ "findFinalCluster4AllSamplesByArithAdd: iNo = " ++ show iNo ++ ", origDistTotal = " ++ show origDistTotal ++ ", newDistTotal = " ++ show newDistTotal
    if newDistTotal < origDistTotal
--    if iNo < 20
      then findFinalCluster4AllSamplesByArithAdd tblName newClusterMap newCentreList kVal (iNo + 1) newDistTotal distWeiRatioList
--      else return scml
      else do
        let finalDistMean = origDistTotal / fromIntegral (length scml)
        return (iNo, finalDistMean)

{- 发现所有样本的聚类结果
 -}
findFinalCluster4AllSamplesWithoutPri :: TableName -> ClusterMap -> CentreList -> KVal -> INo -> DistTotal -> DistWeiRatioList -> IO (INo, Dist)
findFinalCluster4AllSamplesWithoutPri tblName clusterMap origCentreList kVal iNo origDistTotal distWeiRatioList = do
    let newCentreList = updateCentre4AllCluster (Map.elems clusterMap) []
    let struGeneSampleList = Map.foldr (++) [] clusterMap                             -- recover original samples.
    let scml = findCluster4AllSamplesByArithAdd struGeneSampleList newCentreList kVal iNo [] distWeiRatioList
    let newClusterMap = divideSameCluster scml Map.empty
    let newDistTotal = distTotalByClust scml 0.0
    let distMean = newDistTotal / fromIntegral (length scml)
    storeOneIterationResult tblName iNo scml newCentreList distMean
    putStrLn $ "findFinalCluster4AllSamplesByArithAdd: iNo = " ++ show iNo ++ ", origDistTotal = " ++ show origDistTotal ++ ", newDistTotal = " ++ show newDistTotal
    if newDistTotal < origDistTotal
--    if iNo < 20
      then findFinalCluster4AllSamplesByArithAdd tblName newClusterMap newCentreList kVal (iNo + 1) newDistTotal distWeiRatioList
--      else return scml
      else do
        let finalDistMean = origDistTotal / fromIntegral (length scml)
        return (iNo, finalDistMean)

--对上面的距离和迭代次数画图表示，也可以用手肘法找到合适的迭代次数，到此聚类结束

{- Read a value from input stream [MySQLValue], change it into a StruGeneSample value, append it
 - to existed StruGeneSample list, then read the next until read Nothing.
 - Here [MySQLValue] is [MySQLText, MySQLText, MySQLText, MySQLText, MySQLInt8, MySQLText].
 -}
readStreamByInt324TextInt8Text :: [StruGeneSample] -> S.InputStream [MySQLValue] -> IO [StruGeneSample]
readStreamByInt324TextInt8Text es is = do
    S.read is >>= \case                                         -- Dumb element 'case' is an array with type [MySQLValue]
        Just x -> readStreamByInt324TextInt8Text (es ++ [(fromMySQLInt32 (x!!0),
                                                    readPhraSynListFromStr (fromMySQLText (x!!1)),
                                                    readPhraSynFromStr (fromMySQLText (x!!2)),
                                                    readPhraSynFromStr (fromMySQLText (x!!3)),
                                                    readPhraSynListFromStr (fromMySQLText (x!!4)),
                                                    fromMySQLInt8 (x!!5),
                                                    readPriorFromStr (fromMySQLText (x!!6)))]) is
        Nothing -> return es

type TableName = String
{- Store clustering result of every times of iteration.
 - SampleClusterMark :: (StruGeneSample,DMin,CIdx,StruGene,INo)
 - StruGene :: (LeftExtend, LeftOver, RightOver, RightExtend, OverType, Prior)
 - Actually, the modes in clustering have type 'StruGene'.
 -}
storeOneIterationResult :: TableName -> INo -> [SampleClusterMark] -> [StruGene] -> DistMean -> IO ()
storeOneIterationResult tblName iNo scml kCentres distMean = do
    let cIdxPriorsTuple = map (\x -> (thd5 x, [(svt7 . fst5) x])) scml
    let priorsOfSameCIdx = Map.toList $ getPriorsOfSameClusterMap cIdxPriorsTuple Map.empty
--    putStrLn $ "Priors which are from same cluster will be expressed in the following form: " ++ show priorsOfSameCIdx
    let scd = map (\x -> ((fst7 . fst5) x, thd5 x, snd5 x)) scml                 -- [(SIdx, CIdx, DMin)]
    let scdStr = nScdToString scd                                        -- Get the string of [(SIdx, CIdx, DMin)]
    let modesStr = nStruGeneToString kCentres                            -- Get the string of [StruGene]

    conn <- getConn
    let sqlstat = DS.fromString $ "insert into " ++ tblName ++ " set iNo = ?, sampleClustInfo = ?, modes = ?, distMean = ?"
--  let sqlstat = DS.fromString $ "insert into " ++ tblName ++ " values (iNo, scdStr, modesStr, distMean)"
    stmt <- prepareStmt conn sqlstat
    executeStmt conn stmt [toMySQLInt8 iNo, toMySQLText scdStr, toMySQLText modesStr, toMySQLDouble distMean]
    close conn

type CIdxPriorsTuple = (SIdx, [Prior])

{- 求显示同一聚类里的prior
 - (x:xs): PhraSyn列表
 - origFreqOfPhraSyn: 原PhraSyn的频次“字典”
 -}
getPriorsOfSameClusterMap :: [CIdxPriorsTuple] -> Map.Map CIdx [Prior] -> Map.Map CIdx [Prior]
getPriorsOfSameClusterMap [] origPriorOfSameCluster = origPriorOfSameCluster
getPriorsOfSameClusterMap (x:xs) origPriorOfSameCluster = getPriorsOfSameClusterMap xs newPriorOfSameCluster
    where
    newPriorOfSameCluster = Map.insertWith (++) (fst x) (snd x) origPriorOfSameCluster

type TimeTableName = String
type SNum = Int
storeClusterTime :: TimeTableName -> KVal -> SNum -> NominalDiffTime -> NominalDiffTime -> Int -> Double -> IO ()
storeClusterTime timeTblName kVal sNum totalRunTime iterMeanTime its finalDistMean = do
    let tRTStr = init (show totalRunTime)
    let iMTStr = init (show iterMeanTime)
    let tRTDouble = read tRTStr :: Double
    let iMTDouble = read iMTStr :: Double
    let its' = its + 1
    putStrLn $ "storeClusterTime: tRTDouble = " ++ show tRTDouble ++ ", iMTDouble = " ++ show iMTDouble ++ ", iNo = " ++ show its' ++ ", kVal = " ++ show kVal ++ ", sNum = " ++ show sNum ++ ", finalDistMean = " ++ show finalDistMean

    conn <- getConn
    let sqlstat = DS.fromString $ "select kVal, sNum from " ++ timeTblName ++ " where kVal = ? and sNum = ?"
    stmt <- prepareStmt conn sqlstat
    (defs, is) <- queryStmt conn stmt [toMySQLInt32 kVal, toMySQLInt32 sNum]
    row <- S.read is                                   -- Maybe [MySQLInt32, MySQLInt32]
    let row' = case row of
                 Just x -> [fromMySQLInt32 (x!!0), fromMySQLInt32 (x!!1)]     -- [MySQLInt32, MySQLInt32]
                 Nothing -> [0, 0]
    S.skipToEof is                                     -- Skip to end-of-stream.
    if row' /= [0, 0]
      then do
        let sqlstat = DS.fromString $ "update " ++ timeTblName ++ " set totalTime = ?, iterMeanTime = ?, iNo = ?, finalDistMean = ? where kVal = ? and sNum = ?"
        stmt <- prepareStmt conn sqlstat
        ok <- executeStmt conn stmt [toMySQLDouble tRTDouble, toMySQLDouble iMTDouble, toMySQLInt8 its', toMySQLDouble finalDistMean, toMySQLInt32 kVal, toMySQLInt32 sNum]
        putStrLn $ "storeClusterTime: Number of rows update affected = " ++ show (getOkAffectedRows ok)
        close conn
      else do
        let sqlstat = DS.fromString $ "insert into " ++ timeTblName ++ " set kVal = ?, sNum = ?, totalTime = ?, iterMeanTime = ?, iNo = ?, finalDistMean = ?"
        stmt <- prepareStmt conn sqlstat
        ok <- executeStmt conn stmt [toMySQLInt32 kVal, toMySQLInt32 sNum, toMySQLDouble tRTDouble, toMySQLDouble iMTDouble, toMySQLInt8 its', toMySQLDouble finalDistMean]
        putStrLn $ "storeClusterTime: Number of rows insert affected = " ++ show (getOkAffectedRows ok)
        close conn

type BottomKVal = Int
type DeltaKVal = Int
type TopKVal = Int
type KValRange = (BottomKVal, DeltaKVal, TopKVal)
type BottomSNum = Int
type DeltaSNum = Int
type TopSNum = Int
type SNumRange = (BottomSNum, DeltaSNum, TopSNum)

autoRunClustByChangeKValSNum :: String -> String -> Int -> KValRange -> SNumRange -> DistWeiRatioList -> IO ()
autoRunClustByChangeKValSNum arm df kVal kValRange sNumRange distWeiRatioList = do
    let sNum = fst3 sNumRange
    putStrLn $ "The clustering of kVal = " ++ show kVal ++ ", sNum = " ++ show sNum ++ " begins."
    conn <- getConn
    confInfo <- readFile "Configuration"                                        -- Read the local configuration file
    let syntax_ambig_resol_model = getConfProperty "syntax_ambig_resol_model" confInfo

    if | arm == "Nothing" -> putStrLn "autoRunClustByChangeKValSNum: 'syntax_ambig_resol_model' is not correct."
       | df == "Nothing" -> putStrLn "autoRunClustByChangeKValSNum: 'distDef' is not correct."
       | otherwise -> do
--           t1 <- getCurrentTime
{-初始聚类随机选点
           let sqlstat = DS.fromString $ "select id, leftExtend, leftOver, rightOver, rightExtend, overType, prior from " ++ syntax_ambig_resol_model ++ " where id <= ? "
           stmt <- prepareStmt conn sqlstat
           (defs, is) <- queryStmt conn stmt [toMySQLInt32 sNum]
           struGeneSampleList <- readStreamByInt324TextInt8Text [] is
           let sIdxStruGeneMap = Map.fromList $ map (\x -> (fst7 x, (snd7 x, thd7 x, fth7 x, fif7 x, sth7 x, svt7 x))) struGeneSampleList
           randomSIdsList <- getRandomsList kVal 1 sNum []
           let randomInitalModes = getStruGenebySIdsList randomSIdsList sIdxStruGeneMap []
-}
           let clustResTbl = "clust_res_k" ++ show kVal ++ "_" ++ "s" ++ show sNum ++ "_" ++ arm ++ "_" ++ df ++ "_w" ++ (foldl (++) "" (map show (init distWeiRatioList)))
        --   let timeTbl = "clust_time_" ++ arm ++ "_" ++ df ++ "_w" ++ (foldl (++) "" (map show (init distWeiRatioList)))
           let sqlstat = DS.fromString $ "drop table if exists " ++ clustResTbl
           stmt <- prepareStmt conn sqlstat
           executeStmt conn stmt []

           let sqlstat = DS.fromString $ "create table " ++ clustResTbl ++ " (iNo tinyint primary key, sampleClustInfo mediumtext, modes mediumtext, distMean float)"
           stmt <- prepareStmt conn sqlstat
           executeStmt conn stmt []                          -- Create a new MySQL table for storing clustering result.

           let sqlstat = DS.fromString $ "select id, leftExtend, leftOver, rightOver, rightExtend, overType, prior from " ++ syntax_ambig_resol_model ++ " where id >= ? and id <= ? "
           stmt <- prepareStmt conn sqlstat
           (defs, is) <- queryStmt conn stmt [toMySQLInt32 1, toMySQLInt32 sNum]

           struGeneSampleList <- readStreamByInt324TextInt8Text [] is
           let m1 = head struGeneSampleList
           let sps = tail struGeneSampleList
           let initialKPoints = getKModeByMaxMinPoint sps [m1] Map.empty kVal distWeiRatioList
           let scml = findCluster4AllSamplesByArithAdd struGeneSampleList initialKPoints kVal 0 [] distWeiRatioList
--           let scml = findCluster4AllSamplesByArithAdd struGeneSampleList randomInitalModes kVal 0 [] distWeiRatioList
           let clusterMap = divideSameCluster scml Map.empty
           let origDistTotal = distTotalByClust scml 0.0
           let distMean = origDistTotal / fromIntegral sNum
           storeOneIterationResult clustResTbl 0 scml initialKPoints distMean       -- Store the first clustering result.
--           ti1 <- getCurrentTime                                                -- Start time of iteration
           finalINoDistMean <- findFinalCluster4AllSamplesByArithAdd clustResTbl clusterMap initialKPoints kVal 1 origDistTotal distWeiRatioList
--           ti2 <- getCurrentTime                                                -- End time of iteration
--         forM_ scml' $ \scm -> putStrLn $ "autoRunClustByChangeKValSNum: " ++ show scm
           closeStmt conn stmt

           putStrLn $ "autoRunClustByChangeKValSNum: " ++ " final iNo and distMean are " ++  show finalINoDistMean
{-            t2 <- getCurrentTime
--           let totalRunTime = diffUTCTime t2 t1
--           let its = fif5 (scml'!!0)                                            -- times of iteration.
           let its = fst finalINoDistMean                                       -- times of iteration.
           let finalDistMean = snd finalINoDistMean
          let iterMeanTime = (diffUTCTime ti2 ti1) / fromIntegral its
           putStrLn $ "autoRunClustByChangeKValSNum: totalRunTime = " ++ show totalRunTime ++ ", iterMeanTime = " ++ show iterMeanTime ++ ", times of iterations = " ++ show (its + 1)

           let sqlstat = DS.fromString $ "create table if not exists " ++ timeTbl ++ " (kVal int, sNum int, totalTime float, iterMeanTime float, iNo tinyint, finalDistMean float, primary key (kVal, sNum))"
           stmt <- prepareStmt conn sqlstat
           ok <- executeStmt conn stmt []                          -- Create a new MySQL table for storing clustering time.
           putStrLn $ "autoRunClustByChangeKValSNum: okStatus = " ++ show (getOkStatus ok)
           close conn
           storeClusterTime timeTbl kVal sNum totalRunTime iterMeanTime its finalDistMean
           putStrLn $ "The clustering of kVal = " ++ show kVal ++ ", sNum = " ++ show sNum ++ " has finished."

           let struGeneSampleOfLpList = getStruGeneSampleOfLp struGeneSampleList []
           clusterToStruGeneSampleListFromDB struGeneSampleOfLpList clustResTbl arm df kVal kValRange sNumRange distWeiRatioList
           let struGeneSampleOfRpList = [x | x <- struGeneSampleList, notElem x struGeneSampleOfLpList]
           clusterToStruGeneSampleListFromDB struGeneSampleOfRpList clustResTbl arm df kVal kValRange sNumRange distWeiRatioList
-}
           let kVal' = kVal + snd3 kValRange
           let (kVal'', sNum') = case kVal' > thd3 kValRange of
                                   True -> (fst3 kValRange, sNum + snd3 sNumRange)
                                   False -> (kVal', sNum)
           if (sNum' > thd3 sNumRange || snd3 kValRange == 0)
             then putStrLn "autoRunClustByChangeKValSNum: End."
             else autoRunClustByChangeKValSNum arm df kVal'' kValRange (sNum', snd3 sNumRange, thd3 sNumRange) distWeiRatioList

getStruGeneSampleOfLp :: [StruGeneSample] -> [StruGeneSample] -> [StruGeneSample]
getStruGeneSampleOfLp [] struGeneSampleOfLpList = struGeneSampleOfLpList
getStruGeneSampleOfLp (x:xs) struGeneSampleOfLpList = getStruGeneSampleOfLp xs struGeneSampleOfLpList'
    where
    struGeneSampleOfLpList' = case svt7 x == Lp of
                               True -> struGeneSampleOfLpList ++ [x]
                               False -> struGeneSampleOfLpList

clusterToStruGeneSampleListFromDB :: [StruGeneSample] -> String -> String -> String -> Int -> KValRange -> SNumRange -> DistWeiRatioList -> IO ()
clusterToStruGeneSampleListFromDB struGeneSampleList clustResTbl arm df kVal kValRange sNumRange distWeiRatioList = do
    let sNum = fst3 sNumRange
    let m1 = head struGeneSampleList
    let sps = tail struGeneSampleList
    let actualKVal = div kVal 2
    let initialKPoints = getKModeByMaxMinPoint sps [m1] Map.empty actualKVal distWeiRatioList
    let scml = findCluster4AllSamplesByArithAdd struGeneSampleList initialKPoints actualKVal 0 [] distWeiRatioList

    let clusterMap = divideSameCluster scml Map.empty
    let origDistTotal = distTotalByClust scml 0.0
    let distMean = origDistTotal / fromIntegral sNum
    storeOneIterationResult clustResTbl 0 scml initialKPoints distMean       -- Store the first clustering result.
    finalINoDistMean <- findFinalCluster4AllSamplesByArithAdd clustResTbl clusterMap initialKPoints actualKVal 1 origDistTotal distWeiRatioList

    putStrLn $ "autoRunClustByChangeKValSNum: " ++ " final iNo and distMean are " ++  show finalINoDistMean
    putStrLn $ "The clustering of kVal = " ++ show kVal ++ ", sNum = " ++ show sNum ++ " has finished."



autoRunGetAmbiResolAccuracyOfAllClustRes :: String -> String -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> IO ()
autoRunGetAmbiResolAccuracyOfAllClustRes arm df kVal bottomKVal deltaKVal topKVal bottomSNum deltaSNum topSNum = do
    let sNum = bottomSNum
--    putStrLn $ "The ambiguity resolution accuracy of kVal = " ++ show kVal ++ ", sNum = " ++ show sNum ++ " is counting."
    conn <- getConn
    confInfo <- readFile "Configuration"                                        -- Read the local configuration file
    let syntax_ambig_resol_model = getConfProperty "syntax_ambig_resol_model" confInfo

    let ambiResolAccuracyTbl = "ambi_Resol_Accuracy_for_randomModes"
    let sqlstat = DS.fromString $ "create table if not exists " ++ ambiResolAccuracyTbl ++ " (kVal int, sNum int, samplesCount int, accuracyForRandomModes1 float, accuracyForRandomModes2 float, primary key (kVal, sNum, samplesCount))"
    stmt <- prepareStmt conn sqlstat
    executeStmt conn stmt []                          -- Create a new MySQL table for storing all ambiguity resolution accuracy.

    if | arm == "Nothing" -> putStrLn "autoRunGetAmbiResolAccuracyOfAllClustRes: 'syntax_ambig_resol_model' is not correct."
       | df == "Nothing" -> putStrLn "autoRunGetAmbiResolAccuracyOfAllClustRes: 'distDef' is not correct."
       | otherwise -> do

           let sqlstat = DS.fromString $ "select id, leftExtend, leftOver, rightOver, rightExtend, overType, prior from " ++ syntax_ambig_resol_model ++ " where id <= ? "
           stmt <- prepareStmt conn sqlstat
           (defs, is) <- queryStmt conn stmt [toMySQLInt32 sNum]
           struGeneSampleList <- readStreamByInt324TextInt8Text [] is
           let sIdxStruGeneMap = Map.fromList $ map (\x -> (fst7 x, (snd7 x, thd7 x, fth7 x, fif7 x, sth7 x, svt7 x))) struGeneSampleList

           randomSIdsList1 <- getRandomsList kVal 1 sNum []
--           putStrLn $ "randoms = " ++ show randomSIdsList1
           let randomStruGene1 = getStruGenebySIdsList randomSIdsList1 sIdxStruGeneMap []
--           putStrLn $ "randomStruGene = " ++ show randomStruGene1
           randomSIdsList2 <- getRandomsList kVal 1 sNum []
           let randomStruGene2 = getStruGenebySIdsList randomSIdsList2 sIdxStruGeneMap []
{-
           let clustResTbl = "clust_res_k" ++ show kVal ++ "_" ++ "s" ++ show sNum ++ "_" ++ arm ++ "_" ++ df
           let sqlstat = DS.fromString $ "select modes from " ++ clustResTbl
           stmt <- prepareStmt conn sqlstat
           (defs, is) <- queryStmt conn stmt []
           modesListOfAllIterations <- readStreamByText [] is

           let finalModesListStr = last (init modesListOfAllIterations)
           let finalModesList = readStruGeneListFromStr finalModesListStr
           let initalModesListStr = head modesListOfAllIterations
           let initalModesList = readStruGeneListFromStr initalModesListStr
-}
           let randomModesList1 = randomStruGene1
           let randomModesList2 = randomStruGene2
{-
           let sams1 = findAmbiResolResOfAllSamples struGeneSampleList finalModesList []
           let hitResList1 = map (\x -> snd3 x == thd3 x) sams1
           let accuracyForFinalModes = fromIntegral (length [x | x <- hitResList1, x == True]) / fromIntegral sNum

           let sams2 = findAmbiResolResOfAllSamples struGeneSampleList initalModesList []
           let hitResList2 = map (\x -> snd3 x == thd3 x) sams2
           let accuracyForInitalModes = fromIntegral (length [x | x <- hitResList2, x == True]) / fromIntegral sNum
-}
           let sams3 = findAmbiResolResOfAllSamples struGeneSampleList randomModesList1 []
           let hitResList3 = map (\x -> snd3 x == thd3 x) sams3
           let accuracyForRandomModes1 = fromIntegral (length [x | x <- hitResList3, x == True]) / fromIntegral sNum
           let sams4 = findAmbiResolResOfAllSamples struGeneSampleList randomModesList2 []
           let hitResList4 = map (\x -> snd3 x == thd3 x) sams4
           let accuracyForRandomModes2 = fromIntegral (length [x | x <- hitResList4, x == True]) / fromIntegral sNum

--           putStrLn $ "The ambiguity resolution accuracy of (kVal = " ++ show kVal ++ ", sNum = " ++ show sNum ++ "), first " ++ show sNum ++ " samples is accuracyForFinalModes = "++ show accuracyForFinalModes ++ ", accuracyForInitalModes = " ++ show accuracyForInitalModes++ ", accuracyForRandomModes = " ++ show accuracyForRandomModes
           putStrLn $ "The ambiguity resolution accuracy of (kVal = " ++ show kVal ++ ", sNum = " ++ show sNum ++ "), first " ++ show sNum ++ " samples is accuracyForRandomModes1 = " ++ show accuracyForRandomModes1 ++ ", accuracyForRandomModes2 = " ++ show accuracyForRandomModes2
           let sqlstat = DS.fromString $ "replace into " ++ ambiResolAccuracyTbl ++ " set kVal = ?, sNum = ?, samplesCount = ?, accuracyForRandomModes1 = ?, accuracyForRandomModes2 = ?"
           stmt <- prepareStmt conn sqlstat
           executeStmt conn stmt [toMySQLInt32 kVal, toMySQLInt32 sNum, toMySQLInt32 sNum, toMySQLDouble accuracyForRandomModes1, toMySQLDouble accuracyForRandomModes2]

           let kVal' = kVal + deltaKVal
           let (kVal'', sNum') = case kVal' > topKVal of
                                   True -> (bottomKVal, sNum + deltaSNum)
                                   False -> (kVal', sNum)
           if sNum' > topSNum
             then putStrLn "autoRunGetAmbiResolAccuracyOfAllClustRes: End."
             else autoRunGetAmbiResolAccuracyOfAllClustRes arm df kVal'' bottomKVal deltaKVal topKVal sNum' deltaSNum topSNum

getStruGenebySIdsList :: [Int] -> Map.Map SIdx StruGene -> [StruGene] -> [StruGene]
getStruGenebySIdsList [] _ struGeneList = struGeneList
getStruGenebySIdsList (x:xs) sIdxStruGeneMap struGeneList = getStruGenebySIdsList xs sIdxStruGeneMap struGeneList'
    where
    struGene = Map.lookup x sIdxStruGeneMap
    struGene' = case struGene of
                  Just x -> x
                  Nothing -> nullStruGene
    struGeneList' = struGeneList ++ [struGene']

{- 该函数由于每次迭代都需要连接数据库，运行时会报Exception: DecodePacketFailed "\DLE\EOTToo many connections" 1 "not enough bytes"
queryStruGenebySIdsList :: [Int] -> [StruGene] -> IO [StruGene]
queryStruGenebySIdsList idsList struGeneList = do
    length idsList
    conn <- getConn
    confInfo <- readFile "Configuration"
    let syntax_ambig_resol_model = getConfProperty "syntax_ambig_resol_model" confInfo
    let sqlstat = DS.fromString $ "select id, leftExtend, leftOver, rightOver, rightExtend, overType, prior from " ++ syntax_ambig_resol_model ++ " where id = ? "
    stmt <- prepareStmt conn sqlstat
    (defs, is) <- queryStmt conn stmt [toMySQLInt32 x]

    struGeneSampleList <- readStreamByInt324TextInt8Text [] is
    let m = head struGeneSampleList       -- Only a value in struGeneSampleList
    let struGeneList' = struGeneList ++ [(snd7 m, thd7 m, fth7 m, fif7 m, sth7 m, svt7 m)]

    if length xs == 0
      then return struGeneList'
      else queryStruGenebySIdsList xs struGeneList'
-}

getRandomsList :: Int -> Int -> Int -> [Int] -> IO [Int]
getRandomsList count rangeStart rangeEnd randomList = do
    random <- randomRIO (rangeStart, rangeEnd)
    let randomList' = randomList ++ [random]
    if length randomList' == count
      then return randomList'
      else getRandomsList count rangeStart rangeEnd randomList'

{-
getRandomsList :: Int -> Int -> Int -> IO [Int]
getRandomsList count rangeStart rangeEnd = do
    let rollDice = applyAtomicGen (uniformR (rangeStart, rangeEnd)) globalStdGen
    let randomList = replicateM count (rollDice :: IO Int)
    return randomList
-}

{- 对于一个(kVal,sNum)上的聚类结果，计算其消歧的正确率.
 - 歧义消解库stru_gene的前sNum条歧义消解片段和每一个(kVal,sNum)上聚类得到的众心作为原始数据.
 - 对每条歧义消解片段找到其最近的众心，若歧义消解片段的prior和众心的prior一致，算命中，命中次数加1.
 - 最终求出的准确率accuracy为：命中次数除以总次数
-}
getAmbiResolAccuracyOfAClustRes :: IO Double
getAmbiResolAccuracyOfAClustRes = do
    conn <- getConn
    confInfo <- readFile "Configuration"                                        -- Read the local configuration file
    let clustResTbl = getConfProperty "clustResTbl" confInfo
    putStrLn $ "The current clustResTbl is " ++ show clustResTbl

    putStr "Please input the start value of 'id' in stru_gene as test datas: "
    line <- getLine
    let startId = read line :: Int
    putStr "Please input the end value of 'id' in stru_gene as test datas: "
    line <- getLine
    let endId = read line :: Int

    let sNum = endId - startId + 1

    let sqlstat = DS.fromString $ "select modes from " ++ clustResTbl
    stmt <- prepareStmt conn sqlstat
    (defs, is) <- queryStmt conn stmt []
    modesListOfAllIterations <- readStreamByText [] is
    let finalModesListStr = head modesListOfAllIterations
--    let finalModesListStr = last (init modesListOfAllIterations)
    let finalModesList = readStruGeneListFromStr finalModesListStr

    let syntax_ambig_resol_model = getConfProperty "syntax_ambig_resol_model" confInfo
    let sqlstat = DS.fromString $ "select id, leftExtend, leftOver, rightOver, rightExtend, overType, prior from " ++ syntax_ambig_resol_model ++ " where id >= ? and id <= ? "
    stmt <- prepareStmt conn sqlstat
    (defs, is) <- queryStmt conn stmt [toMySQLInt32 startId, toMySQLInt32 endId]

    struGeneSampleList <- readStreamByInt324TextInt8Text [] is
    let sams = findAmbiResolResOfAllSamples struGeneSampleList finalModesList []
--    putStrLn $ "The ambiguity resolution result is for first samples range in " ++ show startId ++ "~" ++ show endId ++ " is " ++ show sams ++ " ."
    let hitResList = map (\x -> snd3 x == thd3 x) sams
    let accuracy = fromIntegral (length [x | x <- hitResList, x == True]) / fromIntegral sNum
    putStrLn $ "The ambiguity resolution accuracy is for samples range in " ++ show startId ++ "~" ++ show endId ++ " is " ++ show accuracy ++ "."
    return accuracy

type Modes = [StruGene]
type ActualPrior = Prior
type AmbiResolPrior = Prior
type SampleAmbiMark = (SIdx, ActualPrior, AmbiResolPrior)

{- find ambiguity resolution result of all stru_gene samples.
-}
findAmbiResolResOfAllSamples :: [StruGeneSample] -> Modes -> [SampleAmbiMark] -> [SampleAmbiMark]
findAmbiResolResOfAllSamples [] _ sampleAmbiMark = sampleAmbiMark
findAmbiResolResOfAllSamples (x:xs) modes sampleAmbiMark = findAmbiResolResOfAllSamples xs modes sampleAmbiMark'
    where
    sampleAmbiMark' = sampleAmbiMark ++ [findAmbiResolResOfASample x modes]

{- find ambiguity resolution result of a stru_gene sample
-}
findAmbiResolResOfASample :: StruGeneSample -> Modes -> SampleAmbiMark
findAmbiResolResOfASample sg modes = (sIdx, actualPrior, ambiResolPrior)
    where
    sIdx = fst7 sg
    actualPrior = svt7 sg
    distList = findDistFromASampleToModes sg modes []
    minDist = minimum distList
    index = elemIndex minDist distList
    index' = case index of
               Just x -> x
               Nothing -> error "findAmbiResolResOfASample: Impossible situation."
    finalMode = modes!!index'
    ambiResolPrior = sth6 finalMode

{- find all distances from a sample to modes.
-}
findDistFromASampleToModes :: StruGeneSample -> Modes -> [Dist] -> [Dist]
findDistFromASampleToModes _ [] distList = distList
findDistFromASampleToModes sg (x:xs) distList = findDistFromASampleToModes sg xs distList'
    where
    sg' = (snd7 sg, thd7 sg, fth7 sg, fif7 sg, sth7 sg, svt7 sg)
    dist = dist4StruGeneWithoutPriByArithAdd sg' x
    distList' = distList ++ [dist]

{- Arithmetically added distance between two StruGenes besides prior.
 -}
dist4StruGeneWithoutPriByArithAdd :: StruGene -> StruGene -> Double
dist4StruGeneWithoutPriByArithAdd s1 s2 = foldl (+) 0.0 $ distVect4StruGeneByIdentity s1 s2

distVect4StruGeneWithoutPri :: StruGene -> StruGene -> [Double]
distVect4StruGeneWithoutPri s1 s2 = [d1, d2, d3, d4, d5]
    where
    d1 = distPhraSynSetByIdentity (fst6 s1) (fst6 s2)
    d2 = distPhraSynByIdentity (snd6 s1) (snd6 s2)
    d3 = distPhraSynByIdentity (thd6 s1) (thd6 s2)
    d4 = distPhraSynSetByIdentity (fth6 s1) (fth6 s2)
    d5 = case fif6 s1 == fif6 s2 of
           True -> 0.0
           False -> 1.0

{- Read original StruGene samples or their clustering result,
 - the source of which is indicated by attribute 'syntax_ambig_resol_model' in configuration.
 - This function is obsoleted.
 -}
getStruGeneSamples :: IO [StruGeneSample]
getStruGeneSamples = do
    confInfo <- readFile "Configuration"
    let syntax_ambig_resol_model = getConfProperty "syntax_ambig_resol_model" confInfo
    conn <- getConn

    putStrLn $ "Get samples from " ++ syntax_ambig_resol_model

    if syntax_ambig_resol_model == "stru_gene"
      then do
        let sqlstat = DS.fromString $ "select id, leftExtend, leftOver, rightOver, rightExtend, overType, prior from " ++ syntax_ambig_resol_model
        stmt <- prepareStmt conn sqlstat
        (defs, is) <- queryStmt conn stmt []
        struGeneSampleList <- readStreamByInt324TextInt8Text [] is              -- [StruGeneSample]
        return struGeneSampleList
      else
        if (length (splitAtDeli '_' syntax_ambig_resol_model) > 4 && (splitAtDeli '_' syntax_ambig_resol_model)!!4 == "sg")
          then do
            let sqlstat = DS.fromString $ "select modes from " ++ syntax_ambig_resol_model ++ " where iNo = (select max(iNo) from " ++ syntax_ambig_resol_model ++ ") - 1"
                                    -- MySQL table 'syntax_ambig_resol_model' have at least two records.
            stmt <- prepareStmt conn sqlstat
            (defs, is) <- queryStmt conn stmt []
            modeListStrList <- readStreamByText [] is
            let modeListStr = head modeListStrList               -- head of [String], which has only one element.
            let modeList = readStruGeneListFromStr modeListStr
            let len = length modeList
            let sgs = zip [1..len] modeList
            return (map (\x -> (fst x, fst6 (snd x), snd6 (snd x), thd6 (snd x), fth6 (snd x), fif6 (snd x), sth6 (snd x))) sgs)
          else do
            putStrLn "getStruGeneSamples: Value of property 'syntax_ambig_resol_model' does not match any MySQL table."
            return []

{- Read stru_gene_202408 samples, the source of which is indicated by attribute 'syntax_ambig_resol_model' in configuration.
 - Sample type StruGene2Sample :: (SIdx, LeftExtend, LeftOver, RightOver, RightExtend, OverType, [ClauTagPrior])
 -}
getStruGene2Samples :: IO [StruGene2Sample]
getStruGene2Samples = do
    confInfo <- readFile "Configuration"
    let syntax_ambig_resol_model = getConfProperty "syntax_ambig_resol_model" confInfo
    let startSn = read (getConfProperty "syntax_resol_sample_startsn" confInfo) :: Int
    let endSn = read (getConfProperty "syntax_resol_sample_endsn" confInfo) :: Int

    conn <- getConn
    putStrLn $ "Get samples from " ++ syntax_ambig_resol_model
    case syntax_ambig_resol_model of
      x | elem x ["stru_gene_202408", "stru_gene_202412", "stru_gene_202501"] -> do
        let sqlstat = DS.fromString $ "select id, leftExtend, leftOver, rightOver, rightExtend, overType, clauTagPrior from " ++ syntax_ambig_resol_model
        stmt <- prepareStmt conn sqlstat
        (defs, is) <- queryStmt conn stmt []
        struGene2SampleList <- readStreamByStruGene2Sample [] is      -- [(SIdx,LeftExtend,LeftOver,RightOVer,RightExtend,OverType,[ClauTagPrior])]
        let struGene2SampleList' = map (\x -> (fst7 x, snd7 x, thd7 x, fth7 x, fif7 x, sth7 x, removeFromCTPListBySentIdxRange (svt7 x) startSn endSn)) struGene2SampleList
        let struGene2SampleList'' = filter (\x -> svt7 x /= []) struGene2SampleList'                 -- Might be unnecessary.
        let sentSnRange = getSentRangeByStruGeneSamples struGene2SampleList'' (maxBound :: Int, minBound :: Int)
        putStrLn $ "  startSn = " ++ show startSn ++ ", endSn = " ++ show endSn
        return struGene2SampleList''
      _ -> do
        putStrLn "getStruGene2Samples: Value of property 'syntax_ambig_resol_model' does not match any MySQL table."
        return []

-- Implementation of menu 'D. Clustering analysis'.
clusteringAnalysis :: Int -> IO ()
clusteringAnalysis funcIndex = do
    -- 6. Test function getPhraSynSetSim.
    if funcIndex == 6
       then do
         putStrLn "(1) Collect phrasal similarity"
         startSn <- getNumUntil "Please input serial_num of start sentence [RETURN for 1]: " [1 ..]
         endSn <- getNumUntil "Please input serial_num of end sentence [RETURN for last]: " [0 ..]
         if startSn > endSn
           then putStrLn "No sentence is designated."
           else do
     --        sentClauPhraList <- getSentClauPhraList startSn endSn
     --        putStrLn $ "Phrasal categries of first sentence are: " ++ show (sentClauPhraList!!0)
             phraSynPair2SimMap <- getPhraSynPair2SimFromTreebank startSn endSn
             putStrLn $ "Map (PhraSyn, PhraSyn) SimDeg: " ++ show phraSynPair2SimMap

             putStrLn "(2) Calculate similarity between PhraSyn value sets."
             let ps1 = (npCate, "A/n->", "AHn", 2)
             let ps2 = (npCate, ">", "AHn", 5)
             let ps3 = (predCate, ">", "VO", 3)
             let ps4 = (npCate, "Cn/a-<", "HnC", 3)

             putStrLn "ps1 = (np, \"A/n->\", \"AHn\", 2), ps2 = (np, \">\", \"AHn\", 5), ps3 = (s\\.np, \">\", \"VO\", 3), ps4 = (np, \"Cn/a-<\", \"HnC\", 3)"
             putStrLn $ "The result of getPhraSynSetSim [] [] phraSynPair2SimMap is: " ++ show (getPhraSynSetSim [] [] phraSynPair2SimMap)
             putStrLn $ "The result of getPhraSynSetSim [] [ps1] phraSynPair2SimMap is: " ++ show (getPhraSynSetSim [] [ps1] phraSynPair2SimMap)
             putStrLn $ "The result of getPhraSynSetSim [ps1] [] phraSynPair2SimMap is: " ++ show (getPhraSynSetSim [ps1] [] phraSynPair2SimMap)
             putStrLn $ "The result of getPhraSynSetSim [ps1] [ps1] phraSynPair2SimMap is: " ++ show (getPhraSynSetSim [ps1] [ps1] phraSynPair2SimMap)
             putStrLn $ "The result of getPhraSynSetSim [ps1] [ps1, ps2] phraSynPair2SimMap is: " ++ show (getPhraSynSetSim [ps1] [ps1, ps2] phraSynPair2SimMap)
             putStrLn $ "The result of getPhraSynSetSim [ps1, ps2] [ps2] phraSynPair2SimMap is: " ++ show (getPhraSynSetSim [ps1, ps2] [ps2] phraSynPair2SimMap)
             putStrLn $ "The result of getPhraSynSetSim [ps1, ps2, ps3] [ps2, ps3, ps4] phraSynPair2SimMap is: " ++ show (getPhraSynSetSim [ps1, ps2, ps3] [ps2, ps3, ps4] phraSynPair2SimMap)
             putStrLn $ "The result of getPhraSynSetSim [ps1] [ps2] phraSynPair2SimMap is: " ++ show (getPhraSynSetSim [ps1] [ps2] phraSynPair2SimMap)
             putStrLn $ "The result of getPhraSynSetSim [ps2] [ps1] phraSynPair2SimMap is: " ++ show (getPhraSynSetSim [ps2] [ps1] phraSynPair2SimMap)
             putStrLn $ "The result of toPhraSynPair2Sim phraSynPair2SimMap (ps1, ps2) is: " ++ show (toPhraSynPair2Sim phraSynPair2SimMap (ps1, ps2))
             putStrLn $ "The result of toPhraSynPair2Sim phraSynPair2SimMap (ps2, ps1) is: " ++ show (toPhraSynPair2Sim phraSynPair2SimMap (ps2, ps1))
             putStrLn $ "The result of getPhraSynSetSim [ps1, ps3] [ps2] phraSynPair2SimMap is: " ++ show (getPhraSynSetSim [ps1, ps3] [ps2] phraSynPair2SimMap)
             putStrLn $ "The result of getPhraSynSetSim [ps2, ps3] [ps1] phraSynPair2SimMap is: " ++ show (getPhraSynSetSim [ps2, ps3] [ps1] phraSynPair2SimMap)
             putStrLn $ "The result of getPhraSynSetSim [ps1] [ps2, ps3] phraSynPair2SimMap is: " ++ show (getPhraSynSetSim [ps1] [ps2, ps3] phraSynPair2SimMap)
             putStrLn $ "The result of getPhraSynSetSim [ps2] [ps1, ps3] phraSynPair2SimMap is: " ++ show (getPhraSynSetSim [ps2] [ps1, ps3] phraSynPair2SimMap)
       else putStr ""

    -- 7. Get similarity degrees between any two contexts of overlapping types by Singular Value Decomposition.
    if funcIndex == 7
       then do
         confInfo <- readFile "Configuration"
         let syntax_ambig_resol_model = getConfProperty "syntax_ambig_resol_model" confInfo
         let phra_gram_dist_algo = getConfProperty "phra_gram_dist_algo" confInfo
         let overlap_type_dist_algo = getConfProperty "overlap_type_dist_algo" confInfo
         putStrLn $ " syntax_ambig_resol_model: " ++ syntax_ambig_resol_model
         putStrLn $ " phra_gram_dist_algo: " ++ phra_gram_dist_algo
         putStrLn $ " overlap_type_dist_algo: " ++ overlap_type_dist_algo

         contOrNot <- getLineUntil ("Continue or not [c/n]? (RETURN for 'n') ") ["c","n"] False
         if contOrNot == "c"
           then do
             -- (1) From stru_gene_202408 or stru_gene_202412, collect all contexts of overlapping types.
             startIdx <- getNumUntil "Please input index number of start overtype sample [Return for 1]: " [1 ..]
             endIdx <- getNumUntil "Please input index number of end overtype sample [Return for last]: " [0 ..]
             context2OverTypeBase <- getContext2OverTypeBase startIdx endIdx        -- [(ContextOfOT, OverType)]

             -- (2) Calculate similarity degrees between any two contexts of overlapping types.
             contextOfOTPairSimTuple <- getContextOfOTPairSimBySVD context2OverTypeBase    -- (contextOfOTPairList, origSimMatrix, orthSimMatrix, contextOfOTPair2SimList)
             let numOfContextOfOTPair = length (fst4 contextOfOTPairSimTuple)
             putStrLn $ "Num. of context pairs of overtypes: " ++ show numOfContextOfOTPair

             let origSimMatrix = snd4 contextOfOTPairSimTuple
             let origSimMatrixToRows = map toList $ toRows origSimMatrix                         -- [[SimDeg]]
             let origSimDist = case overlap_type_dist_algo of
                                 "Euclidean" -> map (sqrt . (/ 4.0) . sumElements) (toRows (cmap (\x -> x*x) origSimMatrix))
                                 "Manhattan" -> map ((/ 4.0) . sumElements) (toRows origSimMatrix)
             let origSim2DistMatrix = fromLists $ map (\x -> fst x ++ [snd x]) $ zip origSimMatrixToRows origSimDist     -- hmatrix

             let orthSimMatrix = thd4 contextOfOTPairSimTuple
             let orthSimMatrixToRows = map toList $ toRows orthSimMatrix                         -- [[SimDeg]]
             let orthSimDist = case overlap_type_dist_algo of
                                 "Euclidean" -> map (sqrt . (/ 4.0) . sumElements) (toRows (cmap (\x -> x*x) orthSimMatrix))
                                 "Manhattan" -> map ((/ 4.0) . sumElements) (toRows orthSimMatrix)
             let orthSim2DistMatrix = fromLists $ map (\x -> fst x ++ [snd x]) $ zip orthSimMatrixToRows orthSimDist     -- hmatrix

             if (numOfContextOfOTPair > 10)
               then do
                 putStrLn " The first 10 elements of contextOfOTPairList: "
                 dispList 1 (take 10 (fst4 contextOfOTPairSimTuple))
                 putStrLn $ " The first 10 elements of origSim2DistMatrix (Last column is " ++ overlap_type_dist_algo ++ " distance): "
                 disp 4 (origSim2DistMatrix ?? (Take 10, Drop 0))
                 putStrLn $ " The first 10 rows of orthSim2DistMatrix (Last column is " ++ overlap_type_dist_algo ++ " distance): "
                 disp 4 (orthSim2DistMatrix ?? (Take 10, Drop 0))
                 putStrLn " The first 10 rows of contextOfOTPair2SimList: "
                 dispList 1 (take 10 (fth4 contextOfOTPairSimTuple))
               else do
                 putStrLn " contextOfOTPairList: "
                 dispList 1 (fst4 contextOfOTPairSimTuple)
                 putStrLn $ " origSim2DistMatrix (Last column is " ++ overlap_type_dist_algo ++ " distance): "
                 disp 4 origSim2DistMatrix
                 putStrLn $ " orthSim2DistMatrix (Last column is " ++ overlap_type_dist_algo ++ " distance): "
                 disp 4 orthSim2DistMatrix
                 putStrLn " contextOfOTPair2SimList: "
                 dispList 1 (fth4 contextOfOTPairSimTuple)
           else putStrLn "Operation was canceled."
       else putStr ""

    -- 8. Get similarity degrees between any two contexts of overlapping types directly.
    if funcIndex == 8
       then do
         confInfo <- readFile "Configuration"
         let syntax_ambig_resol_model = getConfProperty "syntax_ambig_resol_model" confInfo
         let phra_gram_dist_algo = getConfProperty "phra_gram_dist_algo" confInfo
         let overlap_type_dist_algo = getConfProperty "overlap_type_dist_algo" confInfo
         putStrLn $ " syntax_ambig_resol_model: " ++ syntax_ambig_resol_model
         putStrLn $ " phra_gram_dist_algo: " ++ phra_gram_dist_algo
         putStrLn $ " overlap_type_dist_algo: " ++ overlap_type_dist_algo

         contOrNot <- getLineUntil ("Continue or not [c/n]? (RETURN for 'n') ") ["c","n"] False
         if contOrNot == "c"
           then do
             -- (1) From stru_gene_202408 or stru_gene_202412, collect all contexts of overlapping types.
             startIdx <- getNumUntil "Please input index number of start overtype sample [Return for 1]: " [0 ..]
             endIdx <- getNumUntil "Please input index number of end overtype sample [Return for last]: " [0 ..]
             context2OverTypeBase <- getContext2OverTypeBase startIdx endIdx        -- [(ContextOfOT, OverType)]

             -- (2) Calculate similarity degrees between any two contexts of overlapping types.
             contextOfOTPairSimTuple <- getContextOfOTPairSim context2OverTypeBase

             let origSimList = fst contextOfOTPairSimTuple
             let contextOfOTPair2SimList = snd contextOfOTPairSimTuple
             let origSimListWithDist = zip origSimList (map snd contextOfOTPair2SimList)
             let origSimMatrixWithDist = fromLists $ map (\x -> [(fst4 . fst) x, (snd4 . fst) x, (thd4 . fst) x, (fth4 . fst) x, snd x]) origSimListWithDist

             let numOfContextOfOTPair = length contextOfOTPair2SimList
             putStrLn $ "Num. of context pairs of overtypes: " ++ show numOfContextOfOTPair

             if (numOfContextOfOTPair > 10)
               then do
                 putStrLn $ " The first 10 rows of origSimMatrixWithDist (Last column is " ++ overlap_type_dist_algo ++ " distance): "
                 disp 4 (origSimMatrixWithDist ?? (Take 10, Drop 0))
                 putStrLn " The first 10 rows of contextOfOTPair2SimList: "
                 dispList 1 (take 10 contextOfOTPair2SimList)
               else do
                 putStrLn $ " origSimMatrixWithDist (Last column is " ++ overlap_type_dist_algo ++ " distance): "
                 disp 4 origSimMatrixWithDist
                 putStrLn " contextOfOTPair2SimList: "
                 dispList 1 contextOfOTPair2SimList
           else putStrLn "Operation was canceled."
       else putStr ""

-- 9. Get similarity degrees between any two phrasal overlapping types directly.
    if funcIndex == 9
       then do
         confInfo <- readFile "Configuration"
         let syntax_ambig_resol_model = getConfProperty "syntax_ambig_resol_model" confInfo
         let phra_gram_dist_algo = getConfProperty "phra_gram_dist_algo" confInfo
         let overlap_type_dist_algo = getConfProperty "overlap_type_dist_algo" confInfo
         putStrLn $ " syntax_ambig_resol_model: " ++ syntax_ambig_resol_model
         putStrLn $ " phra_gram_dist_algo: " ++ phra_gram_dist_algo
         putStrLn $ " overlap_type_dist_algo: " ++ overlap_type_dist_algo

         contOrNot <- getLineUntil ("Continue or not [c/n]? (RETURN for 'n') ") ["c","n"] False
         if contOrNot == "c"
           then do
             startIdx <- getNumUntil "Please input index number of start overtype sample [Return for 1]: " [0 ..]
             endIdx <- getNumUntil "Please input index number of end overtype sample [Return for last]: " [0 ..]
             overTypePair2Sim <- getOverTypePair2Sim startIdx endIdx
             putStrLn $ "overTypePair2Sim: " ++ show overTypePair2Sim
           else putStrLn "Operation was canceled."
       else putStr ""

{- 10. Get similarity degrees between any two contexts of StruGene samples by Singular Value Decomposition.
 - ContextOfSG :: (LeftExtend, LeftOver, RightOver, RightExtend, OverType)
 - ContextOfOT :: (LeftExtend, LeftOver, RightOver, RightExtend)
 -}
    if funcIndex == 10
       then do
         confInfo <- readFile "Configuration"
         let syntax_ambig_resol_model = getConfProperty "syntax_ambig_resol_model" confInfo
         let phra_gram_dist_algo = getConfProperty "phra_gram_dist_algo" confInfo
         let overlap_type_dist_algo = getConfProperty "overlap_type_dist_algo" confInfo
         let strugene_context_dist_algo = getConfProperty "strugene_context_dist_algo" confInfo

         putStrLn $ " syntax_ambig_resol_model: " ++ syntax_ambig_resol_model
         putStrLn $ " phra_gram_dist_algo: " ++ phra_gram_dist_algo
         putStrLn $ " overlap_type_dist_algo: " ++ overlap_type_dist_algo
         putStrLn $ " strugene_context_dist_algo: " ++ strugene_context_dist_algo

         contOrNot <- getLineUntil ("Continue or not [c/n]? (RETURN for 'n') ") ["c","n"] False
         if contOrNot == "c"
           then do
             -- (1) From stru_gene_202408 or stru_gene_202412, collect all contexts of StruGene samples.
             startIdx <- getNumUntil "Please input index number of start StruGene sample [Return for 1]: " [0 ..]
             endIdx <- getNumUntil "Please input index number of end StruGene sample [Return for last]: " [0 ..]
             context2ClauTagPriorBase <- getContext2ClauTagPriorBase startIdx endIdx             -- [(ContextOfSG, ClauTagPrior)]
             let context2OverTypeBase = map ((\x -> ((fst5 x, snd5 x, thd5 x, fth5 x), fif5 x)) . fst) context2ClauTagPriorBase   -- [(ContextOfOT, OverType)]

             -- (2) Calculate similarity degrees between any two contexts of StruGene samples.
             contextOfSGPairSimTuple <- getContextOfSGPairSimBySVD context2ClauTagPriorBase

             let origSimMatrix = snd4 contextOfSGPairSimTuple
             let origSimMatrixToRows = map toList $ toRows origSimMatrix                         -- [[SimDeg]]

             let origSimDist = case strugene_context_dist_algo of
                                 "Euclidean" -> map (sqrt . (/ 5.0) . sumElements) (toRows (cmap (\x -> x*x) origSimMatrix))
                                 "Manhattan" -> map ((/ 5.0) . sumElements) (toRows origSimMatrix)
             let origSim2RMSMatrix = fromLists $ map (\x -> fst x ++ [snd x]) $ zip origSimMatrixToRows origSimDist     -- hmatrix

             let orthSimMatrix = thd4 contextOfSGPairSimTuple
             let orthSimMatrixToRows = map toList $ toRows orthSimMatrix                         -- [[SimDeg]]
             let orthSimDist = case strugene_context_dist_algo of
                                 "Euclidean" -> map (sqrt . (/ 5.0) . sumElements) (toRows (cmap (\x -> x*x) orthSimMatrix))
                                 "Manhattan" -> map ((/ 5.0) . sumElements) (toRows orthSimMatrix)
             let orthSim2RMSMatrix = fromLists $ map (\x -> fst x ++ [snd x]) $ zip orthSimMatrixToRows orthSimDist     -- hmatrix

             let numOfContextOfSGPair = rows origSimMatrix                          -- Int, Number of rows.
             putStrLn $ "Num. of context pairs of StruGene samples: " ++ show numOfContextOfSGPair

             if (numOfContextOfSGPair > 10)
               then do
                 putStrLn " The first 10 elements of contextOfSGPairList: "
                 dispList 1 (take 10 (fst4 contextOfSGPairSimTuple))
                 putStrLn $ " The first 10 elements of origSim2RMSMatrix (Last column is " ++ strugene_context_dist_algo ++ " distance): "
                 disp 4 (origSim2RMSMatrix ?? (Take 10, Drop 0))
                 putStrLn " The first 10 rows of orthSim2RMSMatrix: "
                 disp 4 (orthSim2RMSMatrix ?? (Take 10, Drop 0))
                 putStrLn $ " The first 10 rows of contextOfSGPair2SimList (Last column is " ++ strugene_context_dist_algo ++ " distance): "
                 dispList 1 (take 10 (fth4 contextOfSGPairSimTuple))
               else do
                 putStrLn " contextOfSGPairList: "
                 dispList 1 (fst4 contextOfSGPairSimTuple)
                 putStrLn $ " origSim2RMSMatrix (Last column is " ++ strugene_context_dist_algo ++ " distance): "
                 disp 4 origSim2RMSMatrix
                 putStrLn $ " orthSim2RMSMatrix (Last column is " ++ strugene_context_dist_algo ++ " distance): "
                 disp 4 orthSim2RMSMatrix
                 putStrLn " contextOfSGPair2SimList: "
                 dispList 1 (fth4 contextOfSGPairSimTuple)
           else putStrLn "Operation was canceled."
       else putStr ""

{- 11. Get similarity degrees between any two contexts of StruGene samples.
 - ContextOfSG :: (LeftExtend, LeftOver, RightOver, RightExtend, OverType)
 - ContextOfOT :: (LeftExtend, LeftOver, RightOver, RightExtend)
 -}
    if funcIndex == 11
       then do
         confInfo <- readFile "Configuration"
         let syntax_ambig_resol_model = getConfProperty "syntax_ambig_resol_model" confInfo
         let phra_gram_dist_algo = getConfProperty "phra_gram_dist_algo" confInfo
         let overlap_type_dist_algo = getConfProperty "overlap_type_dist_algo" confInfo
         let strugene_context_dist_algo = getConfProperty "strugene_context_dist_algo" confInfo

         putStrLn $ " syntax_ambig_resol_model: " ++ syntax_ambig_resol_model
         putStrLn $ " phra_gram_dist_algo: " ++ phra_gram_dist_algo
         putStrLn $ " overlap_type_dist_algo: " ++ overlap_type_dist_algo
         putStrLn $ " strugene_context_dist_algo: " ++ strugene_context_dist_algo

         contOrNot <- getLineUntil ("Continue or not [c/n]? (RETURN for 'n') ") ["c","n"] False
         if contOrNot == "c"
           then do
             -- (1) From stru_gene_202408 or stru_gene_202412, collect all contexts of StruGene samples.
             startIdx <- getNumUntil "Please input index number of start StruGene sample [Return for 1]: " [0 ..]
             endIdx <- getNumUntil "Please input index number of end StruGene sample [Return for last]: " [0 ..]
             context2ClauTagPriorBase <- getContext2ClauTagPriorBase startIdx endIdx             -- [(ContextOfSG, ClauTagPrior)]
             let context2OverTypeBase = map ((\x -> ((fst5 x, snd5 x, thd5 x, fth5 x), fif5 x)) . fst) context2ClauTagPriorBase   -- [(ContextOfOT, OverType)]

             -- (2) Calculate similarity degrees between any two contexts of StruGene samples.
             contextOfSGPairSimTuple <- getContextOfSGPairSim context2ClauTagPriorBase

             let origSimList = fst contextOfSGPairSimTuple
             let contextOfSGPair2SimList = snd contextOfSGPairSimTuple
             let origSimListWithRMS = zip origSimList (map snd contextOfSGPair2SimList)
             let origSimMatrixWithRMS = fromLists $ map (\x -> [(fst5 . fst) x, (snd5 . fst) x, (thd5 . fst) x, (fth5 . fst) x, (fif5 . fst) x, snd x]) origSimListWithRMS

             let numOfContextOfSGPair = length contextOfSGPair2SimList                          -- Int, Number of rows.
             putStrLn $ "Num. of context pairs of StruGene samples: " ++ show numOfContextOfSGPair

             if (numOfContextOfSGPair > 10)
               then do
                 putStrLn $ " The first 10 rows of origSimMatrixWithRMS (Last column is " ++ strugene_context_dist_algo ++ " distance): "
                 disp 4 (origSimMatrixWithRMS ?? (Take 10, Drop 0))
                 putStrLn " The first 10 rows of contextOfSGPair2SimList: "
                 dispList 1 (take 10 contextOfSGPair2SimList)
               else do
                 putStrLn $ " origSimMatrixWithRMS (Last column is " ++ strugene_context_dist_algo ++ " distance): "
                 disp 4 origSimMatrixWithRMS
                 putStrLn " contextOfSGPair2SimList: "
                 dispList 1 contextOfSGPair2SimList
           else putStrLn "Operation was canceled."
       else putStr ""

{- 12. Among StruGene samples, calculate similarity degrees from one to all contexts.
 - ContextOfSG :: (LeftExtend, LeftOver, RightOver, RightExtend, OverType)
 -}
    if funcIndex == 12
       then do
         ct1 <- getCurrentTime                                                  -- Get current time
         confInfo <- readFile "Configuration"
         let syntax_ambig_resol_model = getConfProperty "syntax_ambig_resol_model" confInfo
         let phra_gram_dist_algo = getConfProperty "phra_gram_dist_algo" confInfo
         let overlap_type_dist_algo = getConfProperty "overlap_type_dist_algo" confInfo
         let strugene_context_dist_algo = getConfProperty "strugene_context_dist_algo" confInfo

         putStrLn $ " syntax_ambig_resol_model: " ++ syntax_ambig_resol_model
         putStrLn $ " phra_gram_dist_algo: " ++ phra_gram_dist_algo
         putStrLn $ " overlap_type_dist_algo: " ++ overlap_type_dist_algo
         putStrLn $ " strugene_context_dist_algo: " ++ strugene_context_dist_algo

         contOrNot <- getLineUntil ("Continue or not [c/n]? (RETURN for 'n') ") ["c","n"] False
         if contOrNot == "c"
           then do
             startIdx <- getNumUntil "Please input index number of start StruGene sample [Return for 1]: " [0 ..]
             endIdx <- getNumUntil "Please input index number of end StruGene sample [Return for last]: " [0 ..]
             context2ClauTagPriorBase <- getContext2ClauTagPriorBase startIdx endIdx             -- [(ContextOfSG, ClauTagPrior)]

             let numOfStruGeneSample = length context2ClauTagPriorBase
             sIdx <- getNumUntil "Please select one sample whose context will compare with others' contexts [Return for 1]: " [1 .. numOfStruGeneSample]
             let contextOfSG = fst (context2ClauTagPriorBase!!(sIdx-1))             -- ContextOfSG
             putStrLn $ "Its context is: " ++ show contextOfSG

             contextOfSGPairSimTuple <- getOneToAllContextOfSGSim contextOfSG context2ClauTagPriorBase

             let origSimList = fst contextOfSGPairSimTuple
             let contextOfSGPair2SimList = snd contextOfSGPairSimTuple
             let origSimListWithDist = zip origSimList (map snd contextOfSGPair2SimList)
             let origSimMatrixWithDist = fromLists $ map (\x -> [(fst5 . fst) x, (snd5 . fst) x, (thd5 . fst) x, (fth5 . fst) x, (fif5 . fst) x, snd x]) origSimListWithDist

             let numOfContextOfSGPair = length contextOfSGPair2SimList              -- Int, Number of rows.
             let listOfContextOfSGWithSim1 = filter (\x -> snd x == 1.0) contextOfSGPair2SimList              -- [(ContextOfSG, ContestOfSG)]
             putStrLn $ "Num. of ClauTagPrior context pairs: " ++ show numOfContextOfSGPair
             putStrLn $ "Num. of ClauTagPrior context pairs with similarity degree 1.0: " ++ show (length listOfContextOfSGWithSim1)

             if (numOfContextOfSGPair > 10)
               then do
                 putStrLn $ " The first 10 rows of origSimMatrixWithDist (Last column is " ++ strugene_context_dist_algo ++ " distance): "
                 disp 4 (origSimMatrixWithDist ?? (Take 10, Drop 0))
                 putStrLn " The first 10 rows of contextOfSGPair2SimList: "
                 dispList 1 (take 10 contextOfSGPair2SimList)
               else do
                 putStrLn $ " origSimMatrixWithDist (Last column is " ++ strugene_context_dist_algo ++ " distance): "
                 disp 4 origSimMatrixWithDist
                 putStrLn " contextOfSGPair2SimList: "
                 dispList 1 contextOfSGPair2SimList
             ct2 <- getCurrentTime
             putStrLn $ "Elapsed time: " ++ show (diffUTCTime ct2 ct1)
           else putStrLn "Operation was canceled."
        else putStr ""

{- 13. Get similarity degrees between one ClauTagPrior context to every context of StruGene samples.
 - ContextOfSG :: (LeftExtend, LeftOver, RightOver, RightExtend, OverType)
 - Input No.4 Sample , it is ([(((s\.np)/#(s\.np))/*np,"Desig","DE")],(np,">","AHn"),(s,"<","SP"),[],2)
 -}
    if funcIndex == 13
       then do
         confInfo <- readFile "Configuration"
         let syntax_ambig_resol_model = getConfProperty "syntax_ambig_resol_model" confInfo
         let phra_gram_dist_algo = getConfProperty "phra_gram_dist_algo" confInfo
         let overlap_type_dist_algo = getConfProperty "overlap_type_dist_algo" confInfo
         let strugene_context_dist_algo = getConfProperty "strugene_context_dist_algo" confInfo

         putStrLn $ " syntax_ambig_resol_model: " ++ syntax_ambig_resol_model
         putStrLn $ " phra_gram_dist_algo: " ++ phra_gram_dist_algo
         putStrLn $ " overlap_type_dist_algo: " ++ overlap_type_dist_algo
         putStrLn $ " strugene_context_dist_algo: " ++ strugene_context_dist_algo

         contOrNot <- getLineUntil ("Continue or not [c/n]? (RETURN for 'n') ") ["c","n"] False
         if contOrNot == "c"
           then do
             startIdx <- getNumUntil "Please input index number of start StruGene sample [Return for 1]: " [0 ..]
             endIdx <- getNumUntil "Please input index number of end StruGene sample [Return for last]: " [0 ..]
             context2ClauTagPriorBase <- getContext2ClauTagPriorBase startIdx endIdx             -- [(ContextOfSG, ClauTagPrior)]

             putStrLn "Please input one StruGene context [LeftExtend, LeftOver, RightOVer, RigheExtend, OverType] to be compared: "
             inputStr <- getLine
             let contextOfSG = readContextOfSGFromStr inputStr                      -- ContextOfSG

             contextOfSGPairSimTuple <- getOneToAllContextOfSGSim' contextOfSG context2ClauTagPriorBase

             let origSimList = fst contextOfSGPairSimTuple
             let contextOfSGPair2SimList = snd contextOfSGPairSimTuple
             let origSimListWithDist = zip origSimList (map snd contextOfSGPair2SimList)
             let origSimMatrixWithDist = fromLists $ map (\x -> [(fst5 . fst) x, (snd5 . fst) x, (thd5 . fst) x, (fth5 . fst) x, (fif5 . fst) x, snd x]) origSimListWithDist

             let numOfContextOfSGPair = length contextOfSGPair2SimList              -- Int, Number of rows.
             let listOfContextOfSGWithSim1 = filter (\x -> snd x == 1.0) contextOfSGPair2SimList              -- [(ContextOfSG, ContestOfSG)]
             putStrLn $ "Num. of ClauTagPrior context pairs: " ++ show numOfContextOfSGPair
             putStrLn $ "Num. of ClauTagPrior context pairs with similarity degree 1.0: " ++ show (length listOfContextOfSGWithSim1)

             context2ClauTagPriorTuple <- findStruGeneSampleByMaxContextSim contextOfSG context2ClauTagPriorBase
             let sIdx = fst3 context2ClauTagPriorTuple
             let simDeg = snd3 context2ClauTagPriorTuple
             let context2ClauTagPrior = thd3 context2ClauTagPriorTuple
             putStrLn $ "Highest similarity degree " ++ show simDeg ++ " is obtained by No." ++ show sIdx ++ " sample: " ++ show context2ClauTagPrior

             if (numOfContextOfSGPair > 10)
               then do
                 putStrLn $ " The first 10 rows of origSimMatrixWithDist (Last column is " ++ strugene_context_dist_algo ++ " distance): "
                 disp 4 (origSimMatrixWithDist ?? (Take 10, Drop 0))
                 putStrLn " The first 10 rows of contextOfSGPair2SimList: "
                 dispList 1 (take 10 contextOfSGPair2SimList)
               else do
                 putStrLn $ " origSimMatrixWithDist (Last column is " ++ strugene_context_dist_algo ++ " distance): "
                 disp 4 origSimMatrixWithDist
                 putStrLn " contextOfSGPair2SimList: "
                 dispList 1 contextOfSGPair2SimList
           else putStrLn "Operation was cancelled."
       else putStr ""

{- 14. Among StruGene samples, calculate similarity degree between every pair of contexts.
 - ContextOfSG :: (LeftExtend, LeftOver, RightOver, RightExtend, OverType)
 -}
    if funcIndex == 14
       then do
         confInfo <- readFile "Configuration"
         let syntax_ambig_resol_model = getConfProperty "syntax_ambig_resol_model" confInfo
         let strugene_context_dist_algo = getConfProperty "strugene_context_dist_algo" confInfo
         let overlap_type_dist_algo = getConfProperty "overlap_type_dist_algo" confInfo
         let phrasyn_sim_tbl = getConfProperty "phrasyn_sim_tbl" confInfo
         let store_csg_sim = getConfProperty "store_csg_sim" confInfo
         let csg_sim_tbl = getConfProperty "csg_sim_tbl" confInfo

         putStrLn $ " syntax_ambig_resol_model: " ++ syntax_ambig_resol_model
         putStrLn $ " strugene_context_dist_algo: " ++ strugene_context_dist_algo
         putStrLn $ " overlap_type_dist_algo: " ++ overlap_type_dist_algo
         putStrLn $ " phrasyn_sim_tbl: " ++ phrasyn_sim_tbl
         putStrLn $ " store_csg_sim: " ++ store_csg_sim
         putStrLn $ " csg_sim_tbl: " ++ csg_sim_tbl

         conn <- getConn
         let sqlstat = DS.fromString $ "CREATE TABLE IF NOT EXISTS " ++ csg_sim_tbl ++ " (id INT UNSIGNED PRIMARY KEY AUTO_INCREMENT, contextofsg1idx INT UNSIGNED, contextofsg2idx INT UNSIGNED, sim DOUBLE)"
         stmt <- prepareStmt conn sqlstat
         executeStmt conn stmt []
         closeStmt conn stmt

         contOrNot <- getLineUntil ("Continue or not [c/n]? (RETURN for 'n') ") ["c","n"] False
         if contOrNot == "c"
           then do
             putStrLn "All to all similarity degree calculations begin ..."
             t1 <- getCurrentTime                -- UTCTime

             startIdx <- getNumUntil "Please input index number of start StruGene sample [Return for 1]: " [0 ..]
             endIdx <- getNumUntil "Please input index number of end StruGene sample [Return for last]: " [0 ..]
             sIdx2ContextOfSGList <- getSIdx2ContextOfSGBase startIdx endIdx         -- [(SIdx, ContextOfSG)]
             let sIdx2ContextOfSGSet = Set.fromList sIdx2ContextOfSGList             -- Set (SIdx, ContextOfSG)
             let sIdx2ContextOfSGPairList = filter (\(x,y) -> x <= y) $ Set.toList $ Set.cartesianProduct sIdx2ContextOfSGSet sIdx2ContextOfSGSet
                                                              -- [((SIdx, ContextOfSG), (SIdx, ContextOfSG))], where commutative unique
--             putStrLn $ "sIdx2ContextOfSGPairList!!0: " ++ show (sIdx2ContextOfSGPairList!!0)
             putStrLn $ "ContextOfSG pair list was created."

             phraSynPair2SimList <- readStaticPhraSynPair2Sim
             let phraSynPair2SimMap = Map.fromList phraSynPair2SimList          -- Map (PhraSyn, PhraSyn) SimDeg
             putStrLn $ "Map (PhraSyn, PhraSyn) SimDeg was created."

             let otPair2SimList = case overlap_type_dist_algo of
                                    "Euclidean" -> [((1,1),1.0),((1,2),7.3351553900482406e-3),((1,3),4.8666647227816856e-2),((1,4),3.3917793725912755e-2),((1,5),0.4247127394531829)
                                                   ,((2,2),1.0),((2,3),0.11777246281776142),((2,4),0.1761503383674159),((2,5),1.0017916991325765e-2)
                                                   ,((3,3),1.0),((3,4),0.5887136000291061),((3,5),7.134132733982824e-2)
                                                   ,((4,4),1.0),((4,5),5.1362972267575656e-2)
                                                   ,((5,5),1.0)]
                                    "Manhattan" -> [((1,1),1.0),((1,2),4.470173439533701e-2),((1,3),0.20418481026327456),((1,4),0.16004358484296605),((1,5),0.12951425063877822)
                                                   ,((2,2),1.0),((2,3),0.14760105015849836),((2,4),0.21749622640726418),((2,5),9.390008726364139e-3)
                                                   ,((3,3),1.0),((3,4),0.5220146998431232),((3,5),5.141417139299777e-2)
                                                   ,((4,4),1.0),((4,5),4.3757227872407406e-2)
                                                   ,((5,5),1.0)]

             forM_ sIdx2ContextOfSGPairList $ \(sc1, sc2) -> do                 -- sc1, sc2 :: (SIdx, ContextOfSG)
               simDeg <- getOneContextOfSGPairSim (snd sc1) (snd sc2) phraSynPair2SimMap
               let csg1idx = toMySQLInt32U (fst sc1)
               let csg2idx = toMySQLInt32U (fst sc2)
               let sim = toMySQLDouble simDeg
               case store_csg_sim of
                 "True" -> do
                   let sqlstat = DS.fromString $ "SELECT id FROM " ++ csg_sim_tbl ++ " where contextofsg1idx = ? and contextofsg2idx = ?"
                   stmt <- prepareStmt conn sqlstat
                   (_, is) <- queryStmt conn stmt [csg1idx, csg2idx]
                   rows <- S.toList is                                 -- [[MySQLValue]]
                   closeStmt conn stmt
                   if rows == []
                     then do             -- Not hit
                       let sqlstat = DS.fromString $ "INSERT INTO " ++ csg_sim_tbl ++ " SET contextofsg1idx = ?, contextofsg2idx = ?, sim = ?"
                       stmt <- prepareStmt conn sqlstat
                       ok <- executeStmt conn stmt [csg1idx, csg2idx, sim]
                       putStrLn $ " Similarity between ContextOfSG sample " ++ show (fst sc1) ++ " and " ++ show (fst sc2) ++ " was interted into Record " ++ show (getOkLastInsertID ok)
                       closeStmt conn stmt
                     else if length rows == 1
                            then do      -- Hit
                              let sqlstat = DS.fromString $ "UPDATE " ++ csg_sim_tbl ++ " SET sim = ? where id = ?"
                              stmt <- prepareStmt conn sqlstat
                              ok <- executeStmt conn stmt [sim, (rows!!0)!!0]
                              putStrLn $ " Update similarity between ContextOfSG sample " ++ show (fst sc1) ++ " and " ++ show (fst sc2) ++ " at Record " ++ show (fromMySQLInt32U ((rows!!0)!!0))
                              closeStmt conn stmt
                            else error $ "clusteringAnalysis: More than one time of hitting on (" ++ (show . fst) sc1 ++ ", " ++ (show . fst) sc2 ++ ")"
                 "False" -> putStrLn "clusteringAnalysis: No database operation."
             t2 <- getCurrentTime                                                            -- UTCTime
             putStrLn $ "\n End with time " ++ show (diffUTCTime t2 t1) ++ " seconds."
           else putStrLn "Operation was canceled."
         close conn                                   -- Close MySQL connection.
       else putStr ""

type SentClauPhraList = [[[PhraCate]]]        -- A list includs sentences, a sentence incluse clauses, and a clause includes phrases, a phrase has a value of PhraCate.
type SimDeg = Double          -- Similarity degree of some kind of grammatic attribue, such as syntatic type, grammatic rule, phrasal type, and so on.
type NumOfPhraSyn = Int       -- PhraSyn :: (Category, Tag, PhraStru, Span)
type NumOfCate = Int
type NumOfCatePair = Int
type NumOfTag = Int
type NumOfTagPair = Int
type NumOfPhraStru = Int
type NumOfStruPair = Int
type NumOfSpan = Int
type NumOfSpanPair = Int
type PhraSynPair2Sim = ((PhraSyn, PhraSyn), SimDeg)

{- From SentClauPhraList, get similarity degree bewteen any two syntatic types (namely categories).
 -}
getTypePair2SimFromSCPL :: SentClauPhraList -> (NumOfPhraSyn, NumOfCate, NumOfCatePair, [((Category, Category), SimDeg)])
getTypePair2SimFromSCPL sentClauPhraList = getTypePair2SimFromPSL phraSynList
    where
    phraSynList = getPhraSynFromSents sentClauPhraList []

{- From PhraSynList, get similarity degree bewteen ALL two syntatic types (namely categories).
 - Similarity degree between two categories satisfies commutative law, sim (cate1, cate2) == sim (cate2, cate1),
 - so only sim (cate1, cate2) where cate1 <= cate2 is calculated.
 - numOfPhraSyn: Number of different PhraSyns, namely different instances of (Category, Tag, PhraStru).
 - numOfCate: Number of different categories.
 - numOfCatePair: Number of different category pairs.
 -}
getTypePair2SimFromPSL :: [PhraSyn] -> (NumOfPhraSyn, NumOfCate, NumOfCatePair, [((Category, Category), SimDeg)])
getTypePair2SimFromPSL phraSynList = (numOfPhraSyn, numOfCate, numOfCatePair, typePair2SimList)
    where
    type2TagStruSpanMap = toType2TagStruSpanMap phraSynList Map.empty           -- Map Category [(Tag, PhraStru, Span)]
    type2TagStruSpanMapList = Map.toList type2TagStruSpanMap                    -- [(Category, [(Tag, PhraStru, Span)])]
    type2TagStruSpanSetList = map (\x -> (fst x, (Set.fromList . snd) x)) type2TagStruSpanMapList      -- [(Category, Set (Tag, PhraStru, Span))]
    typePair2SimList = [((fst x, fst y), (fromIntegral . Set.size) (Set.intersection (snd x) (snd y))
                                       / (fromIntegral . Set.size) (Set.union (snd x) (snd y)))
                                       | x <- type2TagStruSpanSetList, y <- type2TagStruSpanSetList, fst x <= fst y]
    numOfPhraSyn = length phraSynList
    numOfCate = Map.size type2TagStruSpanMap
    numOfCatePair = length typePair2SimList

{- Get all different triple (syntactic type, CCG rule tag, Phrasal structure, phrasal span), namely PhraSyn values.
 - The input is the list of sentential categories.
 -}
getPhraSynFromSents :: [[[PhraCate]]] -> [PhraSyn] -> [PhraSyn]
getPhraSynFromSents [] ctps = ctps                                -- No sentence
getPhraSynFromSents [[]] ctps = ctps                              -- One sentence has no clause to deal with.
getPhraSynFromSents [(c:cs)] ctps = getPhraSynFromSents [cs] (insertPhraList2Ctps c ctps)      -- 'c' is a clause, a list of phrasal categories.
getPhraSynFromSents (sent:sents) ctps = union sentCtps sentsCtps  -- "sent" means a sentence. Union result does not include repetitive element.
    where
    sentCtps = getPhraSynFromSents [sent] ctps
    sentsCtps = getPhraSynFromSents sents ctps

{- Insert a series of PhraSyn values into a List.
 -}
insertPhraList2Ctps :: [PhraCate] -> [PhraSyn] -> [PhraSyn]
insertPhraList2Ctps [] ctps = ctps
insertPhraList2Ctps [x] ctps
    | ruleTag == "Desig" = ctps                                                 -- Neglecting words
    | elem (cate, ruleTag, phraStru, span) ctps = ctps                          -- Duplicate quadruples are omited.
    | otherwise = (cate, ruleTag, phraStru, span) : ctps
    where
    cate = ((!!0) . caOfCate) x
                          -- Apply 'caOfCate' to every phrasal category, and take the first element from the above result.
    ruleTag = ((!!0) . taOfCate) x
                          -- Apply 'taOfCate' to every phrasal category, and take the first element from the above result.
    phraStru = ((!!0) . psOfCate) x
                          -- Apply 'psOfCate' to every phrasal category, and take the first element from the above result.
    span = spOfCate x     -- Apply 'spOfCate' to every phrasal category.

insertPhraList2Ctps (x:xs) ctps = insertPhraList2Ctps xs (insertPhraList2Ctps [x] ctps)

{- The string format is "type_tag_stru_span", type is category, tag is grammatic rule tag, stru is phrasal structure, and span is phrasal span.
 -}
stringToPhraSyn :: String -> PhraSyn
stringToPhraSyn ttps = (getCateFromString (ttps'!!0), ttps'!!1, ttps'!!2, read (ttps'!!3) :: Span)
    where
    ttps' = splitAtDeli '_' ttps

{- From SentClauPhraList, get similarity degree bewteen any two grammtic rules (namely tags).
 -}
getTagPair2SimFromSCPL :: SentClauPhraList -> (NumOfPhraSyn, NumOfTag, NumOfTagPair, [((Tag, Tag), SimDeg)])
getTagPair2SimFromSCPL sentClauPhraList = getTagPair2SimFromPSL phraSynList
    where
    phraSynList = getPhraSynFromSents sentClauPhraList []                             -- [PhraSyn]

{- From [PhraSyn], get similarity degree bewteen any two grammtic rules (namely tags).
 - Similarity degree between two grammatic rules satisfies commutative law, sim (tag1, tag2) == sim (tag2, tag1),
 - so only sim (tag1, tag2) where tag1 <= tag2 is calculated.
 - numOfPhraSyn: Number of different PhraSyns, namely (Category, Tag, PhraStru, Span).
 - numOfTag: Number of different grammatic rules.
 - numOfTagPair: Number of different rule pairs.
 -}
getTagPair2SimFromPSL :: [PhraSyn] -> (NumOfPhraSyn, NumOfTag, NumOfTagPair, [((Tag, Tag), SimDeg)])
getTagPair2SimFromPSL phraSynList = (numOfPhraSyn, numOfTag, numOfTagPair, tagPair2SimList)
    where
    tag2TypeStruSpanMap = toTag2TypeStruSpanMap phraSynList Map.empty           -- Map Tag [(Category, PhraStru, Span)]
    tag2TypeStruSpanMapList = Map.toList tag2TypeStruSpanMap                    -- [(Tag, [(Category, PhraStru, Span)])]
    tag2TypeStruSpanSetList = map (\x -> (fst x, (Set.fromList . snd) x)) tag2TypeStruSpanMapList       -- [(Tag, Set (Category, PhraStru, Span))]
    tagPair2SimList = [((fst x, fst y), (fromIntegral . Set.size) (Set.intersection (snd x) (snd y))
                                        / (fromIntegral . Set.size) (Set.union (snd x) (snd y)))
                                        | x<-tag2TypeStruSpanSetList, y<-tag2TypeStruSpanSetList, fst x <= fst y]
    numOfPhraSyn = length phraSynList
    numOfTag = Map.size tag2TypeStruSpanMap
    numOfTagPair = length tagPair2SimList

{- Get Map Category [(Tag, PhraStru, Span)] from PhraSyn list.
 -}
toType2TagStruSpanMap :: [PhraSyn] -> Map Category [(Tag, PhraStru, Span)] -> Map Category [(Tag, PhraStru, Span)]
toType2TagStruSpanMap [] cate2TPSListMap = cate2TPSListMap
toType2TagStruSpanMap [phraSyn] cate2TPSListMap = Map.insert cate newTPS cate2TPSListMap
    where
    cate = fst4 phraSyn
    tps = (snd4 phraSyn, thd4 phraSyn, fth4 phraSyn)
    newTPS = case (Map.lookup cate cate2TPSListMap) of
               Just tPSs -> case (elem tps tPSs) of
                              True -> tPSs
                              False -> tps : tPSs
               Nothing -> [tps]
toType2TagStruSpanMap (ps:pss) cate2TPSListMap = toType2TagStruSpanMap pss (toType2TagStruSpanMap [ps] cate2TPSListMap)

{- Get Map Tag [(Category, PhraStru, Span)] from PhraSyn list.
 -}
toTag2TypeStruSpanMap :: [PhraSyn] -> Map Tag [(Category, PhraStru, Span)] -> Map Tag [(Category, PhraStru, Span)]
toTag2TypeStruSpanMap [] tag2CPSListMap = tag2CPSListMap
toTag2TypeStruSpanMap [phraSyn] tag2CPSListMap = Map.insert tag newCPS tag2CPSListMap
    where
    tag = snd4 phraSyn
    cps = (fst4 phraSyn, thd4 phraSyn, fth4 phraSyn)
    newCPS = case (Map.lookup tag tag2CPSListMap) of
               Just cPSs -> case (elem cps cPSs) of
                              True -> cPSs
                              False -> cps : cPSs
               Nothing -> [cps]
toTag2TypeStruSpanMap (ps:pss) tag2CPSListMap = toTag2TypeStruSpanMap pss (toTag2TypeStruSpanMap [ps] tag2CPSListMap)

{- Get Map PhraStru [(Category, Tag, Span)] from PhraSyn list.
 -}
toStru2TypeTagSpanMap :: [PhraSyn] -> Map PhraStru [(Category, Tag, Span)] -> Map PhraStru [(Category, Tag, Span)]
toStru2TypeTagSpanMap [] stru2CTSListMap = stru2CTSListMap
toStru2TypeTagSpanMap [phraSyn] stru2CTSListMap = Map.insert stru newCTSs stru2CTSListMap
    where
    stru = thd4 phraSyn
    cts = (fst4 phraSyn, snd4 phraSyn, fth4 phraSyn)
    newCTSs = case (Map.lookup stru stru2CTSListMap) of
                Just cTSs -> case (elem cts cTSs) of
                               True -> cTSs
                               False -> cts : cTSs
                Nothing -> [cts]
toStru2TypeTagSpanMap (ps:pss) stru2CTSListMap = toStru2TypeTagSpanMap pss (toStru2TypeTagSpanMap [ps] stru2CTSListMap)

{- Get Map Span [(Category, Tag, PhraStru)] from PhraSyn list.
 -}
toSpan2TypeTagStruMap :: [PhraSyn] -> Map Span [(Category, Tag, PhraStru)] -> Map Span [(Category, Tag, PhraStru)]
toSpan2TypeTagStruMap [] span2CTPListMap = span2CTPListMap
toSpan2TypeTagStruMap [phraSyn] span2CTPListMap = Map.insert span newCTPs span2CTPListMap
    where
    span = fth4 phraSyn
    ctp = (fst4 phraSyn, snd4 phraSyn, thd4 phraSyn)
    newCTPs = case (Map.lookup span span2CTPListMap) of
                Just cTPs -> case (elem ctp cTPs) of
                               True -> cTPs
                               False -> ctp : cTPs
                Nothing -> [ctp]
toSpan2TypeTagStruMap (ps:pss) span2CTPListMap = toSpan2TypeTagStruMap pss (toSpan2TypeTagStruMap [ps] span2CTPListMap)

{- From SentClauPhraList, get similarity degree bewteen any two phrasal structures.
 -}
getStruPair2SimFromSCPL :: SentClauPhraList -> (NumOfPhraSyn, NumOfPhraStru, NumOfStruPair, [((PhraStru, PhraStru), SimDeg)])
getStruPair2SimFromSCPL sentClauPhraList = getStruPair2SimFromPSL phraSynList
    where
    phraSynList = getPhraSynFromSents sentClauPhraList []                             -- [PhraSyn]

{- From [PhraSyn], get similarity degree bewteen any two phrasal structures.
 - Similarity degree between two phrasal structures satisfies commutative law, sim (stru1, stru2) == sim (stru2, stru1),
 - so only sim (stru1, stru2) where stru1 <= stru2 is calculated.
 - numOfPhraSyn: Number of different PhraSyns, namely (Category, Tag, PhraStru).
 - numOfPhraStru: Number of different phrasal structures.
 - numOfStruPair: Number of different phrase structure pairs.
 -}
getStruPair2SimFromPSL :: [PhraSyn] -> (NumOfPhraSyn, NumOfPhraStru, NumOfStruPair, [((PhraStru, PhraStru), SimDeg)])
getStruPair2SimFromPSL phraSynList = (numOfPhraSyn, numOfPhraStru, numOfStruPair, struPair2SimList)
    where
    stru2TypeTagSpanMap = toStru2TypeTagSpanMap phraSynList Map.empty           -- Map PhraStru [(Category, Tag, Span)]
    stru2TypeTagSpanMapList = Map.toList stru2TypeTagSpanMap                    -- [(PhraStru, [(Category, Tag, Span)])]
    stru2TypeTagSpanSetList = map (\x -> (fst x, (Set.fromList . snd) x)) stru2TypeTagSpanMapList       -- [(PhraStru, Set (Category, Tag, Span))]
    struPair2SimList = [((fst x, fst y), (fromIntegral . Set.size) (Set.intersection (snd x) (snd y))
                                         / (fromIntegral . Set.size) (Set.union (snd x) (snd y)))
                                         | x<-stru2TypeTagSpanSetList, y<-stru2TypeTagSpanSetList, fst x <= fst y]
    numOfPhraSyn = length phraSynList
    numOfPhraStru = Map.size stru2TypeTagSpanMap
    numOfStruPair = length struPair2SimList

{- From SentClauPhraList, get similarity degree bewteen any phrases in their PhraSyn = (Category, Tag, PhraStru, Span).
 - Singular Value Decomposition (SVD) is used in calculating similarity degrees between PhraSyn pairs.
 -}
getPhraSynPairSimFromSCPLBySVD :: DistAlgo -> SentClauPhraList -> IO ([(PhraSyn, PhraSyn)], Matrix Double, Matrix Double, Matrix Double, [((PhraSyn, PhraSyn), SimDeg)])
getPhraSynPairSimFromSCPLBySVD distAlgo sentClauPhraList = getPhraSynPairSimBySVD distAlgo phraSynPairs
    where
    phraSynList = getPhraSynFromSents sentClauPhraList []                       -- [PhraSyn]
    phraSynPairs = [(x, y) | x <- phraSynList, y <- phraSynList, x <= y]        -- [(PhraSyn, PhraSyn)]

{- From SentClauPhraList, get similarity degree bewteen any phrases in their PhraSyn = (Category, Tag, PhraStru, Span).
 - No Singular Value Decomposition (SVD) is used in calculating similarity degrees between PhraSyn pairs.
 -}
getPhraSynPairSimFromSCPL :: DistAlgo -> SentClauPhraList -> IO ([(PhraSyn, PhraSyn)], Matrix Double, [((PhraSyn, PhraSyn), SimDeg)])
getPhraSynPairSimFromSCPL distAlgo sentClauPhraList = getPhraSynPairSim distAlgo phraSynPairs
    where
    phraSynList = getPhraSynFromSents sentClauPhraList []                       -- [PhraSyn]
    phraSynPairs = [(x, y) | x <- phraSynList, y <- phraSynList, x <= y]        -- [(PhraSyn, PhraSyn)]

{- From [PhraSyn], get similarity degree bewteen any two phrasal spans.
 - Similarity degree between two phrasal spans satisfies commutative law, sim (span1, span2) == sim (span2, span1),
 - so only sim (span1, span2) where span1 <= span2 is calculated.
 - numOfSpan: Number of different PhraSyns, namely (Category, Tag, PhraStru, Span).
 - numOfSpan: Number of different phrasal spans.
 - numOfSpanPair: Number of different phrase span pairs.
 -}
getSpanPair2SimFromSCPL :: SentClauPhraList -> (NumOfPhraSyn, NumOfSpan, NumOfSpanPair, [((Span, Span), SimDeg)])
getSpanPair2SimFromSCPL sentClauPhraList = getSpanPair2SimFromPSL phraSynList
    where
    phraSynList = getPhraSynFromSents sentClauPhraList []                       -- [PhraSyn]

{- From [PhraSyn], get similarity degree bewteen any two phrasal spans.
 - Similarity degree between two phrasal spans satisfies commutative law, sim (span1, span2) == sim (span2, span1),
 - so only sim (span1, span2) where span1 <= span2 is calculated.
 - numOfSpan: Number of different PhraSyns, namely (Category, Tag, PhraStru, Span).
 - numOfSpan: Number of different phrasal spans.
 - numOfSpanPair: Number of different phrase span pairs.
 -}
getSpanPair2SimFromPSL :: [PhraSyn] -> (NumOfPhraSyn, NumOfSpan, NumOfSpanPair, [((Span, Span), SimDeg)])
getSpanPair2SimFromPSL phraSynList = (numOfPhraSyn, numOfSpan, numOfSpanPair, spanPair2SimList)
    where
    span2TypeTagStruMap = toSpan2TypeTagStruMap phraSynList Map.empty           -- Map Span [(Category, Tag, PhraStru)]
    span2TypeTagStruMapList = Map.toList span2TypeTagStruMap                    -- [(Span, [(Category, Tag, PhraStru)])]
    span2TypeTagStruSetList = map (\x -> (fst x, (Set.fromList . snd) x)) span2TypeTagStruMapList       -- [(Span, Set (Category, Tag, PhraStru))]
    spanPair2SimList = [((fst x, fst y), (fromIntegral . Set.size) (Set.intersection (snd x) (snd y))
                                          / (fromIntegral . Set.size) (Set.union (snd x) (snd y)))
                                          | x<-span2TypeTagStruSetList, y<-span2TypeTagStruSetList, fst x <= fst y]
    numOfPhraSyn = length phraSynList
    numOfSpan = Map.size span2TypeTagStruMap
    numOfSpanPair = length spanPair2SimList

{- From [(PhraSyn, PhraSyn)], get similarity degree bewteen any phrases in their PhraSyn = (Category, Tag, PhraStru, Span).
 - sim(phraSyn1, phraSyn2) = f(ccSim, ttSim, ppSim, ssSim), where ccSim is similarity degree between categories of these two phrases,
 - ttSim is similarity degree between grammatic rule tags of these two phrases, ppSim is similarity degree between phrase-inner structures,
 - and ssSim is similarity degree between spans of these two phrases.
 - Category, grammatic rule, phrase-inner structure and phrasal spans are not independent with each other, so matrix [[ccSim, ttSim, ssSim]] is converted into
 - matrix [[ccSim', ttSim', ssSim']] acoording to SVD (Singular Value Decomposition) such that ccSim', ttSim' and ssSim' are orthogonal columns.
 - Thus, sim(phraSyn1, phraSyn2) = f(ccSim, ttSim, ppSim, ssSim) = f'(ccSim', ttSim', ppSim', ssSim'), where f' is Euclid length of vector (ccSim', ttSim', ppSim', ssSim').
 - origSimList: List of (ccSim, ttSim, ppSim, ssSim), in which every triple (ccSim, ttSim, ppSim, ssSim) is corresponding to one tuple (phraSyn1, phraSyn2) in phraSynPairList.
 - origSimMatrix:  (k >< 4) [ccSim1, ttSim1, ppSim1, ssSim1, ..., ccSimk, ttSimk, ppSimk,ssSimk], which is matrix form of origSimList.
 - centredSimMatrix: (k >< 4) [ccSim1/ccSimMean, ttSim1/ttSimMean, ppSim1/ppSimMean, ssSim1/ssSimMean, ...], which is centred similarity degree matrix.
 - covSimMatrix: (1/(k-1))(centredSimMatrixT <> centredSimMatrix), where centredSimMatrixT is the transpose of centredSimMatrix.
 - orthSimMatrix: origSimMatrix LA.<> v, where v is the right singular matrix.
 -}
getPhraSynPairSimBySVD :: DistAlgo -> [(PhraSyn, PhraSyn)] -> IO ([(PhraSyn, PhraSyn)], Matrix Double, Matrix Double, Matrix Double, [((PhraSyn, PhraSyn), SimDeg)])
getPhraSynPairSimBySVD distAlgo phraSynPairs = do
    let phraSynList = nub $ (\x -> fst x ++ snd x) $ unzip phraSynPairs         -- [PhraSyn], used to calculate similarity for every grammatic attribute.
    confInfo <- readFile "Configuration"
    let howToGetGramAttrSim = getConfProperty "howToGetGramAttrSim" confInfo

    staticTypePair2Sim <- readStaticTypePair2Sim
    let typePair2Sim = case howToGetGramAttrSim of
                         "dynamic" -> fth4 $ getTypePair2SimFromPSL phraSynList                  -- [((Category, Category), SimDeg)]
                         "static" -> staticTypePair2Sim
                         _ -> error "getPhraSynPairSimBySVD: howToGetGramAttrSim Error."
    staticTagPair2Sim <- readStaticTagPair2Sim
    let tagPair2Sim = case howToGetGramAttrSim of
                        "dynamic" -> fth4 $ getTagPair2SimFromPSL phraSynList                    -- [((Tag, Tag), SimDeg)]
                        "static" -> staticTagPair2Sim
                        _ -> error "getPhraSynPairSimBySVD: howToGetGramAttrSim Error."
    staticStruPair2Sim <- readStaticStruPair2Sim
    let struPair2Sim = case howToGetGramAttrSim of
                         "dynamic" -> fth4 $ getStruPair2SimFromPSL phraSynList                  -- [((PhraStru, PhraStru), SimDeg)]
                         "static" -> staticStruPair2Sim
                         _ -> error "getPhraSynPairSimBySVD: howToGetGramAttrSim Error."
    staticSpanPair2Sim <- readStaticSpanPair2Sim
    let spanPair2Sim = case howToGetGramAttrSim of
                         "dynamic" -> fth4 $ getSpanPair2SimFromPSL phraSynList                  -- [((Span, Span), SimDeg)]
                         "static" -> staticSpanPair2Sim
                         _ -> error "getPhraSynPairSimBySVD: howToGetGramAttrSim Error."

    let numOfPhraSynPair = length phraSynPairs
    let origSimList = [(getSimDegFromAttPair2Sim (fst4 x) (fst4 y) typePair2Sim
                      , getSimDegFromAttPair2Sim (snd4 x) (snd4 y) tagPair2Sim
                      , getSimDegFromAttPair2Sim (thd4 x) (thd4 y) struPair2Sim
                      , getSimDegFromAttPair2Sim (fth4 x) (fth4 y) spanPair2Sim) | (x, y) <- phraSynPairs]
    let origSimMatrix = (numOfPhraSynPair >< 4) $ concat [[fst4 e, snd4 e, thd4 e, fth4 e] | e <- origSimList]     -- hmatrix

{- The following commented codes are replaced with function 'unSym . snd . meanCov'
    origSimMatrixToColumns = map toList $ toColumns origSimMatrix               -- [[SimDeg]]
    simMeanList = map (\x -> sum x / (fromIntegral numOfPhraSynPair)) origSimMatrixToColumns             -- [SimDeg]
    meanMatrix = fromLists $ take numOfPhraSynPair (repeat simMeanList)         -- (numOfPhraSynPair >< 4)
    centredOrigSimMatrix = origSimMatrix - meanMatrix

    centredOrigSimMatrixT = tr' centredOrigSimMatrix                            -- Transpose
    covSimMatrix = cmap (/ (fromIntegral (numOfPhraSynPair - 1))) (centredOrigSimMatrixT LA.<> centredOrigSimMatrix)   -- (4 >< 4)
-}
    let covSimMatrix = unSym $ snd (meanCov origSimMatrix)                          -- Matrix Double (4 >< 4)

-- Get right singular value matrix 'v', by (u,s,v) = svd origSimMatrix
-- Then, convert origSimMatrix to another similarity matrix in which column dimensions are orthogonal.
    let (u, s, v) = svd origSimMatrix
    let orthSimMatrix = origSimMatrix LA.<> v                                       -- (numOfPhraSynPair >< 4)

    let phraSynPairSimList = case distAlgo of
                               "Euclidean" -> map (sqrt . (/ 4.0) . sumElements) (toRows (cmap (\x -> x*x) orthSimMatrix))
                               "Manhattan" -> map ((/ 4.0) . sumElements) (toRows orthSimMatrix)
    let phraSynPair2SimList = zip phraSynPairs phraSynPairSimList     -- [((PhraSyn, PhraSyn), SimDeg)]
    return (phraSynPairs, origSimMatrix, covSimMatrix, orthSimMatrix, phraSynPair2SimList)

{- From [(PhraSyn, PhraSyn)], get similarity degree bewteen any phrases in their PhraSyn = (Category, Tag, PhraStru, Span).
 - sim(phraSyn1, phraSyn2) = f(ccSim, ttSim, ppSim, ssSim), where ccSim is similarity degree between categories of these two phrases,
 - ttSim is similarity degree between grammatic rule tags of these two phrases, ppSim is similarity degree between phrase-inner structures,
 - and ssSim is similarity degree between spans of these two phrases.
 - Category, grammatic rule, phrase-inner structure, and phrasal spans are not independent with each other,
 - so there are various distance algorithms of function 'f'.
 - DistAlgo: Distance algorithm name.
 - origSimList: List of (ccSim, ttSim, ppSim, ssSim), in which every triple (ccSim, ttSim, ssSim) is corresponding to one tuple (phraSyn1, phraSyn2) in phraSynPairList.
 - origSimMatrix:  (k >< 3) [ccSim1, ttSim1, ppSim, ssSim1, ..., ccSimk, ttSimk, ppSimk, ssSimk], which is matrix form of origSimList.
 - How to get similarity degrees on basic grammatic attributes?
 -   dynamic: Collect PhraSyn values, then calculate similarity degrees on grammatic attributes.
 -   static: Precalculate similarity degrees on grammatic attributes, and store them in database.
 -}
getPhraSynPairSim :: DistAlgo -> [(PhraSyn, PhraSyn)] -> IO ([(PhraSyn, PhraSyn)], Matrix Double, [((PhraSyn, PhraSyn), SimDeg)])
getPhraSynPairSim distAlgo phraSynPairs = do
    let phraSynList = nub $ (\x -> fst x ++ snd x) $ unzip phraSynPairs         -- [PhraSyn], used to calculate similarity for every grammatic attribute.
    confInfo <- readFile "Configuration"
    let howToGetGramAttrSim = getConfProperty "howToGetGramAttrSim" confInfo

    staticTypePair2Sim <- readStaticTypePair2Sim
    let typePair2Sim = case howToGetGramAttrSim of
                         "dynamic" -> fth4 $ getTypePair2SimFromPSL phraSynList                  -- [((Category, Category), SimDeg)]
                         "static" -> staticTypePair2Sim
                         _ -> error "getPhraSynPairSim: howToGetGramAttrSim Error."
--    putStrLn $ "getPhraSynPairSim: typePair2Sim!!0 = " ++ show (typePair2Sim!!0)
    staticTagPair2Sim <- readStaticTagPair2Sim
    let tagPair2Sim = case howToGetGramAttrSim of
                        "dynamic" -> fth4 $ getTagPair2SimFromPSL phraSynList                    -- [((Tag, Tag), SimDeg)]
                        "static" -> staticTagPair2Sim
                        _ -> error "getPhraSynPairSim: howToGetGramAttrSim Error."
--    putStrLn $ "getPhraSynPairSim: size of tagPair2Sim = " ++ show (length tagPair2Sim)
    staticStruPair2Sim <- readStaticStruPair2Sim
    let struPair2Sim = case howToGetGramAttrSim of
                         "dynamic" -> fth4 $ getStruPair2SimFromPSL phraSynList                  -- [((PhraStru, PhraStru), SimDeg)]
                         "static" -> staticStruPair2Sim
                         _ -> error "getPhraSynPairSim: howToGetGramAttrSim Error."
--    putStrLn $ "getPhraSynPairSim: size of struPair2Sim = " ++ show (length struPair2Sim)
    staticSpanPair2Sim <- readStaticSpanPair2Sim
    let spanPair2Sim = case howToGetGramAttrSim of
                         "dynamic" -> fth4 $ getSpanPair2SimFromPSL phraSynList                  -- [((Span, Span), SimDeg)]
                         "static" -> staticSpanPair2Sim
                         _ -> error "getPhraSynPairSim: howToGetGramAttrSim Error."
--    putStrLn $ "getPhraSynPairSim: size of spanPair2Sim = " ++ show (length spanPair2Sim)

    let numOfPhraSynPair = length phraSynPairs
    putStrLn $ "getPhraSynPairSim: Pair num. of PhraSyn values = " ++ show numOfPhraSynPair

    let origSimList = [(getSimDegFromAttPair2Sim (fst4 x) (fst4 y) typePair2Sim
                      , getSimDegFromAttPair2Sim (snd4 x) (snd4 y) tagPair2Sim
                      , getSimDegFromAttPair2Sim (thd4 x) (thd4 y) struPair2Sim
                      , getSimDegFromAttPair2Sim (fth4 x) (fth4 y) spanPair2Sim) | (x, y) <- phraSynPairs]
    let origSimMatrix = (numOfPhraSynPair >< 4) $ concat [[fst4 e, snd4 e, thd4 e, fth4 e] | e <- origSimList]    -- hmatrix

    let phraSynPairSimList = case distAlgo of
                               "Euclidean" -> map (sqrt . (/ 4.0) . sumElements) (toRows (cmap (\x -> x*x) origSimMatrix))
                               "Manhattan" -> map ((/ 4.0) . sumElements) (toRows origSimMatrix)
    let phraSynPair2SimList = zip phraSynPairs phraSynPairSimList               -- [((PhraSyn, PhraSyn), SimDeg)]
    return (phraSynPairs, origSimMatrix, phraSynPair2SimList)

-- Get PhraSyn value pair from a similarity degree vector.
getPhraSynPairFromSimDegVector :: ( ((Category, Category), SimDeg)
                                  , ((Tag, Tag), SimDeg)
                                  , ((PhraStru, PhraStru), SimDeg)
                                  , ((Span, Span), SimDeg)
                                  ) -> (PhraSyn, PhraSyn)
getPhraSynPairFromSimDegVector (ccSim, ttSim, ppSim, ssSim) = (phraSyn1, phraSyn2)
    where
    phraSyn1 = ((fst . fst) ccSim, (fst . fst) ttSim, (fst . fst) ppSim, (fst . fst) ssSim)
    phraSyn2 = ((snd . fst) ccSim, (snd . fst) ttSim, (snd . fst) ppSim, (snd . fst) ssSim)

-- Given two attribute values, get their similarity degree from attribute Similarity degree list.
getSimDegFromAttPair2Sim :: (Eq a, Show a) => a -> a -> [((a, a), SimDeg)] -> SimDeg
getSimDegFromAttPair2Sim a1 a2 [] = error $ "getSimDegFromAttPair2Sim: Similarity degree list has been exhausted. (a1, a2) = " ++ show (a1,a2)
getSimDegFromAttPair2Sim a1 a2 (x:xs)
    | (a1, a2) == fst x || (a2, a1) == fst x = snd x       -- Commutative law in xx2Sim List.
    | otherwise = getSimDegFromAttPair2Sim a1 a2 xs

{- Reading parsing trees from start sentence to end sentence in treebank, get [[[PhraCate]]] from which
 - [PhraSynPair2Sim] is returned, where PhraSynPair2Sim :: ((PhraSyn, PhraSyn), SimDeg).
 - If start index and end index are both 0, then default start index and default end index are used.
 - which are defined in file Configuration.
 - The treebank is the database table property 'tree_souce' indicates.
 -}
getPhraSynPair2SimFromTreebank :: SentIdx -> SentIdx -> IO (Map (PhraSyn, PhraSyn) SimDeg)
getPhraSynPair2SimFromTreebank startIdx endIdx = do
    sentClauPhraList <- getSentClauPhraList startIdx endIdx
    confInfo <- readFile "Configuration"
    let phra_gram_dist_algo = getConfProperty "phra_gram_dist_algo" confInfo
    phraSynPairSimTuple <- getPhraSynPairSimFromSCPL phra_gram_dist_algo sentClauPhraList
    return $ Map.fromList (thd3 phraSynPairSimTuple)

{- Extract all pairs of PhraSyn values from [ContextOfOT], who can appear in calculating their grammatic similarities.
 - From [ContextOfOT], get [LeftExtend], [LeftOver], [RightOver] and [RightExtend].
 - Only coming from same part of overtype contexts, such as LeftExtend, two PhraSyn values has similarity calculation.
 - Form PhraSyn pair sets respectively for different grammtic parts, where first PhraSyn value is less than or equal to the second.
 - For every pair, calculate their similarity. Finally return Map (PhraSyn, PhraSyn) SimDeg.
 -}
getPhraSynPair2SimFromCOT :: DistAlgo -> [ContextOfOT] -> IO (Map (PhraSyn, PhraSyn) SimDeg)
getPhraSynPair2SimFromCOT distAlgo contextOfOTList = do
    let (les, los, ros, res) = unzip4 contextOfOTList                               -- Extract [LeftExtend], [LeftOver], [RightOver] and [RightExtend].
    let les' = Set.fromList les                                   -- Remove duplicates, Set LeftExtend
    let los' = Set.fromList los                                   -- Remove duplicates, Set LeftOver
    let ros' = Set.fromList ros                                   -- Remove duplicates, Set RightOver
    let res' = Set.fromList res                                   -- Remove duplicates, Set RightExtend

    let leps = Set.cartesianProduct les' les'        -- Set (LeftExtend, LeftExtend)
    let lops = Set.cartesianProduct los' los'        -- Set (LeftOver, LeftOver)
    let rops = Set.cartesianProduct ros' ros'        -- Set (RightOver, RightOver)
    let reps = Set.cartesianProduct res' res'        -- Set (RightExtend, RightExtend)

    let lePhraSynPairs = Set.fromList [(u, v) | (le1, le2) <- Set.toList leps, u <- le1, v <- le2]   -- Set (PhraSyn, PhraSyn)
    let loPhraSynPairs = lops
    let roPhraSynPairs = rops
    let rePhraSynPairs = Set.fromList [(u, v) | (re1, re2) <- Set.toList reps, u <- re1, v <- re2]   -- Set (PhraSyn, PhraSyn)

    let phraSynPairSet = Set.union (Set.union (Set.union lePhraSynPairs loPhraSynPairs) roPhraSynPairs) rePhraSynPairs   -- Set (PhraSyn, PhraSyn)

    let phraSynPairs = filter (\(ps1, ps2) -> ps1 <= ps2) $ Set.toList phraSynPairSet     -- [(PhraSyn, PhraSyn)], where Set.toList is in not-descending order.

    putStrLn $ "getPhraSynPair2SimFromCOT: Num. of PhraSyn value pairs = " ++ (show . length) phraSynPairs
    phraSynPairSimTuple <- getPhraSynPairSim distAlgo phraSynPairs
    return $ Map.fromList (thd3 phraSynPairSimTuple)

{- Extract all pairs of PhraSyn values from [ContextOfSG], who can appear in calculating their grammatic similarities.
 - From [ContextOfSG], get [LeftExtend], [LeftOver], [RightOver] and [RightExtend].
 - Only coming from same part of contexts, such as LeftExtend, two PhraSyn values has similarity calculation.
 - Form PhraSyn pair sets respectively for different grammtic parts, every pair in which satisfies first PhraSyn value is less than or equal to the second.
 - For every pair, calculate their similarity. Finally return Map (PhraSyn, PhraSyn) SimDeg.
 - This function have not been used yet.
 -}
getPhraSynPair2SimFromCSG :: DistAlgo -> [ContextOfSG] -> IO (Map (PhraSyn, PhraSyn) SimDeg)
getPhraSynPair2SimFromCSG distAlgo contextOfSGList = do
    let (les, los, ros, res, _) = unzip5 contextOfSGList                            -- Extract [LeftExtend], [LeftOver], [RightOver] and [RightExtend].
    let les' = Set.fromList les                                   -- Remove duplicates, Set LeftExtend
    let los' = Set.fromList los                                   -- Remove duplicates, Set LeftOver
    let ros' = Set.fromList ros                                   -- Remove duplicates, Set RightOver
    let res' = Set.fromList res                                   -- Remove duplicates, Set RightExtend

    let leps = Set.cartesianProduct les' les'        -- Set (LeftExtend, LeftExtend)
    let lops = Set.cartesianProduct los' los'        -- Set (LeftOver, LeftOver)
    let rops = Set.cartesianProduct ros' ros'        -- Set (RightOver, RightOver)
    let reps = Set.cartesianProduct res' res'        -- Set (RightExtend, RightExtend)

    let lePhraSynPairs = Set.fromList [(u, v) | (le1, le2) <- Set.toList leps, u <- le1, v <- le2]   -- Set (PhraSyn, PhraSyn)
    let loPhraSynPairs = lops
    let roPhraSynPairs = rops
    let rePhraSynPairs = Set.fromList [(u, v) | (re1, re2) <- Set.toList reps, u <- re1, v <- re2]   -- Set (PhraSyn, PhraSyn)

    let phraSynPairSet = Set.union (Set.union (Set.union lePhraSynPairs loPhraSynPairs) roPhraSynPairs) rePhraSynPairs   -- Set (PhraSyn, PhraSyn)
    let phraSynPairs = filter (\(ps1, ps2) -> ps1 <= ps2) $ Set.toList phraSynPairSet     -- [(PhraSyn, PhraSyn)], where Set.toList is in not-descending order.

    phraSynPairSimTuple <- getPhraSynPairSim distAlgo phraSynPairs
    return $ Map.fromList (thd3 phraSynPairSimTuple)

{- Reading parsing trees from start sentence to end sentence in treebank, get [[[PhraCate]]].
 - If start index and end index are both 0, then default start index and default end index are used, which are defined in file Configuration.
 - The treebank is the database table property 'tree_souce' indicates.
 -}
getSentClauPhraList :: SentIdx -> SentIdx -> IO SentClauPhraList
getSentClauPhraList startIdx endIdx = do
    conn <- getConn
    confInfo <- readFile "Configuration"                                        -- Read the local configuration file
    let tree_source = getConfProperty "tree_source" confInfo
    let startIdx' = case (startIdx == 0) of
                      True -> read (getConfProperty "defaultStartIdx" confInfo) :: Int
                      False -> startIdx
    let endIdx' = case (endIdx == 0) of
                    True -> read (getConfProperty "defaultEndIdx" confInfo) :: Int
                    False -> endIdx
    putStrLn $ "stardIdx = " ++ show startIdx' ++ " , endIdx = " ++ show endIdx'
    putStrLn $ "The source of parsing trees is set as: " ++ tree_source         -- Display the source of parsing trees
    let sqlstat = DS.fromString $ "select tree from " ++ tree_source ++ " where serial_num >= ? and serial_num <= ?"
    stmt <- prepareStmt conn sqlstat
    (defs, is) <- queryStmt conn stmt [toMySQLInt32 startIdx', toMySQLInt32 endIdx']

    sentStrList <- readStreamByText [] is                     -- [String], here a string is the parsing result of a sentence.
    let sentClauStrList = map stringToList sentStrList        -- [[String]], here a string is the parsing result of a clause.
    let sentClauNumList = map length sentClauStrList          -- [Int], here an integer is the number of clauses in a sentence.
    let sentNum = length sentStrList                          -- The number of sentences.
    let clauseTotalNum = foldl (+) 0 sentClauNumList          -- The total number of clauses.

    let sentClauTreeList = map (map readTree) sentClauStrList     -- [[Tree]], here Tree ::= (ClauIdx, [PhraCate])
    let sentClauPhraList = map (map snd) sentClauTreeList     -- [[[PhraCate]]], here a PhraCate is the representation in memory of a phrase.
    return sentClauPhraList

{- Get similarity degree between two phrasal sets in their grammatical features.
 - (1) Suppose set A and B are [PhraSyn]. Get upper triangular matrix of similarity degrees, where sim(a,b) is similarity of (a,b).
 -     M = [((a,b), sim(a,b)) | (a,b) in A X B, a <= b]
 - (2) Match elements between A and B according to maximum similarity degree, forming list L.
 - (3) If |A| < |B|. For every element b in B, which does not appear in L, append ((nullPhraSyn, b), 0) to L.
 - (4) If |A| > |B|. For every element a in A, which does not appear in L, append ((a, nullPhraSyn), 0) to L.
 - Finally, return mean value of similarity degrees of all matched PhraSyn value pairs.
 - In Map (PhraSyn, PhraSyn) SimDeg, every key (x,y) satisfies order of x <= y.
 -}
getPhraSynSetSim :: [PhraSyn] -> [PhraSyn] -> Map (PhraSyn, PhraSyn) SimDeg -> SimDeg
getPhraSynSetSim [] [] _ = 1.0           -- Two empty sets
getPhraSynSetSim _ [] _ = 0.0
getPhraSynSetSim [] _ _ = 0.0
getPhraSynSetSim psl1 psl2 phraSynPair2SimMap = simDeg
    where
    phraSynPairMatrix = [(a,b) | a <- zip [1..] psl1, b <- zip [1..] psl2]      -- [((RowIdx, PhraSyn), (ColIdx, PhraSyn))], a tuple matrix.
    phraSynPair2SimMatrix = map (\x -> (((fst . fst) x, (fst . snd) x), (toPhraSynPair2Sim phraSynPair2SimMap) ((snd . fst) x, (snd . snd) x))) phraSynPairMatrix
                                                                                -- [((RowIdx, ColIdx), ((PhraSyn, PhraSyn), SimDeg))]
    matchedPhraSynPair2SimList = map snd $ getMatchedElemPair2SimList [] phraSynPair2SimMatrix      -- [((PhraSyn, PhraSyn), SimDeg)]
    allMatchedPhraSynPair2SimList = case (signum (length psl1 - (length psl2))) of
      -1 -> matchedPhraSynPair2SimList ++ [((nullPhraSyn, x), 0.0) | x <- psl2, notElem x (map (snd . fst) matchedPhraSynPair2SimList)]
      0 -> matchedPhraSynPair2SimList
      1 -> matchedPhraSynPair2SimList ++ [((x, nullPhraSyn), 0.0) | x <- psl1, notElem x (map (fst . fst) matchedPhraSynPair2SimList)]
    simDeg = foldl (+) 0.0 (map snd allMatchedPhraSynPair2SimList) / (fromIntegral (length allMatchedPhraSynPair2SimList))

{- Convert (PhraSyn, PhraSyn) to ((PhraSyn, PhraSyn), SimDeg) by looking Map (PhraSyn, PhraSyn) SimDeg.
 - For every key (x,y) in Map (PhraSyn, PhraSyn) SimDeg, x <= y.
 -}
toPhraSynPair2Sim :: Map (PhraSyn, PhraSyn) SimDeg -> (PhraSyn, PhraSyn) -> ((PhraSyn, PhraSyn), SimDeg)
toPhraSynPair2Sim phraSynPair2SimMap (ps1, ps2) = case (Map.lookup (ps1, ps2) phraSynPair2SimMap) of
    Just x -> ((ps1, ps2), x)
    Nothing -> case (Map.lookup (ps2, ps1) phraSynPair2SimMap) of
                 Just x -> ((ps1, ps2), x)
                 Nothing -> error $ "toPhraSynPair2Sim: Key (" ++ show ps1 ++ ", " ++ show ps2 ++ ") does not exist."

-- Types for calculating similarity degree of phrasal overlapping types.
type NumOfContextOfOT = Int       -- ContextOfOT :: (LeftExtend, LeftOver, RightOver, RightExtend)
type NumOfLeftExtend = Int
type NumOfLeftOver = Int
type NumOfRightOver = Int
type NumOfRightExtend = Int
type NumOfLeftExtendPair = Int
type NumOfLeftOverPair = Int
type NumOfRightOverPair = Int
type NumOfRightExtendPair = Int
type ContextOfOTPair2Sim = ((ContextOfOT, ContextOfOT), SimDeg)

{- Reading overtype and its context from the sample database of a certain syntax_ambig_resol_model, such as 'stru_gene_202408'.
 - If start index and end index are both 0, then all records in sample database will be read.
 - Context2OverTypeBase :: [Context2OverType]
 - Context2OverType :: (ContextOfOT, OverType)
 - ContextOfOT :: (LeftExtend, LeftOver, RightOver, RightExtend)
 -}
getContext2OverTypeBase :: SIdx -> SIdx -> IO Context2OverTypeBase
getContext2OverTypeBase startIdx endIdx = do
    conn <- getConn
    confInfo <- readFile "Configuration"                                        -- Read the local configuration file
    let syntax_ambig_resol_model = getConfProperty "syntax_ambig_resol_model" confInfo
    case syntax_ambig_resol_model of
      x | elem x ["stru_gene_202408", "stru_gene_202412", "stru_gene_202501"] -> do
          let startIdx' = case startIdx of
                            0 -> 1
                            _ -> startIdx
          let sqlstat = DS.fromString $ "select count(*) from " ++ syntax_ambig_resol_model
          (defs, is) <- query_ conn sqlstat
          rows <- readStreamByInt64 [] is
          let endIdx' = case endIdx of
                          0 -> head rows
                          _ -> endIdx

          putStrLn $ "stardIdx = " ++ show startIdx' ++ " , endIdx = " ++ show endIdx'

          putStrLn $ "The source of overtypes and their contexts is set as: " ++ syntax_ambig_resol_model     -- Display the source of overtypes and their contexts
          let sqlstat = DS.fromString $ "select leftExtend, leftOver, rightOver, rightExtend, overType from " ++ syntax_ambig_resol_model ++ " where id >= ? and id <= ?"
          stmt <- prepareStmt conn sqlstat
          (defs, is) <- queryStmt conn stmt [toMySQLInt32U startIdx', toMySQLInt32U endIdx']

          context2OverTypeBase <- readStreamByContext2OverType [] is            -- [Context2OverType]
          let context2OverTypeNum = length context2OverTypeBase                 -- The number of Context2OverType samples.
          putStrLn $ "getContext2OverTypeBase: context2OverTypeNum = " ++ show context2OverTypeNum
          return context2OverTypeBase
      _ -> do
        putStrLn "getContext2OverTypeBase: Value of property 'syntax_ambig_resol_model' does not match any MySQL table."
        return []

{- Get similarity degree between two overtype contexts, namely two vectors of (LeftExtend, LeftOver, RightOver, RightExtend).
 - sim(contextOfOT1, contextOfOT2) = f(leSim, loSim, roSim, reSim), where
 -     leSim is similarity degree between two LeftExtends, namely two PhraSyn sets.
 -     loSim is similarity degree between two LeftOvers, namely two PhraSyns.
 -     roSim is similarity degree between two RightOvers, namely two PhraSyns.
 -     reSim is similarity degree between two RightExtends, namely two PhraSyn sets.
 - LeftExtend, LeftOver, RightOver, and RightExtend, are not independent with each other, so matrix [[leSim, loSim, roSim, reSim]] is converted into
 - matrix [[leSim', loSim', roSim', reSim']] acoording to SVD (Singular Value Decomposition) such that leSim', loSim', roSim' and reSim'' are orthogonal columns.
 - Thus, sim(contextOfOT1, contextOfOT2) = f(leSim, loSim, roSim, reSim) = f'(leSim', loSim', roSim', reSim'), where f' is Euclid length of vector (leSim', loSim', roSim', reSim').
 -
 - contextOfOTPairList: List of (ContextOfOT, ContextOfOT), in every element of which first ContextOfOT value is less than or equal to the second.
 - origSimList: List of (leSim, loSim, roSim, reSim), in which every quadtuple (leSim, loSim, roSim, reSim) is corresponding to one tuple (contextOfOT1, contextOfOT2) in contextOfOTPairList.
 - origSimMatrix:  (k >< 4) [leSim1, loSim1, roSim1, reSim1, ..., leSimk, loSimk, roSimk, reSimk], which is matrix form of origSimList.
 - orthSimMatrix: origSimMatrix LA.<> v, where v is the right singular matrix.
 -}
getContextOfOTPairSimBySVD :: Context2OverTypeBase -> IO ([(ContextOfOT, ContextOfOT)], Matrix Double, Matrix Double, [((ContextOfOT, ContextOfOT), SimDeg)])
getContextOfOTPairSimBySVD context2OverTypeBase = do
    let contextOfOTList = nub $ map fst context2OverTypeBase                    -- [ContextOfOT], here there is no repetitive ContextOfOT values.
    putStrLn $ "Num. of repetitive ContextOfOT values = " ++ show (length context2OverTypeBase - (length contextOfOTList))

    confInfo <- readFile "Configuration"
    let phra_gram_dist_algo = getConfProperty "phra_gram_dist_algo" confInfo    -- Distance algorithm for phrasal grammar aspects

    phraSynPair2SimMap <- getPhraSynPair2SimFromCOT phra_gram_dist_algo contextOfOTList    -- Map (PhraSyn, PhraSyn) SimDeg
    let leftExtendList = map fst4 contextOfOTList                                      -- [LeftExtend]
    let lePair2SimList = getPhraSynSetPair2Sim leftExtendList phraSynPair2SimMap       -- [((LeftExtend, LeftExtend), SimDeg)]
    let leftOverList = map snd4 contextOfOTList                                        -- [LeftOver]
    let loPair2SimList = getPhraSynPair2Sim leftOverList phraSynPair2SimMap            -- [((LeftOver, LeftOver), SimDeg)]
    let rightOverList = map thd4 contextOfOTList                                       -- [RightOver]
    let roPair2SimList = getPhraSynPair2Sim rightOverList phraSynPair2SimMap           -- [((RightOver, RightOver), SimDeg)]
    let rightExtendList = map fth4 contextOfOTList                                     -- [RightExtend]
    let rePair2SimList = getPhraSynSetPair2Sim rightExtendList phraSynPair2SimMap      -- [((RightExtend, RightExtend), SimDeg)]

    let contextOfOTPairList = [(x, y) | x <- contextOfOTList, y <- contextOfOTList, x <= y]   -- [(ContextOfOT, ContextOfOT)]
                                                      -- Ord definition is exhaustive, here there is no repetitive pairs.
    let numOfContextOfOTPair = length contextOfOTPairList
    let origSimList = [(getSimDegFromAttPair2Sim (fst4 x) (fst4 y) lePair2SimList
                      , getSimDegFromAttPair2Sim (snd4 x) (snd4 y) loPair2SimList
                      , getSimDegFromAttPair2Sim (thd4 x) (thd4 y) roPair2SimList
                      , getSimDegFromAttPair2Sim (fth4 x) (fth4 y) rePair2SimList) | (x, y) <- contextOfOTPairList]

    let origSimMatrix = (numOfContextOfOTPair >< 4) $ concat [[fst4 e, snd4 e, thd4 e, fth4 e] | e <- origSimList]       -- hmatrix

    let (u, s, v) = svd origSimMatrix
    let orthSimMatrix = origSimMatrix LA.<> v

    confInfo <- readFile "Configuration"
    let overlap_type_dist_algo = getConfProperty "overlap_type_dist_algo" confInfo

    let contextOfOTPairSimList = case overlap_type_dist_algo of
                                   "Euclidean" -> map (sqrt . (/ 4.0) . sumElements) (toRows (cmap (\x -> x*x) orthSimMatrix))
                                   "Manhattan" -> map ((/ 4.0) . sumElements) (toRows orthSimMatrix)
    let contextOfOTPair2SimList = zip contextOfOTPairList contextOfOTPairSimList     -- [((ContextOfOT, ContextOfOT), SimDeg)]

    return (contextOfOTPairList, origSimMatrix, orthSimMatrix, contextOfOTPair2SimList)

{- Get similarity degree between two overtype contexts, namely two vectors of (LeftExtend, LeftOver, RightOver, RightExtend).
 - sim(contextOfOT1, contextOfOT2) = f(leSim, loSim, roSim, reSim), where
 -     leSim is similarity degree between two LeftExtends, namely two PhraSyn sets.
 -     loSim is similarity degree between two LeftOvers, namely two PhraSyns.
 -     roSim is similarity degree between two RightOvers, namely two PhraSyns.
 -     reSim is similarity degree between two RightExtends, namely two PhraSyn sets.
 -     f is Root Mean Square (RMS) metric in four-dimensional space.
 - contextOfOTPairList: List of (ContextOfOT, ContextOfOT), in every element of which first ContextOfOT value is less than or equal to the second.
 - origSimList: List of (leSim, loSim, roSim, reSim), in which every quadtuple (leSim, loSim, roSim, reSim) is corresponding to one tuple (contextOfOT1, contextOfOT2) in contextOfOTPairList.
 -}
getContextOfOTPairSim :: Context2OverTypeBase -> IO ([(SimDeg, SimDeg, SimDeg, SimDeg)], [((ContextOfOT, ContextOfOT), SimDeg)])
getContextOfOTPairSim context2OverTypeBase = do
    let contextOfOTList = nub $ map fst context2OverTypeBase                    -- [ContextOfOT], here there is no repetitive ContextOfOT values.
    putStrLn $ "Num. of repetitive ContextOfOT values = " ++ show (length context2OverTypeBase - (length contextOfOTList))

    confInfo <- readFile "Configuration"
    let phra_gram_dist_algo = getConfProperty "phra_gram_dist_algo" confInfo

    phraSynPair2SimMap <- getPhraSynPair2SimFromCOT phra_gram_dist_algo contextOfOTList     -- Map (PhraSyn, PhraSyn) SimDeg
    let leftExtendList = map fst4 contextOfOTList                               -- [LeftExtend]
    let lePair2SimList = getPhraSynSetPair2Sim leftExtendList phraSynPair2SimMap      -- [((LeftExtend, LeftExtend), SimDeg)]
    let leftOverList = map snd4 contextOfOTList                                       -- [LeftOver]
    let loPair2SimList = getPhraSynPair2Sim leftOverList phraSynPair2SimMap           -- [((LeftOver, LeftOver), SimDeg)]
    let rightOverList = map thd4 contextOfOTList                                      -- [RightOVer]
    let roPair2SimList = getPhraSynPair2Sim rightOverList phraSynPair2SimMap          -- [((RightOver, RightOver), SimDeg)]
    let rightExtendList = map fth4 contextOfOTList                                    -- [RightExtend]
    let rePair2SimList = getPhraSynSetPair2Sim rightExtendList phraSynPair2SimMap     -- [((RightExtend, RightExtend), SimDeg)]

    let contextOfOTPairList = [(x, y) | x <- contextOfOTList, y <- contextOfOTList, x <= y]      -- [(ContextOfOT, ContextOfOT)]
                                                                -- Ord definition is exhaustive, here there is no repetitive pairs.
    let origSimList = [(getSimDegFromAttPair2Sim (fst4 x) (fst4 y) lePair2SimList
                      , getSimDegFromAttPair2Sim (snd4 x) (snd4 y) loPair2SimList
                      , getSimDegFromAttPair2Sim (thd4 x) (thd4 y) roPair2SimList
                      , getSimDegFromAttPair2Sim (fth4 x) (fth4 y) rePair2SimList) | (x, y) <- contextOfOTPairList]

    confInfo <- readFile "Configuration"
    let overlap_type_dist_algo = getConfProperty "overlap_type_dist_algo" confInfo

    let simList = case overlap_type_dist_algo of
                    "Euclidean" -> [sqrt (sum [les * les, los * los, ros * ros, res * res] / 4.0) | (les,los,ros,res) <- origSimList]
                    "Manhattan" -> [sum [les, los, ros, res] / 4.0 | (les,los,ros,res) <- origSimList]
    let contextOfOTPair2SimList = zip contextOfOTPairList simList     -- [((ContextOfOT, ContextOfOT), SimDeg)]
    return (origSimList, contextOfOTPair2SimList)

{- Get similarity degree bewteen any two PhraSyn sets, where PhraSyn set denotes LeftExtend and RightExtend.
 - Similarity degree between two PhraSyn sets satisfies commutative law, sim (s1, s2) == sim (s2, s1),
 - so only sim (s1, s2) where s1 <= s2 is calculated.
 -}
getPhraSynSetPair2Sim :: [[PhraSyn]] -> Map (PhraSyn, PhraSyn) SimDeg -> [(([PhraSyn], [PhraSyn]), SimDeg)]
getPhraSynSetPair2Sim phraSynSetList phraSynPair2SimMap = phraSynSetPair2SimList
    where
    phraSynSetPair2SimList = [((x, y), getPhraSynSetSim x y phraSynPair2SimMap) | x <- phraSynSetList, y <- phraSynSetList, x <= y]

{- Get similarity degree bewteen any two PhraSyn values, where PhraSyn denotes LeftOver and RightOver.
 - Similarity degree between two PhraSyn values satisfies commutative law, sim (ps1, ps2) == sim (ps2, ps1),
 - so only sim (ps1, ps2) where ps1 <= ps2 is calculated.
 -}
getPhraSynPair2Sim :: [PhraSyn] -> Map (PhraSyn, PhraSyn) SimDeg -> [((LeftOver, LeftOver), SimDeg)]
getPhraSynPair2Sim phraSynList phraSynPair2SimMap = phraSynPair2SimList
    where
    phraSynPair2SimList = [((x, y), case (Map.lookup (x, y) phraSynPair2SimMap) of
                                       Just x -> x
                                       Nothing -> 0.0
                           ) | x <- phraSynList, y <- phraSynList, x <= y]

{- Get similarity degree between two overtype contexts, namely two vectors of (LeftExtend, LeftOver, RightOver, RightExtend).
 -}
lookupContextOfOTPairSim :: [((ContextOfOT, ContextOfOT), SimDeg)] -> (ContextOfOT, ContextOfOT) -> ((ContextOfOT, ContextOfOT), SimDeg)
lookupContextOfOTPairSim [] (cot1, cot2) = ((cot1, cot2), 0.0)
lookupContextOfOTPairSim (((c1, c2), simDeg):cpsl) (cot1, cot2)
    | ((cot1, cot2) == (c1, c2)) || ((cot2, cot1) == (c1, c2)) = ((cot1, cot2), simDeg)
    | otherwise = lookupContextOfOTPairSim cpsl (cot1, cot2)

{- Get similarity degrees bewteen any two overlapping types.
 - startIdx: Index of start index of ambiguity model samples.
 - endIdx: Index of end index of ambiguity model samples.
 - overtype: 1 .. 5
 -}
getOverTypePair2Sim :: SIdx -> SIdx -> IO [((OverType, OverType), SimDeg)]
getOverTypePair2Sim startIdx endIdx = do
    context2OverTypeBase <- getContext2OverTypeBase startIdx endIdx             -- [(ContextOfOT, OverType)]
    let contexts4OT1 = [fst x | x <- context2OverTypeBase, snd x == 1]          -- [ContextOfOT]
    let contexts4OT2 = [fst x | x <- context2OverTypeBase, snd x == 2]          -- [ContextOfOT]
    let contexts4OT3 = [fst x | x <- context2OverTypeBase, snd x == 3]          -- [ContextOfOT]
    let contexts4OT4 = [fst x | x <- context2OverTypeBase, snd x == 4]          -- [ContextOfOT]
    let contexts4OT5 = [fst x | x <- context2OverTypeBase, snd x == 5]          -- [ContextOfOT]
    let overType2ContextList = [(1,contexts4OT1),(2,contexts4OT2),(3,contexts4OT3),(4,contexts4OT4),(5,contexts4OT5)]  -- [(OverType, [ContextOfOT])]
    putStrLn $ "getOverTypePair2Sim: Num. of samples per overType: " ++ show (map (\x -> (fst x, length (snd x))) overType2ContextList)
    contextOfOTPairSimTuple <- getContextOfOTPairSim context2OverTypeBase
    let contextOfOTPairSimList = snd contextOfOTPairSimTuple                    -- [((ContextOfOT, ContextOfOT), OverType)]
    let oos = [((fst o1, fst o2), getContextOfOTSetSim (snd o1) (snd o2) contextOfOTPairSimList)
                  | o1 <- overType2ContextList, o2 <- overType2ContextList, fst o1 <= fst o2]
    return oos

{- Get similarity degree between two ContextOfOT sets in their grammatical features.
 - (1) Suppose set A and B are [ContextOfOT]. Get upper triangular matrix of similarity degrees, where sim(a,b) is similarity of (a,b).
 -     M = [((a,b), sim(a,b)) | (a,b) in A X B, a <= b]
 - (2) Match elements between A and B according to maximum similarity degree, forming list L.
 - (3) If |A| < |B|. For every element b in B, which does not appear in L, append ((nullContextOfOT, b), 0) to L.
 - (4) If |A| > |B|. For every element a in A, which does not appear in L, append ((a, nullContextOfOT), 0) to L.
 - Finally, return mean value of similarity degrees of all matched PhraSyn value pairs.
 -}
getContextOfOTSetSim :: [ContextOfOT] -> [ContextOfOT] -> [((ContextOfOT, ContextOfOT), SimDeg)] -> SimDeg
getContextOfOTSetSim cotl1 cotl2 contextOfOTPair2SimList = simDeg
    where
--    contextOfOTPairMatrix = nubBy (\x y -> x == swap y) [(a,b) | a <- cotl1, b <- cotl2]         -- Matrix element must be number value type. Name matrix is borrowed.
--    contextOfOTPair2SimMatrix = map (lookupContextOfOTPairSim contextOfOTPair2SimList) contextOfOTPairMatrix      -- [((ContextOfOT, ContextOfOT), SimDeg)]
    contextOfOTPairMatrix = [(a,b) | a <- zip [1..] cotl1, b <- zip [1..] cotl2]      -- [((RowIdx, ContextOfOT), (ColIdx, ContextOfOT))], a tuple matrix.
    contextOfOTPair2SimMatrix = map (\x -> (((fst . fst) x, (fst . snd) x), (lookupContextOfOTPairSim contextOfOTPair2SimList) ((snd . fst) x, (snd . snd) x))) contextOfOTPairMatrix
                                                                                    -- [((RowIdx, ColIdx), ((ContextOfOT, ContextOfOT), SimDeg))]
    matchedContextOfOTPair2SimList = map snd $ getMatchedElemPair2SimList [] contextOfOTPair2SimMatrix              -- [((ContextOfOT, ContextOfOT), SimDeg)]
    allMatchedContextOfOTPair2SimList = case (signum (length cotl1 - (length cotl2))) of
      -1 -> matchedContextOfOTPair2SimList ++ [((nullContextOfOT, x), 0.0) | x <- cotl2, notElem x (map (snd . fst) matchedContextOfOTPair2SimList)]
      0 -> matchedContextOfOTPair2SimList
      1 -> matchedContextOfOTPair2SimList ++ [((x, nullContextOfOT), 0.0) | x <- cotl1, notElem x (map (fst . fst) matchedContextOfOTPair2SimList)]
    simDeg = foldl (+) 0.0 (map snd allMatchedContextOfOTPair2SimList) / (fromIntegral (length allMatchedContextOfOTPair2SimList))

-- Types for calculating similarity degree of syntactic ambiguity contexts.
type NumOfContextOfSG = Int       -- ContextOfSG :: (LeftExtend, LeftOver, RightOver, RightExtend, OverType)
type NumOfContextOfSGPair = Int
type ContextOfSGPair2Sim = ((ContextOfSG, ContextOfSG), SimDeg)

{- Reading Prior and its context from the sample database of a certain syntax_ambig_resol_model, such as 'stru_gene_202408'.
 - If start index and end index are both 0, then all records in sample database will be read.
 - Context2ClauTagPrior :: (ContextOfSG, Prior)
 - Context2ClauTagPriorBase :: [Context2ClauTagPrior]
 - ContextOfSG :: (LeftExtend, LeftOver, RightOver, RightExtend, OverType)
 -}
getContext2ClauTagPriorBase :: SIdx -> SIdx -> IO [Context2ClauTagPrior]
getContext2ClauTagPriorBase startIdx endIdx = do
    conn <- getConn
    confInfo <- readFile "Configuration"                                        -- Read the local configuration file
    let syntax_ambig_resol_model = getConfProperty "syntax_ambig_resol_model" confInfo
    case syntax_ambig_resol_model of
      x | elem x ["stru_gene_202408", "stru_gene_202412", "stru_gene_202501"] -> do
          let startIdx' = case startIdx of
                            0 -> 1
                            _ -> startIdx
          let sqlstat = DS.fromString $ "select count(*) from " ++ syntax_ambig_resol_model
          (defs, is) <- query_ conn sqlstat
          rows <- readStreamByInt64 [] is
          let endIdx' = case endIdx of
                          0 -> head rows
                          _ -> endIdx

          putStrLn $ "stardIdx = " ++ show startIdx' ++ " , endIdx = " ++ show endIdx'

          putStrLn $ "The source of priors and their contexts is set as: " ++ syntax_ambig_resol_model     -- Display the source of priors and their contexts
          let sqlstat = DS.fromString $ "select leftExtend, leftOver, rightOver, rightExtend, overType, clauTagPrior from " ++ syntax_ambig_resol_model ++ " where id >= ? and id <= ?"
          stmt <- prepareStmt conn sqlstat
          (defs, is) <- queryStmt conn stmt [toMySQLInt32U startIdx', toMySQLInt32U endIdx']

          context2ClauTagPriorBase <- readStreamByContext2ClauTagPrior [] is                  -- [Context2ClauTagPrior]
          let context2ClauTagPriorNum = length context2ClauTagPriorBase                       -- The number of Context2OverType samples.
          putStrLn $ "getContext2ClauTagPriorBase: context2ClauTagPriorNum = " ++ show context2ClauTagPriorNum
          return context2ClauTagPriorBase
      _ -> do
        putStrLn "getContext2ClauTagPriorBase: Value of property 'syntax_ambig_resol_model' does not match any MySQL table."
        return []

{- Reading SIdx and ContextOfSG from the sample database of a certain syntax_ambig_resol_model, such as 'stru_gene_202408'.
 - If start index and end index are both 0, then all records in sample database will be read.
 - ContextOfSG :: (LeftExtend, LeftOver, RightOver, RightExtend, OverType)
 -}
getSIdx2ContextOfSGBase :: SIdx -> SIdx -> IO [(SIdx, ContextOfSG)]
getSIdx2ContextOfSGBase startIdx endIdx = do
    conn <- getConn
    confInfo <- readFile "Configuration"                                        -- Read the local configuration file
    let syntax_ambig_resol_model = getConfProperty "syntax_ambig_resol_model" confInfo
    case syntax_ambig_resol_model of
      x | elem x ["stru_gene_202408", "stru_gene_202412", "stru_gene_202501"] -> do
          let startIdx' = case startIdx of
                            0 -> 1
                            _ -> startIdx
          let sqlstat = DS.fromString $ "SELECT COUNT(*) FROM " ++ syntax_ambig_resol_model
          (defs, is) <- query_ conn sqlstat
          rows <- readStreamByInt64 [] is
          let endIdx' = case endIdx of
                          0 -> head rows
                          _ -> endIdx

          putStrLn $ "stardIdx = " ++ show startIdx' ++ " , endIdx = " ++ show endIdx'

          putStrLn $ "The source of SIdx and ContextOfSG is set as: " ++ syntax_ambig_resol_model     -- Display the source of SIdx and ContextOfSG
          let sqlstat = DS.fromString $ "SELECT id, leftExtend, leftOver, rightOver, rightExtend, overType FROM " ++ syntax_ambig_resol_model ++ " where id >= ? and id <= ?"
          stmt <- prepareStmt conn sqlstat
          (defs, is) <- queryStmt conn stmt [toMySQLInt32U startIdx', toMySQLInt32U endIdx']

          sIdx2ContextOfSGListFromDB <- readStreamByInt32UTextTextTextTextInt8 [] is          -- [(Int, (String, String, String, String, Int))]
          let sIdx2ContextOfSGList = map (\(sIdx, (leStr, loStr, roStr, reStr, ot))
                                          -> (sIdx, (readPhraSynListFromStr leStr
                                                  , readPhraSynFromStr loStr
                                                  , readPhraSynFromStr roStr
                                                  , readPhraSynListFromStr reStr
                                                  , ot))) sIdx2ContextOfSGListFromDB          -- [(SIdx, ContextOfSG)]
          let contextOfSGSampleNum = length sIdx2ContextOfSGList                              -- The number of ContextOfSG samples.
          putStrLn $ "getSIdx2ContextOfSGBase: Num. of ContextOfSG Samples = " ++ show contextOfSGSampleNum
          return sIdx2ContextOfSGList
      _ -> do
        putStrLn "getSIdx2ContextOfSGBase: Value of property 'syntax_ambig_resol_model' does not match any MySQL table."
        return []

{- Get similarity degree between two prior contexts, namely two vectors of (LeftExtend, LeftOver, RightOver, RightExtend, OverType).
 - sim(contextOfSG1, contextOfSG2) = f(leSim, loSim, roSim, reSim, otSim), where
 -     leSim is similarity degree between two LeftExtends, namely two PhraSyn sets.
 -     loSim is similarity degree between two LeftOvers, namely two PhraSyns.
 -     roSim is similarity degree between two RightOvers, namely two PhraSyns.
 -     reSim is similarity degree between two RightExtends, namely two PhraSyn sets.
 -     otSim is similarity degree between two OverTypes, namely two values among [1..5].
 - If LeftExtend, LeftOver, RightOver, RightExtend and OverType are independent with each other,
 - namely columns of matrix [[leSim, loSim, roSim, reSim, otSim]] are orthogonal columns.
 - Function f shoule be Root Mean Square of (leSim, loSim, roSim, reSim, otSim). Otherwise, f should be arithmetic mean.
 -
 - contextOfSGPairList: List of (ContextOfSG, ContextOfSG), in every element of which first ContextOfSG value is less than or equal to the second.
 - origSimList: List of (leSim, loSim, roSim, reSim, otSim), in which every quintuple (leSim, loSim, roSim, reSim, otSim) is corresponding to one tuple (contextOfSG1, contextOfSG2) in contextOfSGPairList.
 -}
getContextOfSGPairSim :: Context2ClauTagPriorBase -> IO ([(SimDeg, SimDeg, SimDeg, SimDeg, SimDeg)], [((ContextOfSG, ContextOfSG), SimDeg)])
getContextOfSGPairSim context2ClauTagPriorBase = do
    let contextOfSGList = nub $ map fst context2ClauTagPriorBase                -- [ContextOfSG], here there is no repetitive ContextOfSG values.
    putStrLn $ "Num. of repetitive ContextOfSG values = " ++ show (length context2ClauTagPriorBase - (length contextOfSGList))

    confInfo <- readFile "Configuration"
    let phra_gram_dist_algo = getConfProperty "phra_gram_dist_algo" confInfo    -- Distance algorithm for phrasal grammar aspects
    let strugene_context_dist_algo = getConfProperty "strugene_context_dist_algo" confInfo    -- Distance algorithm for StruGene contexts

    let contextOfOTList = nub $ map (\x -> (fst5 x, snd5 x, thd5 x, fth5 x)) contextOfSGList    -- [ContextOfOT]
    phraSynPair2SimMap <- getPhraSynPair2SimFromCOT phra_gram_dist_algo contextOfOTList      -- Map (PhraSyn, PhraSyn) SimDeg

    let leftExtendList = map fst5 contextOfSGList                               -- [LeftExtend]
    let lePair2SimList = getPhraSynSetPair2Sim leftExtendList phraSynPair2SimMap       -- [((LeftExtend, LeftExtend), SimDeg)]
    let leftOverList = map snd5 contextOfSGList                                 -- [LeftOver]
    let loPair2SimList = getPhraSynPair2Sim leftOverList phraSynPair2SimMap            -- [((LeftOver, LeftOver), SimDeg)]
    let rightOverList = map thd5 contextOfSGList                                -- [RightOver]
    let roPair2SimList = getPhraSynPair2Sim rightOverList phraSynPair2SimMap           -- [((RightOver, RightOver), SimDeg)]
    let rightExtendList = map fth5 contextOfSGList                              -- [RightExtend]
    let rePair2SimList = getPhraSynSetPair2Sim rightExtendList phraSynPair2SimMap      -- [((RightExtend, RightExtend), SimDeg)]

    let context2OverTypeBase = map ((\x -> ((fst5 x, snd5 x, thd5 x, fth5 x), fif5 x)) . fst) context2ClauTagPriorBase   -- [(ContextOfOT, OverType)]
    let contexts4OT1 = [fst x | x <- context2OverTypeBase, snd x == 1]          -- [ContextOfOT]
    let contexts4OT2 = [fst x | x <- context2OverTypeBase, snd x == 2]          -- [ContextOfOT]
    let contexts4OT3 = [fst x | x <- context2OverTypeBase, snd x == 3]          -- [ContextOfOT]
    let contexts4OT4 = [fst x | x <- context2OverTypeBase, snd x == 4]          -- [ContextOfOT]
    let contexts4OT5 = [fst x | x <- context2OverTypeBase, snd x == 5]          -- [ContextOfOT]
    let overType2ContextList = [(1,contexts4OT1),(2,contexts4OT2),(3,contexts4OT3),(4,contexts4OT4),(5,contexts4OT5)]  -- [(OverType, [ContextOfOT])]
    putStrLn $ "getContextOfSGPairSim: Num. of samples per overType: " ++ show (map (\x -> (fst x, length (snd x))) overType2ContextList)
    contextOfOTPairSimTuple <- getContextOfOTPairSim context2OverTypeBase
    let contextOfOTPairSimList = snd contextOfOTPairSimTuple                    -- [((ContextOfOT, ContextOfOT), OverType)]
    let otPair2SimList = [((fst o1, fst o2), getContextOfOTSetSim (snd o1) (snd o2) contextOfOTPairSimList)
                          | o1 <- overType2ContextList, o2 <- overType2ContextList, fst o1 <= fst o2]      -- [((OverType, OverType), SimDeg)]
    putStrLn $ "otPair2SimList: " ++ show otPair2SimList
    let contextOfSGPairList = [(x, y) | x <- contextOfSGList, y <- contextOfSGList, x <= y]   -- [(ContextOfSG, ContextOfSG)]
                                                      -- Ord definition is exhaustive, here there is no repetitive pairs.
    let numOfContextOfSGPair = length contextOfSGPairList
    let origSimList = [(getSimDegFromAttPair2Sim (fst5 x) (fst5 y) lePair2SimList
                      , getSimDegFromAttPair2Sim (snd5 x) (snd5 y) loPair2SimList
                      , getSimDegFromAttPair2Sim (thd5 x) (thd5 y) roPair2SimList
                      , getSimDegFromAttPair2Sim (fth5 x) (fth5 y) rePair2SimList
                      , getSimDegFromAttPair2Sim (fif5 x) (fif5 y) otPair2SimList
                       ) | (x, y) <- contextOfSGPairList]


    let simList = case strugene_context_dist_algo of
                    "Euclidean" -> [sqrt (sum [les * les, los * los, ros * ros, res * res, ots * ots] / 5.0) | (les,los,ros,res,ots) <- origSimList]
                    "Manhattan" -> [sum [les, los, ros, res, ots] / 5.0 | (les,los,ros,res,ots) <- origSimList]
    let contextOfSGPair2SimList = zip contextOfSGPairList simList     -- [((ContextOfSG, ContextOfSG), SimDeg)]
    return (origSimList, contextOfSGPair2SimList)

{- Get similarity degree between two prior contexts, namely two vectors of (LeftExtend, LeftOver, RightOver, RightExtend, OverType).
 - sim(contextOfSG1, contextOfSG2) = f(leSim, loSim, roSim, reSim, otSim), where
 -     leSim is similarity degree between two LeftExtends, namely two PhraSyn sets.
 -     loSim is similarity degree between two LeftOvers, namely two PhraSyns.
 -     roSim is similarity degree between two RightOvers, namely two PhraSyns.
 -     reSim is similarity degree between two RightExtends, namely two PhraSyn sets.
 -     otSim is similarity degree between two OverTypes, namely two values among [1..5].
 - LeftExtend, LeftOver, RightOver, RightExtend and OverType are not independent with each other, so matrix [[leSim, loSim, roSim, reSim, otSim]] is converted into
 - matrix [[leSim', loSim', roSim', reSim', otSim']] acoording to SVD (Singular Value Decomposition) such that leSim', loSim', roSim', reSim' and otSim' are orthogonal columns.
 - Thus, sim(contextOfSG1, contextOfSG2) = f(leSim, loSim, roSim, reSim, otSim) = f'(leSim', loSim', roSim', reSim', otSim'), where f' is Euclid length of vector (leSim', loSim', roSim', reSim').
 - Normalizedly, f' is Root Mean Square of (leSim', loSim', roSim', reSim', otSim')
 -
 - contextOfSGPairList: List of (ContextOfSG, ContextOfSG), in every element of which first ContextOfSG value is less than or equal to the second.
 - origSimList: List of (leSim, loSim, roSim, reSim, otSim), in which every quintuple (leSim, loSim, roSim, reSim, otSim) is corresponding to one tuple (contextOfSG1, contextOfSG2) in contextOfSGPairList.
 - origSimMatrix:  (k >< 4) [leSim1, loSim1, roSim1, reSim1, otSim1, ..., leSimk, loSimk, roSimk, reSimk, otSimk], which is matrix form of origSimList.
 - orthSimMatrix: origSimMatrix LA.<> v, where v is the right singular matrix.
 -}
getContextOfSGPairSimBySVD :: Context2ClauTagPriorBase -> IO ([(ContextOfSG, ContextOfSG)], Matrix Double, Matrix Double, [((ContextOfSG, ContextOfSG), SimDeg)])
getContextOfSGPairSimBySVD context2ClauTagPriorBase = do
    let contextOfSGList = nub $ map fst context2ClauTagPriorBase                -- [ContextOfSG], here there is no repetitive ContextOfSG values.
    putStrLn $ "Num. of repetitive ContextOfSG values = " ++ show (length context2ClauTagPriorBase - (length contextOfSGList))

    confInfo <- readFile "Configuration"
    let phra_gram_dist_algo = getConfProperty "phra_gram_dist_algo" confInfo    -- Distance algorithm for phrasal grammar aspects

    let contextOfOTList = nub $ map (\x -> (fst5 x, snd5 x, thd5 x, fth5 x)) contextOfSGList    -- [ContextOfOT]
    phraSynPair2SimMap <- getPhraSynPair2SimFromCOT phra_gram_dist_algo contextOfOTList      -- Map (PhraSyn, PhraSyn) SimDeg
--    putStrLn $ "phraSynPair2SimMap: " ++ show phraSynPair2SimMap

    let leftExtendList = map fst5 contextOfSGList                               -- [LeftExtend]
    let lePair2SimList = getPhraSynSetPair2Sim leftExtendList phraSynPair2SimMap       -- [((LeftExtend, LeftExtend), SimDeg)]
    let leftOverList = map snd5 contextOfSGList                                 -- [LeftOver]
    let loPair2SimList = getPhraSynPair2Sim leftOverList phraSynPair2SimMap            -- [((LeftOver, LeftOver), SimDeg)]
    let rightOverList = map thd5 contextOfSGList                                -- [RightOver]
    let roPair2SimList = getPhraSynPair2Sim rightOverList phraSynPair2SimMap           -- [((RightOver, RightOver), SimDeg)]
    let rightExtendList = map fth5 contextOfSGList                              -- [RightExtend]
    let rePair2SimList = getPhraSynSetPair2Sim rightExtendList phraSynPair2SimMap      -- [((RightExtend, RightExtend), SimDeg)]

    let context2OverTypeBase = map ((\x -> ((fst5 x, snd5 x, thd5 x, fth5 x), fif5 x)) . fst) context2ClauTagPriorBase   -- [(ContextOfOT, OverType)]
    let contexts4OT1 = [fst x | x <- context2OverTypeBase, snd x == 1]          -- [ContextOfOT]
    let contexts4OT2 = [fst x | x <- context2OverTypeBase, snd x == 2]          -- [ContextOfOT]
    let contexts4OT3 = [fst x | x <- context2OverTypeBase, snd x == 3]          -- [ContextOfOT]
    let contexts4OT4 = [fst x | x <- context2OverTypeBase, snd x == 4]          -- [ContextOfOT]
    let contexts4OT5 = [fst x | x <- context2OverTypeBase, snd x == 5]          -- [ContextOfOT]
    let overType2ContextList = [(1,contexts4OT1),(2,contexts4OT2),(3,contexts4OT3),(4,contexts4OT4),(5,contexts4OT5)]  -- [(OverType, [ContextOfOT])]
    putStrLn $ "getContextOfSGPairSimBySVD: Num. of samples per overType: " ++ show (map (\x -> (fst x, length (snd x))) overType2ContextList)
    contextOfOTPairSimTuple <- getContextOfOTPairSim context2OverTypeBase
    let contextOfOTPairSimList = snd contextOfOTPairSimTuple                    -- [((ContextOfOT, ContextOfOT), OverType)]
    let otPair2SimList = [((fst o1, fst o2), getContextOfOTSetSim (snd o1) (snd o2) contextOfOTPairSimList)
                          | o1 <- overType2ContextList, o2 <- overType2ContextList, fst o1 <= fst o2]      -- [((OverType, OverType), SimDeg)]
    putStrLn $ "otPair2SimList: " ++ show otPair2SimList
    let contextOfSGPairList = [(x, y) | x <- contextOfSGList, y <- contextOfSGList, x <= y]   -- [(ContextOfSG, ContextOfSG)]
                                                      -- Ord definition is exhaustive, here there is no repetitive pairs.
    let numOfContextOfSGPair = length contextOfSGPairList
    let origSimList = [(getSimDegFromAttPair2Sim (fst5 x) (fst5 y) lePair2SimList
                      , getSimDegFromAttPair2Sim (snd5 x) (snd5 y) loPair2SimList
                      , getSimDegFromAttPair2Sim (thd5 x) (thd5 y) roPair2SimList
                      , getSimDegFromAttPair2Sim (fth5 x) (fth5 y) rePair2SimList
                      , getSimDegFromAttPair2Sim (fif5 x) (fif5 y) otPair2SimList
                       ) | (x, y) <- contextOfSGPairList]
    let origSimMatrix = (numOfContextOfSGPair >< 5) $ concat [[fst5 e, snd5 e, thd5 e, fth5 e, fif5 e] | e <- origSimList]       -- hmatrix

    let (u, s, v) = svd origSimMatrix
    let orthSimMatrix = origSimMatrix LA.<> v

-- Normalization: Root Mean Square of every row vector (leSim', loSim', roSim', reSim') in matrix orthSimMatrix.
    let contextOfSGPairSimList = map (sqrt . (/ 5.0) . sumElements) (toRows (cmap (\x -> x*x) orthSimMatrix))
    let contextOfSGPair2SimList = zip contextOfSGPairList contextOfSGPairSimList     -- [((ContextOfSG, ContextOfSG), SimDeg)]

    return (contextOfSGPairList, origSimMatrix, orthSimMatrix, contextOfSGPair2SimList)

{- Get similarity degree between two ContextOfSG samples, namely two vectors of (LeftExtend, LeftOver, RightOver, RightExtend, OverType).
 - sim(contextOfSG1, contextOfSG2) = f(leSim, loSim, roSim, reSim, otSim), where
 -     leSim is similarity degree between two LeftExtends, namely two PhraSyn sets.
 -     loSim is similarity degree between two LeftOvers, namely two PhraSyns.
 -     roSim is similarity degree between two RightOvers, namely two PhraSyns.
 -     reSim is similarity degree between two RightExtends, namely two PhraSyn sets.
 -     otSim is similarity degree between two OverTypes, namely two values among [1..5].
 - Where f can be Root Mean Square of (leSim, loSim, roSim, reSim, otSim) or their arithmetic mean.
 - Similarity degrees between any pair of PhraSyn values are required for calculating similarity between two given ContextOfSG samples.
 -}
getOneContextOfSGPairSim :: ContextOfSG -> ContextOfSG -> Map (PhraSyn, PhraSyn) SimDeg -> IO SimDeg
getOneContextOfSGPairSim csg1 csg2 phraSynPair2SimMap = do
    confInfo <- readFile "Configuration"
    let strugene_context_dist_algo = getConfProperty "strugene_context_dist_algo" confInfo
        overlap_type_dist_algo = getConfProperty "overlap_type_dist_algo" confInfo

        otPair2SimList = case overlap_type_dist_algo of
                           "Euclidean" -> [((1,1),1.0),((1,2),7.3351553900482406e-3),((1,3),4.8666647227816856e-2),((1,4),3.3917793725912755e-2),((1,5),0.4247127394531829)
                                          ,((2,2),1.0),((2,3),0.11777246281776142),((2,4),0.1761503383674159),((2,5),1.0017916991325765e-2)
                                          ,((3,3),1.0),((3,4),0.5887136000291061),((3,5),7.134132733982824e-2)
                                          ,((4,4),1.0),((4,5),5.1362972267575656e-2)
                                          ,((5,5),1.0)]
                           "Manhattan" -> [((1,1),1.0),((1,2),4.470173439533701e-2),((1,3),0.20418481026327456),((1,4),0.16004358484296605),((1,5),0.12951425063877822)
                                          ,((2,2),1.0),((2,3),0.14760105015849836),((2,4),0.21749622640726418),((2,5),9.390008726364139e-3)
                                          ,((3,3),1.0),((3,4),0.5220146998431232),((3,5),5.141417139299777e-2)
                                          ,((4,4),1.0),((4,5),4.3757227872407406e-2)
                                          ,((5,5),1.0)]

    putStrLn $ "getOneContextOfSGPairSim: (csg1, csg2): " ++ show (csg1, csg2)
    let les = getPhraSynSetSim (fst5 csg1) (fst5 csg2) phraSynPair2SimMap           -- LeftExtend SimDeg
        los = case (Map.lookup (snd5 csg1, snd5 csg2) phraSynPair2SimMap) of        -- LeftOver SimDeg
                Just x -> x
                Nothing -> case (Map.lookup (snd5 csg2, snd5 csg1) phraSynPair2SimMap) of
                             Just y -> y
                             Nothing -> error $ "getOneContextOfSGPairSim: Failed to lookup (" ++ (show . snd5) csg1 ++ ", " ++ (show . snd5) csg2 ++ ")."
        ros = case (Map.lookup (thd5 csg1, thd5 csg2) phraSynPair2SimMap) of        -- RightOver SimDeg
                Just x -> x
                Nothing -> case (Map.lookup (thd5 csg2, thd5 csg1) phraSynPair2SimMap) of
                             Just y -> y
                             Nothing -> error $ "getOneContextOfSGPairSim: Failed to lookup (" ++ (show . thd5) csg1 ++ ", " ++ (show . thd5) csg2 ++ ")."
        res = getPhraSynSetSim (fth5 csg1) (fth5 csg2) phraSynPair2SimMap           -- RightExtend SimDeg
        ots = getSimDegFromAttPair2Sim (fif5 csg1) (fif5 csg2) otPair2SimList       -- OverType SimDeg

        sim = case strugene_context_dist_algo of
                "Euclidean" -> sqrt (sum [les * les, los * los, ros * ros, res * res, ots * ots] / 5.0)
                "Manhattan" -> sum [les, los, ros, res, ots] / 5.0
    putStrLn $ "getOneContextOfSGPairSim: sim(leSim, loSim, roSim, reSim, otSim) = sim(" ++ printf "%.04f" les ++ ", "
                                                                                         ++ printf "%.04f" los ++ ", "
                                                                                         ++ printf "%.04f" ros ++ ", "
                                                                                         ++ printf "%.04f" res ++ ", "
                                                                                         ++ printf "%.04f" ots ++ ") = "
                                                                                         ++ printf "%.04f" sim
    return sim

{- Get similarity degree between two prior contexts, namely two vectors of (LeftExtend, LeftOver, RightOver, RightExtend, OverType).
 - This is particular version for 'getContextOfSGPairSim', in which similarity degrees on grammtic attributes are precalculated and stored into global cache.
 - sim(contextOfSG1, contextOfSG2) = f(leSim, loSim, roSim, reSim, otSim), where
 -     leSim is similarity degree between two LeftExtends, namely two PhraSyn sets.
 -     loSim is similarity degree between two LeftOvers, namely two PhraSyns.
 -     roSim is similarity degree between two RightOvers, namely two PhraSyns.
 -     reSim is similarity degree between two RightExtends, namely two PhraSyn sets.
 -     otSim is similarity degree between two OverTypes, namely two values among [1..5].
 - Suppose LeftExtend, LeftOver, RightOver, RightExtend and OverType are independent with each other,
 - namely columns of matrix [[leSim, loSim, roSim, reSim, otSim]] are orthogonal columns.
 - Thus, sim(contextOfSG1, contextOfSG2) = f(leSim, loSim, roSim, reSim, otSim), where f is Root Mean Square of (leSim, loSim, roSim, reSim, otSim)
 -
 - contextOfSGPairList: List of (ContextOfSG, ContextOfSG), in every element of which first ContextOfSG value is less than or equal to the second.
 - origSimList: List of (leSim, loSim, roSim, reSim, otSim), in which every quintuple (leSim, loSim, roSim, reSim, otSim) is corresponding to one tuple (contextOfSG1, contextOfSG2) in contextOfSGPairList.
 -}
getContextOfSGPairSimWithStaticOT :: Context2ClauTagPriorBase -> IO ([(SimDeg, SimDeg, SimDeg, SimDeg, SimDeg)], [((ContextOfSG, ContextOfSG), SimDeg)])
getContextOfSGPairSimWithStaticOT context2ClauTagPriorBase = do
    let contextOfSGList = nub $ map fst context2ClauTagPriorBase                -- [ContextOfSG], here there is no repetitive ContextOfSG values.
    putStrLn $ "Num. of repetitive ContextOfSG values = " ++ show (length context2ClauTagPriorBase - (length contextOfSGList))

    confInfo <- readFile "Configuration"
    let phra_gram_dist_algo = getConfProperty "phra_gram_dist_algo" confInfo    -- Distance algorithm for phrasal grammar aspects
    let strugene_context_dist_algo = getConfProperty "strugene_context_dist_algo" confInfo    -- Distance algorithm for StruGene contexts
    let overlap_type_dist_algo = getConfProperty "overlap_type_dist_algo" confInfo            -- Distance algorithm for OverType values

    let contextOfOTList = nub $ map (\x -> (fst5 x, snd5 x, thd5 x, fth5 x)) contextOfSGList    -- [ContextOfOT]
    phraSynPair2SimMap <- getPhraSynPair2SimFromCOT phra_gram_dist_algo contextOfOTList      -- Map (PhraSyn, PhraSyn) SimDeg

    let leftExtendList = map fst5 contextOfSGList                               -- [LeftExtend]
    let lePair2SimList = getPhraSynSetPair2Sim leftExtendList phraSynPair2SimMap       -- [((LeftExtend, LeftExtend), SimDeg)]
    let leftOverList = map snd5 contextOfSGList                                 -- [LeftOver]
    let loPair2SimList = getPhraSynPair2Sim leftOverList phraSynPair2SimMap            -- [((LeftOver, LeftOver), SimDeg)]
    let rightOverList = map thd5 contextOfSGList                                -- [RightOver]
    let roPair2SimList = getPhraSynPair2Sim rightOverList phraSynPair2SimMap           -- [((RightOver, RightOver), SimDeg)]
    let rightExtendList = map fth5 contextOfSGList                              -- [RightExtend]
    let rePair2SimList = getPhraSynSetPair2Sim rightExtendList phraSynPair2SimMap      -- [((RightExtend, RightExtend), SimDeg)]

    let otPair2SimList = case overlap_type_dist_algo of
                           "Euclidean" -> [((1,1),1.0),((1,2),7.3351553900482406e-3),((1,3),4.8666647227816856e-2),((1,4),3.3917793725912755e-2),((1,5),0.4247127394531829)
                                          ,((2,2),1.0),((2,3),0.11777246281776142),((2,4),0.1761503383674159),((2,5),1.0017916991325765e-2)
                                          ,((3,3),1.0),((3,4),0.5887136000291061),((3,5),7.134132733982824e-2)
                                          ,((4,4),1.0),((4,5),5.1362972267575656e-2)
                                          ,((5,5),1.0)]
                           "Manhattan" -> [((1,1),1.0),((1,2),4.470173439533701e-2),((1,3),0.20418481026327456),((1,4),0.16004358484296605),((1,5),0.12951425063877822)
                                          ,((2,2),1.0),((2,3),0.14760105015849836),((2,4),0.21749622640726418),((2,5),9.390008726364139e-3)
                                          ,((3,3),1.0),((3,4),0.5220146998431232),((3,5),5.141417139299777e-2)
                                          ,((4,4),1.0),((4,5),4.3757227872407406e-2)
                                          ,((5,5),1.0)]

    let contextOfSGPairList = [(x, y) | x <- contextOfSGList, y <- contextOfSGList, x <= y]   -- [(ContextOfSG, ContextOfSG)]
                                                      -- Ord definition is exhaustive, here there is no repetitive pairs.
    let numOfContextOfSGPair = length contextOfSGPairList
    let origSimList = [(getSimDegFromAttPair2Sim (fst5 x) (fst5 y) lePair2SimList
                      , getSimDegFromAttPair2Sim (snd5 x) (snd5 y) loPair2SimList
                      , getSimDegFromAttPair2Sim (thd5 x) (thd5 y) roPair2SimList
                      , getSimDegFromAttPair2Sim (fth5 x) (fth5 y) rePair2SimList
                      , getSimDegFromAttPair2Sim (fif5 x) (fif5 y) otPair2SimList
                       ) | (x, y) <- contextOfSGPairList]


    let simList = case strugene_context_dist_algo of
                    "Euclidean" -> [sqrt (sum [les * les, los * los, ros * ros, res * res, ots * ots] / 5.0) | (les,los,ros,res,ots) <- origSimList]
                    "Manhattan" -> [sum [les, los, ros, res, ots] / 5.0 | (les,los,ros,res,ots) <- origSimList]
    let contextOfSGPair2SimList = zip contextOfSGPairList simList     -- [((ContextOfSG, ContextOfSG), SimDeg)]
    return (origSimList, contextOfSGPair2SimList)

{- Get similarity degrees between one ClauTagPrior context and ALL ClauTagPrior contexts.
 - contextOfSG: A ClauTagPrior context of a certain StruGene sample.
 - context2ClauTagPriorBase: The whole samples of mapping from ContextOfSG to ClauTagPrior, INCLUDING the sample of contextOfSG to its ClauTagPrior.
 - otPair2SimList: Similarity degrees between overlapping types calculated by first 500 StruGene samples.
 - origSimList: List of (leSim, loSim, roSim, reSim, otSim), in which every quintuple is corresponding to one tuple (contextOfSG1, contextOfSG2) in contextOfSGPairList.
 -}
getOneToAllContextOfSGSim :: ContextOfSG -> Context2ClauTagPriorBase -> IO ([(SimDeg, SimDeg, SimDeg, SimDeg, SimDeg)], [((ContextOfSG, ContextOfSG), SimDeg)])
getOneToAllContextOfSGSim contextOfSG context2ClauTagPriorBase = do
    let contextOfSGList = nub $ map fst context2ClauTagPriorBase                -- [ContextOfSG], here there is no repetitive ContextOfSG values.
    let (les, los, ros, res, _) = unzip5 contextOfSGList                        -- Extract [LeftExtend], [LeftOver], [RightOver] and [RightExtend].
    let leps = [(fst5 contextOfSG, y) | y <- les]                               -- [(LeftExtend, LeftExtend)]
    let lops = [(snd5 contextOfSG, y) | y <- los]                               -- [(LeftOver, LeftOver)]
    let rops = [(thd5 contextOfSG, y) | y <- ros]                               -- [(RightOver, RightOVer)]
    let reps = [(fth5 contextOfSG, y) | y <- res]                               -- [(RightExtend, RightExtend)]

    let lePhraSynPairs = Set.fromList $ [(u, v) | lep <- leps, u <- fst lep, v <- snd lep]   -- Set (PhraSyn, PhraSyn)
    let loPhraSynPairs = Set.fromList lops
    let roPhraSynPairs = Set.fromList rops
    let rePhraSynPairs = Set.fromList $ [(u, v) | rep <- reps, u <- fst rep, v <- snd rep]   -- Set (PhraSyn, PhraSyn)

    let phraSynPairSet = Set.union (Set.union (Set.union lePhraSynPairs loPhraSynPairs) roPhraSynPairs) rePhraSynPairs
    let phraSynPairs = nub $ map (\(x,y) -> if (x<=y) then (x,y) else (y,x)) $ Set.toList phraSynPairSet         -- [(PhraSyn, PhraSyn)]
                                                                                -- PhraSyn order in every pair was rectified.
    confInfo <- readFile "Configuration"
    let phra_gram_dist_algo = getConfProperty "phra_gram_dist_algo" confInfo
    let overlap_type_dist_algo = getConfProperty "overlap_type_dist_algo" confInfo

--    putStrLn $ "phraSynPairs: " ++ show phraSynPairs
--    putStrLn $ "origSimMatrix: " ++ show (snd3 (getPhraSynPairSim phra_gram_dist_algo phraSynPairs))
--    putStrLn $ "phraSynPair2SimList: " ++ show (thd3 (getPhraSynPairSim phra_gram_dist_algo phraSynPairs))

    phraSynPairSimTuple <- getPhraSynPairSim phra_gram_dist_algo phraSynPairs
    let phraSynPair2SimMap = Map.fromList $ thd3 phraSynPairSimTuple
--    putStrLn $ "phraSynPair2SimMap: " ++ show phraSynPair2SimMap

    let leftExtendList = map fst5 contextOfSGList                               -- [LeftExtend]
    let leftExtend = fst5 contextOfSG                                           -- LeftExtend
    let lePair2SimList = [((leftExtend, y), getPhraSynSetSim leftExtend y phraSynPair2SimMap) | y <- leftExtendList]
                                                                                -- [((LeftExtend, LeftExtend), SimDeg)]
--    putStrLn $ "lePair2SimList: " ++ show lePair2SimList

    let leftOverList = map snd5 contextOfSGList                                 -- [LeftOver]
    let leftOver = snd5 contextOfSG                                             -- LeftOver
    let loPair2SimList = [((leftOver, y), case (Map.lookup (leftOver, y) phraSynPair2SimMap) of
                                            Just x -> x
                                            Nothing -> case (Map.lookup (y, leftOver) phraSynPair2SimMap) of
                                                         Just x -> x
                                                         Nothing -> 0.0
                          ) | y <- leftOverList]                           -- [((LeftOver, LeftOver), SimDeg)]
--    putStrLn $ "loPair2SimList: " ++ show loPair2SimList

    let rightOverList = map thd5 contextOfSGList                                -- [RightOver]
    let rightOver = thd5 contextOfSG                                            -- RightOver
    let roPair2SimList = [((rightOver, y), case (Map.lookup (rightOver, y) phraSynPair2SimMap) of
                                             Just x -> x
                                             Nothing -> case (Map.lookup (y, rightOver) phraSynPair2SimMap) of
                                                          Just x -> x
                                                          Nothing -> 0.0
                          ) | y <- rightOverList]                          -- [((RightOver, RightOver), SimDeg)]
--    putStrLn $ "roPair2SimList: " ++ show roPair2SimList

    let rightExtendList = map fth5 contextOfSGList                              -- [RightExtend]
    let rightExtend = fth5 contextOfSG                                          -- RightExtend
    let rePair2SimList = [((rightExtend, y), getPhraSynSetSim rightExtend y phraSynPair2SimMap) | y <- rightExtendList]
                                                                                -- [((RightExtend, RightExtend), SimDeg)]
--    putStrLn $ "rePair2SimList: " ++ show rePair2SimList

-- Euclidean OverType value similarities are calculated by using the first 500 samples of stru_gene_202408,
-- Manhattan OverType value similarities are calculated by using the first 500 samples of stru_gene_202412.
    let otPair2SimList = case overlap_type_dist_algo of
                           "Euclidean" -> [((1,1),1.0),((1,2),7.3351553900482406e-3),((1,3),4.8666647227816856e-2),((1,4),3.3917793725912755e-2),((1,5),0.4247127394531829)
                                          ,((2,2),1.0),((2,3),0.11777246281776142),((2,4),0.1761503383674159),((2,5),1.0017916991325765e-2)
                                          ,((3,3),1.0),((3,4),0.5887136000291061),((3,5),7.134132733982824e-2)
                                          ,((4,4),1.0),((4,5),5.1362972267575656e-2)
                                          ,((5,5),1.0)]
                           "Manhattan" -> [((1,1),1.0),((1,2),4.470173439533701e-2),((1,3),0.20418481026327456),((1,4),0.16004358484296605),((1,5),0.12951425063877822)
                                          ,((2,2),1.0),((2,3),0.14760105015849836),((2,4),0.21749622640726418),((2,5),9.390008726364139e-3)
                                          ,((3,3),1.0),((3,4),0.5220146998431232),((3,5),5.141417139299777e-2)
                                          ,((4,4),1.0),((4,5),4.3757227872407406e-2)
                                          ,((5,5),1.0)]
    let contextOfSGPairList = [(contextOfSG, y) | y <- contextOfSGList]         -- [(ContextOfSG, ContextOfSG)]
    let csgIdx = case (elemIndex contextOfSG contextOfSGList) of
                   Just idx -> idx
                   Nothing -> error "getOneToAllContextOfSGSim: contextOfSG index in contextOfSGList was NOT found."
    let contextOfSGIdxPairList = zip (repeat (csgIdx + 1)) [1 .. length contextOfSGList]      -- [(SIdx, SIdx)]

    let numOfContextOfSGPair = length contextOfSGPairList
    let origSimList = [(getSimDegFromAttPair2Sim (fst5 contextOfSG) (fst5 y) lePair2SimList
                      , getSimDegFromAttPair2Sim (snd5 contextOfSG) (snd5 y) loPair2SimList
                      , getSimDegFromAttPair2Sim (thd5 contextOfSG) (thd5 y) roPair2SimList
                      , getSimDegFromAttPair2Sim (fth5 contextOfSG) (fth5 y) rePair2SimList
                      , getSimDegFromAttPair2Sim (fif5 contextOfSG) (fif5 y) otPair2SimList
                       ) | y <- contextOfSGList]

    let strugene_context_dist_algo = getConfProperty "strugene_context_dist_algo" confInfo
    let simList = case strugene_context_dist_algo of
                    "Euclidean" -> [sqrt (sum [les * les, los * los, ros * ros, res * res, ots * ots] / 5.0) | (les,los,ros,res,ots) <- origSimList]
                    "Manhattan" -> [(sum [les, los, ros, res, ots] / 5.0) | (les,los,ros,res,ots) <- origSimList]
    let contextOfSGPair2SimList = zip contextOfSGPairList simList               -- [((ContextOfSG, ContextOfSG), SimDeg)]

    let contextOfSGIdxPair2SimList = filter (\((id1, id2), _) -> id1 <= id2) $ zip contextOfSGIdxPairList simList
                             -- [((SIdx, SIdx), SimDeg)], upper triangular matrix.

    let store_csg_sim = getConfProperty "store_csg_sim" confInfo
    let csg_sim_tbl = getConfProperty "csg_sim_tbl" confInfo
    case store_csg_sim of
                    "True" -> do
                        conn <- getConn
                        let sqlstat = DS.fromString $ "CREATE TABLE IF NOT EXISTS " ++ csg_sim_tbl ++ " (id INT UNSIGNED PRIMARY KEY AUTO_INCREMENT, contextofsg1idx INT UNSIGNED, contextofsg2idx INT UNSIGNED, sim DOUBLE)"
                        stmt <- prepareStmt conn sqlstat
                        executeStmt conn stmt []
                        closeStmt conn stmt

                        let csgSimList = map (\((csg1idx, csg2idx), sim) -> [toMySQLInt32U csg1idx, toMySQLInt32U csg2idx, toMySQLDouble sim]) contextOfSGIdxPair2SimList
                        forM_ csgSimList $ \[csg1idx, csg2idx, sim] -> do
                            let sqlstat = DS.fromString $ "SELECT id FROM " ++ csg_sim_tbl ++ " where contextofsg1idx = ? and contextofsg2idx = ?"
                            stmt <- prepareStmt conn sqlstat
                            (_, is) <- queryStmt conn stmt [csg1idx, csg2idx]
                            rows <- S.toList is                                 -- [[MySQLValue]]
                            closeStmt conn stmt
                            if rows == []
                              then do             -- Not hit
                                let sqlstat = DS.fromString $ "INSERT INTO " ++ csg_sim_tbl ++ " SET contextofsg1idx = ?, contextofsg2idx = ?, sim = ?"
                                stmt <- prepareStmt conn sqlstat
                                ok <- executeStmt conn stmt [csg1idx, csg2idx, sim]
                                putStrLn $ "getOneToAllContextOfSGSim: LastInsertID : " ++ show (getOkLastInsertID ok)
                                closeStmt conn stmt
                              else if length rows == 1
                                     then do      -- Hit
                                       let sqlstat = DS.fromString $ "UPDATE " ++ csg_sim_tbl ++ " SET sim = ? where id = ?"
                                       stmt <- prepareStmt conn sqlstat
                                       ok <- executeStmt conn stmt [sim, (rows!!0)!!0]
                                       putStrLn $ "getOneToAllContextOfSGSim: Update record whose id is " ++ show (fromMySQLInt32U ((rows!!0)!!0))
                                       closeStmt conn stmt
                                     else error $ "getOneToAllContextOfSGSim: More than one time of hitting on (" ++ (show . fromMySQLInt32U) csg1idx ++ ", " ++ (show . fromMySQLInt32U) csg2idx ++ ")"
                        close conn                                   -- Close MySQL connection.
                    "False" -> putStrLn "getOneToAllContextOfSGSim: No database operation."

    return (origSimList, contextOfSGPair2SimList)

{- Get similarity degrees between one ClauTagPrior context and ALL ClauTagPrior contexts.
 - contextOfSG: A ClauTagPrior context
 - context2ClauTagPriorBase: All samples of mapping from ContextOfSG to ClauTagPrior
 - contextOfSG can belong to context2ClauTagPriorBase or NOT.
 -}
getOneToAllContextOfSGSim' :: ContextOfSG -> Context2ClauTagPriorBase -> IO ([(SimDeg, SimDeg, SimDeg, SimDeg, SimDeg)], [((ContextOfSG, ContextOfSG), SimDeg)])
getOneToAllContextOfSGSim' contextOfSG context2ClauTagPriorBase = do
    let contextOfSGList = map fst context2ClauTagPriorBase                      -- [ContextOfSG]
    if (elem contextOfSG contextOfSGList)
      then getOneToAllContextOfSGSim contextOfSG context2ClauTagPriorBase
      else do
        let context2ClauTagPriorBase' = (contextOfSG, []) : context2ClauTagPriorBase     -- Insert contextOfSG at the head position of contextOfSGList
        contextOfSGPairSimTuple <- getOneToAllContextOfSGSim contextOfSG context2ClauTagPriorBase'
        return (tail (fst contextOfSGPairSimTuple), tail (snd contextOfSGPairSimTuple))

{- Given one StruGene context, get the ClauTagPrior of StruGene sample whose context is most similar to that StruGene context.
 - contextOfSG: A ClauTagPrior context.
 - context2ClauTagPriorBase: All samples of mapping from ContextOfSG to ClauTagPrior.
 - ContextOfSGPair2Sim :: ((ContextOfSG, ContextOfSG), simDeg)
 - Algorithms:
 -   If hit a StruGene context, return the context corresponding Context2ClauTagPrior value;
 -   Otherwise, return the first element among all elements with the highest similarity degree in Context2ClauTagPrior list.
 -}
findStruGeneSampleByMaxContextSim :: ContextOfSG -> Context2ClauTagPriorBase -> IO (SIdx, SimDeg, Context2ClauTagPrior)
findStruGeneSampleByMaxContextSim contextOfSG context2ClauTagPriorBase = do
    let contextOfSGList = map fst context2ClauTagPriorBase                  -- [ContextOfSG]
    if elem (contextOfSG) contextOfSGList
      then do                                                               -- Hit the context of a certain sample
        putStrLn "findStruGeneSampleByMaxContextSim: Hit a sample."
        let idx = elemIndex (contextOfSG) contextOfSGList                   -- Find its position
        let idx' = maybe (-1) (+0) idx                                      -- '-1' is impossible.
        return $ (idx' + 1, 1.0, context2ClauTagPriorBase!!idx')            -- Similarity degree is 1.0 when hitting happens.
      else do
        putStrLn $ "findStruGeneSampleByMaxContextSim: Missing contextOfSG: " ++ show contextOfSG
        let context2ClauTagPriorBase' = (contextOfSG, []) : context2ClauTagPriorBase     -- Insert contextOfSG at the head position of contextOfSGList
        contextOfSGPairSimTuple' <- getOneToAllContextOfSGSim contextOfSG context2ClauTagPriorBase'
        let contextOfSGPairSimTuple = (tail (fst contextOfSGPairSimTuple'), tail (snd contextOfSGPairSimTuple'))
        let contextOfSGPair2SimList = snd contextOfSGPairSimTuple           -- [((ContextOfSG, ContextOfSG), SimDeg)]
        let simDegList = map snd contextOfSGPair2SimList                    -- [SimDeg]
        let maxSim = maximum simDegList                                     -- 1.0 is impossible.
        let idx = elemIndex maxSim simDegList                               -- Use the first element with maximum
        let idx' = maybe (-1) (+0) idx
        return $ (idx' + 1, maxSim, context2ClauTagPriorBase!!idx')         -- SIdx values are from 1 to begin.

{- Among set of ContestOfSG values, calculate similarity degrees between one element and all elements,
 - If one element has similarity degree 1.0 only with itself, print '[OK]';
 - Otherwise, print '[Failed]' and list all ContextOfSG pairs with everyone of which has similarity degree 1.0.
 - startIdx: The SIdx value of first sample.
 - offset: Element offset in element list.
 -}
findWhereSim1HappenAmongStruGeneSamples :: SIdx -> Int -> [Context2ClauTagPrior] -> IO ()
findWhereSim1HappenAmongStruGeneSamples startIdx offset context2ClauTagPriorBase = do
    if offset < (length context2ClauTagPriorBase)
      then do
        let contextOfSG = fst (context2ClauTagPriorBase!!offset)                -- ContextOfSG
        contextOfSGPairSimTuple <- getOneToAllContextOfSGSim contextOfSG context2ClauTagPriorBase
        let contextOfSGPair2SimList = snd contextOfSGPairSimTuple               -- [((ContextOfSG, ContextOfSG), SimDeg)]
        let contextOfSGPair2SimList' = zip [0 ..] contextOfSGPair2SimList       -- [(Int, ((ContextOfSG, ContextOfSG), SimDeg))]
        let listOfContextOfSGWithSim1 = filter (\x -> (snd . snd) x == 1.0) contextOfSGPair2SimList'   -- Keep items whose similarity Degrees are 1.0
        if (length listOfContextOfSGWithSim1 == 1)
          then putStrLn $ "  Sample " ++ show (startIdx + offset) ++ " [OK]"     -- Only one pair has similarity degree 1.0
          else do
            putStr $ "  Sample " ++ show (startIdx + offset) ++ " " ++ show ((fst . fst . snd) (listOfContextOfSGWithSim1!!0))
            putStrLn $ " has similarity degree 1.0 with following samples:"     -- More than one pairs have similarity degree 1.0
            printContextOfSGPair2SimList startIdx listOfContextOfSGWithSim1
        findWhereSim1HappenAmongStruGeneSamples startIdx (offset + 1) context2ClauTagPriorBase
      else putStrLn ""

{- Print a list of [(Offset, ((ContextOfSG, ContextOfSG), SimDeg))], where Offset is index of this list.
 - startIdx: The SIdx value of first sample.
 -}
printContextOfSGPair2SimList :: SIdx -> [(Int, ((ContextOfSG, ContextOfSG), SimDeg))] -> IO ()
printContextOfSGPair2SimList _ [] = putStr ""
printContextOfSGPair2SimList startIdx (e:es) = do
    putStrLn $ "    Sample " ++ show (startIdx + fst e) ++ " " ++ show ((snd. fst . snd) e)
    printContextOfSGPair2SimList startIdx es

-- Read static similarity degrees between categories.
readStaticTypePair2Sim :: IO [((Category, Category), SimDeg)]
readStaticTypePair2Sim = do
    confInfo <- readFile "Configuration"
    let type_sim_tbl = getConfProperty "type_sim_tbl" confInfo
    conn <- getConn
    let sqlStr = "SELECT * FROM " ++ type_sim_tbl
    (_, is) <- query_ conn (DS.fromString sqlStr)
    rows <- S.toList is
    let typePair2Sim = map (\x -> (((getCateFromString . fromMySQLText) (x!!1), (getCateFromString . fromMySQLText) (x!!2)),  fromMySQLDouble (x!!3))) rows
    return typePair2Sim

-- Read similarity degrees between grammatic rules.
readStaticTagPair2Sim :: IO [((Tag, Tag), SimDeg)]
readStaticTagPair2Sim = do
    confInfo <- readFile "Configuration"
    let tag_sim_tbl = getConfProperty "tag_sim_tbl" confInfo
    conn <- getConn
    let sqlStr = "SELECT * FROM " ++ tag_sim_tbl
    (_, is) <- query_ conn (DS.fromString sqlStr)
    rows <- S.toList is
    let tagPair2Sim = map (\x -> ((fromMySQLText (x!!1), fromMySQLText (x!!2)),  fromMySQLDouble (x!!3))) rows
    return tagPair2Sim

-- Read similarity degrees between phrasal structures.
readStaticStruPair2Sim :: IO [((PhraStru, PhraStru), SimDeg)]
readStaticStruPair2Sim = do
    confInfo <- readFile "Configuration"
    let stru_sim_tbl = getConfProperty "stru_sim_tbl" confInfo
    conn <- getConn
    let sqlStr = "SELECT * FROM " ++ stru_sim_tbl
    (_, is) <- query_ conn (DS.fromString sqlStr)
    rows <- S.toList is
    let struPair2Sim = map (\x -> ((fromMySQLText (x!!1), fromMySQLText (x!!2)),  fromMySQLDouble (x!!3))) rows
    return struPair2Sim

-- Read similarity degrees between phrasal spans.
readStaticSpanPair2Sim :: IO [((Span, Span), SimDeg)]
readStaticSpanPair2Sim = do
    confInfo <- readFile "Configuration"
    let span_sim_tbl = getConfProperty "span_sim_tbl" confInfo
    conn <- getConn
    let sqlStr = "SELECT * FROM " ++ span_sim_tbl
    (_, is) <- query_ conn (DS.fromString sqlStr)
    rows <- S.toList is
    let spanPair2Sim = map (\x -> ((fromMySQLInt8 (x!!1), fromMySQLInt8 (x!!2)),  fromMySQLDouble (x!!3))) rows
    return spanPair2Sim

-- Read similarity degrees between PhraSyn values.
readStaticPhraSynPair2Sim :: IO [((PhraSyn, PhraSyn), SimDeg)]
readStaticPhraSynPair2Sim = do
    confInfo <- readFile "Configuration"
    let phrasyn_sim_tbl = getConfProperty "phrasyn_sim_tbl" confInfo
    conn <- getConn
    let sqlStr = "SELECT * FROM " ++ phrasyn_sim_tbl
    (_, is) <- query_ conn (DS.fromString sqlStr)
    rows <- S.toList is
    let phrasynPair2Sim = map (\x -> (((readPhraSynFromStr . fromMySQLText) (x!!1)
                                     , (readPhraSynFromStr . fromMySQLText) (x!!2))
                                     ,  fromMySQLDouble (x!!3))
                              ) rows
    return phrasynPair2Sim
